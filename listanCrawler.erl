% listanCrawler   Copyright © 2016   Eliot Roxbergh 
%
% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License version 3 as published by
% the Free Software Foundation.
% 
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU General Public License for more details.


-module      (listanCrawler).
-export      ([main/0, search_site/1]).

-include_lib ("kernel/include/inet.hrl").


% Note url on same domain can be just /html/file.html
% Similar //pic/picture.png
%
% So we look for / as prefix
% This should be tagged as local in some way, and domain should be added
% For example /pic/picture.png should be http://myDomain.com/pic/picture.png

% url_cleaner(URLs) ->
% a url must have atleast one dot (.)
% cannot begin or end with dot
% might even look for known TLDs
% probly strip prefix (http://, www.)
% sort (which algoritm)

% url_in_blacklist(URL) ->
% load me from file please
% blacklist some such as
% jquery.min.js
% jquery.js
% /title
% /css
% .css
% .js
%

% url_in_whitelist(URL) ->
% load me from file please
% if in whitelist mode, only accept these
% .html .png .txt ..

% url_fileEnding(URL) ->
% if missing maybe assume html


% is_url_scannable(URL) ->
% is page scannable 
% file ending .html .htm .txt ...

% url_counter(URLs) ->
% return [{URL,Occurances}]


% url_search(Keywords) ->
% eg ["youtube", ".com", ".de"]
% or [".png", ".jpg"]


% search_whole_domain(URL) ->
% recursive search all URLs found that leads to same domain
% (can easy be implemented to allow searching recursively for any keyword, eg. visit all links containing "cats" in url)
%
% basic idea, probly bad.
% for each: URL = url_search(URL)), search_site(URL) ++ search_whole_domain(URL)
%
% This would be a good time for concurrency/workers,
% maybe send messages about URLs already searched



main() ->
	URLs = search_site("http://sweclockers.com/"),
	URLs.

% Find all URLs on website
search_site(Url) ->
	Body = get_site_body(Url),
	
	UrlList =  search_text_for_url(Body, "", false),
	UrlList.



%Search text for URLs and return those 
search_text_for_url([], _, _) ->
	   [];

search_text_for_url(Text, CurrentURL, CurrentIsURL) ->	
	
	[NextChar | TextNew] = Text,
	NextIsIllegal = unallowed_char(NextChar),
	TextIsEmpty = string:equal(TextNew, ""),

	%look for start of valid URL
	if (not CurrentIsURL) ->
			
		%remove illegal char
	   	if(NextIsIllegal) ->
			if(TextIsEmpty) ->
				CurrentURL;
			(not TextIsEmpty) ->
				search_text_for_url(TextNew, "", false)
			end;

		%look for valid URL prefix
		true ->	
		  	{Prefix, TextNoPrefix} = find_remove_url_prefix(Text),
			IsUrl = not string:equal(Prefix, ""),
	 		
		       	% URL found!
			% Seperated prefix from text
			if (IsUrl) ->
				search_text_for_url(TextNoPrefix, Prefix, true);
				
			% Did NOT start with valid prefix
			% First char removed from text
			(not IsUrl) ->
				search_text_for_url(TextNoPrefix, "", false)
			end
	
	
	%We are in the middle of a URL
	%continue adding to url until illegal char or EOF
	true ->
		%URL finished processing
		if(NextIsIllegal) ->
			if(TextIsEmpty) ->
				[{CurrentURL, 1}];
			(not TextIsEmpty) ->
				 [{CurrentURL, 1}] ++ search_text_for_url(TextNew, "", false)
			end;
	
		%Continuing adding to URL
		(not NextIsIllegal) ->
		if(TextIsEmpty) ->
				 [{(CurrentURL ++ [NextChar]), 1}]; 
				(not TextIsEmpty) ->
					  TmpURL = CurrentURL ++ [NextChar],
					  search_text_for_url(TextNew, TmpURL, CurrentIsURL)
			end
		end
	end.


%If Text starts with an url prefix (eg www. http://)
%Then remove that prefix otherwise only remove first char

% regex could be nice
% gör detta på andra ställen med
%find_remove_url_prefix([Head|Text], CurrentPrefix) ->
%find_remove_url_prefix([], CurrentPrefix) ->
%
% ta bort textlen
%
find_remove_url_prefix(Text) ->
	ValidPrefix = ["http://", "https://" , "www.", "/"],
	TextLen = string:len(Text),
	
	%Valid if text starts with prefix
	TextMatchingPrefix = starts_with_these(Text, ValidPrefix),	
	PrefixIsValid = not string:equal(TextMatchingPrefix, ""),
	
	%Remove valid prefix from text, return seperated
	if (PrefixIsValid) ->
		PrefixLen = string:len(TextMatchingPrefix),
		ElemToRemove = min(PrefixLen+1,TextLen+1),
		TextNew = string:substr(Text, ElemToRemove),
		{TextMatchingPrefix, TextNew};

   	%No prefix found remove first character
	(not PrefixIsValid) ->
		ElemToRemove = min(TextLen+1, 2),
		TextNew = string:substr(Text, ElemToRemove),
		{"", TextNew}
	end.


%Returns substring if another string begins with it
starts_with_these(String, [SubString | SubStrings]) ->

	FoundMatch = starts_with_this(String, SubString),
	
	%There are multple substrings, search recursively before giving up
	if (SubStrings /= []) ->
		if (FoundMatch) ->
			SubString;
		(not FoundMatch) ->
			starts_with_these(String, SubStrings)
		end;
	
	
	true ->
		if (FoundMatch) ->
			SubString;
		   
		% Not found return ""
		(not FoundMatch) ->
			""
		end
	end.


% Does string start with substring
% Matches on available characters if string is smaller
% Not case sensitive
starts_with_this(String, SubString) ->
	CharsLeft = min(string:len(String),string:len(SubString)),
	
	% Make same length, make lowercase
	StringShort = string:to_lower( string:substr(String, 1, string:len(SubString)) ),
	SubStringShort = string:substr(SubString, 1, string:len(String)),

	% Bad argument (empty list) 
	if (CharsLeft < 1) ->
		false;

	% Last char, all chars before were equal, compare and return
  	(CharsLeft == 1) ->
		string:equal([StringShort], [SubStringShort]);
	
	% Compare char is equal, check next one if so
	 (CharsLeft > 1) ->
	   	[A|AS] = StringShort,
		[B|BS] = SubStringShort,
		CharsEqual = string:equal([A],[B]),

		if(CharsEqual) ->
		 	starts_with_this(AS,BS);
		true ->
			 false
		end
	end.




% Is char allowed in URL
% ! Needs additional work and whitelisted chars !
unallowed_char(Char) ->
	AllowedChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~:/?#[]@!$&'()*+,;=åäöÅÄÖ",
	IllegalChar = not lists:member(Char, AllowedChars),

	IllegalChar.


%Send HTTP GET request to server, return body
get_site_body(Url) ->
	application:start(inets),
	get_site_body_helper(Url, 10).


% Retry if GET request fails, until success or no tries left
get_site_body_helper(Url, TriesLeft) ->
	WaitAfterFail = 2000, %ms
	GetReq = httpc:request(Url),
	{Response, _ } = GetReq,
	
	if (Response == ok) ->
		{_,{_,_, Body}} = GetReq,
		Body;

   	(Response /= ok) ->
		io:fwrite("Cannot connect, retrying.. tries left: ~p \n", [TriesLeft-1]),
		if(TriesLeft == 1) ->
			erlang:error(timeout);	
		true ->
			timer:sleep(WaitAfterFail),
			get_site_body_helper(Url,TriesLeft-1)
		end
	end.

	
%FOR TLS
%application:start(crypto),
%application:start(public_key),
%application:start(ssl),
%application:start(inets)
