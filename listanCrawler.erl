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
-export      ([main/0, search_site/2]).

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


% ---------------------------
% function
% a -> a 
% ----
% text
% ---------------------------


main() ->
	URLs = search_site_for_url("http://sweclockers.cosm/"),
	URLs.



% ---------------------------
% search_site
% String -> [String] -> [{String, Integer}]
% ----
% Search string, return hits and number of each hit found
% ---------------------------

search_site(Url, Keywords) ->
	Body = get_site_body(Url),
	
	UrlList =  find_keywords_in_list(Body, Keywords),
	UrlList.


% ---------------------------
% search_site_for_url
% String -> [String] -> [{String, Integer}]
% ----
% Find all URLs on website, unique sites found
% 	and how often they occured
% ---------------------------
search_site_for_url(Url) ->
	URLprefix = ["http://", "https://" , "www.", "/"],
	search_site(Url, URLprefix).



% ---------------------------
% find_keywords_in_list
% String -> String -> String -> [{String, Integer}]
% ----
% Find every keyword in string
% Case insensitive, keyword can be longer than string
% If a character matches multiple keywords it will only return once
% 	this may result in missed results.
% ---------------------------

find_keywords_in_list(Text, Keywords) ->
	find_keywords_in_list(Text, "", false, Keywords).

find_keywords_in_list([], _, _, _) ->
	   [];

find_keywords_in_list(Text, CurrentURL, CurrentIsURL, Keywords) ->	
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
				find_keywords_in_list(TextNew, "", false, Keywords)
			end;

		%look for valid URL prefix
		true ->	
		  	{Prefix, TextNoPrefix} = remove_prefixes(Text, Keywords),
			IsUrl = not string:equal(Prefix, ""),
	 		
		       	% URL found!
			% Seperated prefix from text
			if (IsUrl) ->
				find_keywords_in_list(TextNoPrefix, Prefix, true, Keywords);
				
			% Did NOT start with valid prefix
			% First char removed from text
			(not IsUrl) ->
				find_keywords_in_list(TextNoPrefix, "", false, Keywords)
			end
		end;
	
	%We are in the middle of a URL
	%continue adding to url until illegal char or EOF
	true ->
		%URL finished processing
		if(NextIsIllegal) ->
			if(TextIsEmpty) ->
				[{CurrentURL, 1}];
			(not TextIsEmpty) ->
				 [{CurrentURL, 1}] ++ find_keywords_in_list(TextNew, "", false, Keywords)
			end;
	
		%Continuing adding to URL
		(not NextIsIllegal) ->
		if(TextIsEmpty) ->
				 [{(CurrentURL ++ [NextChar]), 1}]; 
				(not TextIsEmpty) ->
					  TmpURL = CurrentURL ++ [NextChar],
					  find_keywords_in_list(TextNew, TmpURL, CurrentIsURL, Keywords)
			end
		end
	end.

% ---------------------------
% remove_prefixes
% [String] -> [[String]] -> {[String], [String]} 
% ----
% ONLY WORKS FOR STRINGS RIGHT NOW
% 
% Look if a list starts with any of specified lists
% If so, remove the first occurance and return seperated in a tuple.
% Otherwise only remove first char and return {List, []}.
% Not case sensitive, can return true even if search query is longer than the list
% ---------------------------

remove_prefixes([_|List], []) ->
	{[], List};
remove_prefixes([], _) ->
	{[], []};

remove_prefixes(List, [Prefix|Prefixes]) ->
	Result = remove_prefix(List,Prefix),

	case Result of
		% Not found, try next prefix
		{[], _} -> remove_prefixes(List, Prefixes);
		
		% Prefix found
		_ -> Result
	end.


% ---------------------------
% remove_prefix
% [String] -> {[String], [String]} 
% ----
% ONLY WORKS FOR STRINGS RIGHT NOW
%
% Look if a list starts with another list.
% If so, remove and return seperated in a tuple.
% Otherwise remove first char instead and return togheter with empty list.
% Not case sensitive, can return true even if search query is longer than the list.
% ---------------------------
remove_prefix(List, Result) ->
	remove_prefix(List, Result, "").
remove_prefix([], _, Result) ->
	{Result, ""};
remove_prefix(List, [], Result) ->
	{Result,List};
remove_prefix([HL|List], [HP|ValidPrefix], Result) ->
	%not case sensitive
	HLlower = string:to_lower(HL),
	IsMatch = string:equal(HLlower,HP),
	
	if (IsMatch) ->
		remove_prefix(List, ValidPrefix, Result ++ [HL]);
	   (not IsMatch) ->
		   {[], List}
	end.



% ---------------------------
% unallowed_char
% Char -> Boolean
% ----
% Is character allowed in a url
% ---------------------------

% ! Needs additional work and whitelisted chars !
unallowed_char(Char) ->
	AllowedChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~:/?#[]@!$&'()*+,;=åäöÅÄÖ",
	IllegalChar = not lists:member(Char, AllowedChars),
	IllegalChar.




% ---------------------------
% get_site_body
% String 
% ----
% Send HTTP GET request to server, return body
% ---------------------------
get_site_body(Url) ->
	application:start(inets),
	get_site_body_helper(Url, 10).


% ---------------------------
% get_site_body_helper
% String -> Integer
% ----
% Helper function
% Retry if GET request fails, until success or no tries left
% ---------------------------
get_site_body_helper(_, 0) ->
	erlang:error(timeout);

get_site_body_helper(Url,TriesLeft) ->
	WaitAfterFail = 1500, %ms
	GetReq = httpc:request(Url),
	{Response, _ } = GetReq,
	
	if (Response == ok) ->
		{_,{_,_, Body}} = GetReq,
		Body;

   	(Response /= ok) ->
		io:fwrite("Error: cannot connect\nRetrying.. tries left: ~p \n\n", [TriesLeft-1]),
		timer:sleep(WaitAfterFail),
		get_site_body_helper(Url,TriesLeft-1)
	end.

	
%FOR TLS
%application:start(crypto),
%application:start(public_key),
%application:start(ssl),
%application:start(inets)
