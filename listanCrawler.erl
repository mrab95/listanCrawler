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
-record(url_info, {url, nr, isScannable, isLocal}).


% ____________________ TODO & comments ________________________

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
% __________________________________________________


%Main
%I'm just here for convenience
main() ->
	URLs_info = search_site_for_url("http://sweclockers.com"),
	URLs_info.
	%io:fwrite(Result).


%% !! PROBLEM WE SAVE ALL URLS IN ONE LONG STRING ATM... !!
%% can return as records/tuples, or maybe there's another way


% ---------------------------
% search_site
% String -> [String] -> [String]
% ----
% Search string, return hits 
% ---------------------------

search_site(Url, Keywords) ->
	Body = get_site_body(Url),
	
	URLs_info = find_keywords_in_list(Body, Keywords),
	CleanURLs_info = clean_search_result(CleanURLs_info),
	URLs_info.


% ---------------------------
% search_site_for_url
% String -> [String] -> [String]
% ----
% Find all URLs on website, unique sites found
% 	and how often they occured
% ---------------------------
search_site_for_url(Url) ->
	URLprefix = ["http://", "https://" , "www.", "/"],
	search_site(Url, URLprefix).


% ---------------------------
% clean url, count


%how does sort() work? Is there a more efficient way
clean_search_result([#url_info{url = Url, nr = Nr, isScannable = IsScannable, isLocal = IsLocal}]) ->
	SortedURLs = lists:sort(URLs),
	ValidURLs = remove_invalid_urls(SortedURLs),
	URLFixedLocal = fix_local_urls(ValidURLs, "http://sweclockers.com"),
	URLsCounted = count_each_url(URLFixedLocal),

	SortedURLs.

	

%--------
remove_invalid_urls([]) ->
	[];
remove_invalid_urls([URL|URLs]) ->
	URLtooShort = (string:len(URL) < 6),
	LastCharValid = string:equal([lists:last(URL)], "."),
	[FirstChar | _] = URL,
	FirstCharValid = string:equal([FirstChar], "."),
	URLblacklisted = is_url_blacklisted(URL),

	URLvalid = ((FirstCharValid and LastCharValid) and ((not URLtooShort) and (not URLblacklisted))),
	
	if (URLvalid)  ->
		URL ++ remove_invalid_urls(URLs);
	(not URLvalid) ->
		remove_invalid_urls(URLs)
	end.

%---------
is_url_blacklisted(URL) ->
	%% maybe read from file
	Blacklist = [".css",".js", "//Add", "//Don't", "adserver.adtech.de"],
	is_url_blacklisted(URL, Blacklist).
	
is_url_blacklisted(URL, Blacklist) ->
	(find_keywords_in_list(URL, Blacklist) /= []).

%--------


fix_local_urls([], _) ->
	[];

%!! remove '/' if domain ends with it !!
fix_local_urls([URL | URLs], Domain) ->
	[Head | _] = URL,
	IsLocal = string:equal([Head], "/"),

	if (IsLocal)  ->
		[Domain++URL] ++ fix_local_urls(URLs, Domain);
	(not IsLocal) ->
		fix_local_urls(URLs, Domain)
	end.




%--------
count_each_url(URLs) ->
	count_each_url(URLs, 1).

count_each_url([], _) ->
	[];

count_each_url([Url1 | []], Counter) ->
	[{Url1, Counter}];


count_each_url([URL1 | URLs], Counter) ->
	[URL2 | _ ] = URLs,

	FirstEqual = string:equal(URL1, URL2),

	if(FirstEqual) ->
		  count_each_url( (URLs) , (Counter+1));
	(not FirstEqual) ->
		[{URL1, Counter}] ++ count_each_url( (URLs) , 1)
	end.







% ---------------------------
% find_keywords_in_list
% String -> String -> String -> [String]
% ----
% Find every keyword in string
% Case insensitive, keyword can be longer than string
% If a character matches multiple keywords it will only return once
% 	this may result in missed results.
% ---------------------------

find_keywords_in_list(Text, Keywords) ->
	find_keywords_in_list(Text, "", false, Keywords).

find_keywords_in_list([], _, _, _) ->
	#url_info{url = "", nr="", isScannable="", isLocal=""};

find_keywords_in_list(Text, CurrentURL, CurrentIsURL, Keywords) ->	
	[NextChar | TextNew] = Text,
	NextIsIllegal = unallowed_char(NextChar),
	TextIsEmpty = string:equal(TextNew, ""),

	%look for start of valid URL
	if (not CurrentIsURL) ->

		%remove illegal char
	   	if(NextIsIllegal) ->
			if(TextIsEmpty) ->
				#url_info{url = CurrentURL, nr="", isScannable="", isLocal=""};
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
				#url_info{url = CurrentURL, nr="", isScannable="", isLocal=""};
			(not TextIsEmpty) ->
			[#url_info{url = (CurrentURL++[NextChar]), nr="", isScannable="", isLocal=""}]
				  ++ find_keywords_in_list(TextNew, "", false, Keywords)
			end;
	
		%Continuing adding to URL
		(not NextIsIllegal) ->
		if(TextIsEmpty) ->
				#url_info{url = (CurrentURL++[NextChar]), nr="", isScannable="", isLocal=""};
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
% String -> String 
% ----
% Send HTTP GET request to server, return body
% ---------------------------
get_site_body(Url) ->
	application:start(inets),
	get_site_body_helper(Url, 10).


% ---------------------------
% get_site_body_helper
% String -> Integer -> Body
% ----
% Helper function
% Retry if GET request fails, until success or no tries left
% ---------------------------

get_site_body_helper(Url, TriesMax) ->
	io:fwrite("\nDownloading HTML-page: ~s \n", [Url]),
	get_site_body_helper(Url, TriesMax, TriesMax).

get_site_body_helper(_, 0, _) ->
	erlang:error(http_timeout);

get_site_body_helper(Url,TriesLeft, TriesMax) ->
	WaitAfterFail = 800, %ms
	FirstRun = (TriesLeft == TriesMax),
	GetReq = httpc:request(Url),
	{Response, _ } = GetReq,
	
	if (Response == ok) ->
		{_,{_,_, Body}} = GetReq,
		io:fwrite("\nProcessing HTML-page: ~s \n\n", [Url]),
		Body;

   	(Response /= ok) ->
		if(FirstRun) ->
			io:fwrite("\nProblem loading page: ~s \nRetrying.. \n", [Url]),
			timer:sleep(WaitAfterFail),
			get_site_body_helper(Url,TriesLeft-1, TriesMax);
		(not FirstRun) ->
			timer:sleep(WaitAfterFail),
			get_site_body_helper(Url,TriesLeft-1, TriesMax)
		end	
	end.

	
%FOR TLS
%application:start(crypto),
%application:start(public_key),
%application:start(ssl),
%application:start(inets)
	%io:fwrite(Result).

