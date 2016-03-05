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
-record(url_info, {prefix, url, suffix, urlSearched, nr, isScannable, isLocal}).


% ____________________ TODO & comments ________________________

% url_in_whitelist(URL) ?
% load me from file please
% if in whitelist mode, only accept these
% .html .png .txt ..

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


% ---------------------------
% search_site
% String -> [String] -> [String]
% ----
% Search string, return hits 
% ---------------------------

search_site(URLsearched, Keywords) ->
	LastChar = lists:last(URLsearched),

	% delete ending '/' from URL
	if (LastChar == '/') ->
	   search_site( (lists:droplast(URLsearched)) , Keywords );
	
	true ->
	%	SiteBody = get_site_body(URLsearched),
		SiteBody = " http://tesTeTet.se www.apa.se www.apa.se http:apa.se /index.html",	
		URLs_info1 = text_to_url_records(SiteBody, Keywords),
		
		%
		

		
		%

		%URLs_info2 = clean_search_result(URLs_info1, URLsearched),
		URLs_info1
	end.


% ---------------------------
% search_site_for_url
% String -> [String] -> [String]
% ----
% Find all URLs on website, unique sites found
% 	and how often they occured
% ---------------------------
search_site_for_url(URL) ->
	URLprefix = ["http://", "https://" , "www.", "/"],
	search_site(URL, URLprefix).


% ---------------------------
% clean url, count

%!! use keyreplace to change values in record !!

%how does sort() work? Is there a more efficient way
%clean_search_result(URLs = [#url_info{}], URLsearched) ->
clean_search_result(URLs, URLsearched) ->
	URLsClean1 = remove_invalid_urls(URLs),
	URLsClean2 = fix_local_urls(URLsClean1, URLsearched),
	

	Sort_url_info = fun(R0, R1) ->
		{R0#url_info.nr ,R0#url_info.url} < {R1#url_info.nr, R1#url_info.url} end,	

	URLsClean3 = lists:sort(Sort_url_info, URLsClean2),
	URLsClean4 = count_each_url(URLsClean3),

	%I break, but why?
	%URLsClean5 = lists:sort(Sort_url_info2, URLsClean4),
	
	URLsClean4.

	

%--------
remove_invalid_urls([]) ->
	[];

remove_invalid_urls([URL_info | URLs_info]) ->
	URL = URL_info#url_info.url,
	URLvalid = is_url_valid(URL),	
	if (URLvalid)  ->
		[URL_info] ++ remove_invalid_urls(URLs_info);
	(not URLvalid) ->
		remove_invalid_urls(URLs_info)
	end;

remove_invalid_urls(URL_info) ->
	URL = URL_info#url_info.url,
	URLvalid = is_url_valid(URL),
	
	if (URLvalid)  ->
		[URL_info];
	(not URLvalid) ->
		[]
	end.

%------

%Note argument is url without prefix

is_url_valid([]) ->
	false;

is_url_valid(undefined) ->
	false;

is_url_valid(URL) ->
	%NOTE urls with this ending will be REMOVED
	% !! RATHER REMOVE THESE CHARS AND RECHECK IF VALID AGAIN !!
	IllegalChars = " ;\":,>\\|",
	
	URLtooShort = (length(URL) < 4),
	LastCharValid = not lists:member(lists:last(URL), IllegalChars),
	URLblacklisted = is_url_blacklisted(URL),
	
	(LastCharValid and ((not URLtooShort) and (not URLblacklisted))).
	

%---------
%
%!! I just search start of text right...?  !!
is_url_blacklisted(URL) ->
	%% maybe read from file
	Blacklist = [".css",".js", ".ico", "script>", "javascript\"", "//m." , "rss+xml" , "//Add", "//Don't", "adserver.adtech.de"],
	is_url_blacklisted(URL, Blacklist).
	
is_url_blacklisted(URL, Blacklist) ->
	{_, Prefix} = remove_prefix(URL, Blacklist),
	{_, Suffix} = remove_suffix(URL, Blacklist),
	
	if((Suffix == []) and (Prefix == [])) ->
		false;
	true ->
		true
	end. 

%--------


fix_local_urls([], _) ->
	[];

fix_local_urls([URLinfo | URLs], URLsearched) ->
	URL = URLinfo#url_info.url,
	[H | _] = URL,
	IsLocal = string:equal([H], "/"), %(H == '/'),
	if (IsLocal)  ->
		[URLinfo#url_info{url = (URLsearched ++ URL), isLocal = true}] ++ fix_local_urls(URLs, URLsearched);
	(not IsLocal) ->
		[URLinfo#url_info{isLocal = false}] ++ fix_local_urls(URLs, URLsearched)
	end.



%--------
%AND REMOVES DUPLICATES
count_each_url(URLs) ->
	count_each_url(URLs, 1).

count_each_url([], _) ->
	[];

count_each_url([URLinfo | []], Counter) ->
	URLinfo#url_info{nr = Counter};

count_each_url([URLinfo0 | URLs], Counter) ->
	
	[URLinfo1 | _ ] = URLs,
	URL0 = URLinfo0#url_info.url,
	URL1 = URLinfo1#url_info.url,

	FirstEqual = string:equal(URL0, URL1),

	if(FirstEqual) ->
		  count_each_url( (URLs) , (Counter+1));
	(not FirstEqual) ->
		[URLinfo0#url_info{nr = Counter}] ++ count_each_url(URLs, 1)
	end.

%-----------------------

%DO I WORK?
isScannable(FileType) ->
	Scannables = [".txt", ".html", ".htm"],
	lists:dropwhile( (strings:equal(FileType, lists:first(Scannables))), Scannables).

%-----------------------
seperate_url_file_ending(URLinfo) ->
	FileTypes = [".txt", ".html", ".png"],
	URL = URLinfo#url_info.url, Prefix = URLinfo#url_info.prefix,
	Local = URLinfo#url_info.isLocal, Searched = URLinfo#url_info.urlSearched,

	{NewURL, Suffix} = remove_suffix(URL, FileTypes),
	Scannable = isScannable(Suffix),
	#url_info{prefix = Prefix, url = NewURL, isLocal = Local, urlSearched = Searched, isScannable = Scannable, suffix = Suffix}.









%----------------------


% ---------------------------
% text_to_url_records
% String -> String -> String -> [String]
% ----
% Find every keyword in string
% Case insensitive, keyword can be longer than string
% If a character matches multiple keywords it will only return once
% 	this may result in missed results.
% ---------------------------

% !! misleading name, actually only looks if STRING STARTS WITH keyword !!

% how to use this to find if it ENDS with keyword?

% !! returns a undefined url_info record too much

text_to_url_records(Text, Keywords) ->
	text_to_url_records(Text, [], [], Keywords).

text_to_url_records([], _, _, _) ->
	#url_info{};

text_to_url_records(Text, CurrentURL, Prefix, Keywords) ->	
	[NextChar | TextNew] = Text,
	NextIsIllegal = unallowed_char(NextChar),
	TextIsEmpty = (TextNew == []),
	CurrentIsURL = (Prefix /= []),
	
	%look for start of valid URL
	if (not CurrentIsURL) ->

		%remove illegal char
	   	if(NextIsIllegal) ->
			if(TextIsEmpty) ->
				#url_info{url = CurrentURL, prefix = Prefix};
			(not TextIsEmpty) ->
				text_to_url_records(TextNew, [], [], Keywords)
			end;

		%look for valid URL prefix
		true ->	
		  	{TextNoPrefix, PrefixNew} = remove_prefix(Text, Keywords),
			IsUrl = (PrefixNew /= []),
		       	% URL found!
			% Seperated prefix from text
			if (IsUrl) ->
				text_to_url_records(TextNoPrefix, [], PrefixNew, Keywords);
				
			% Did NOT start with valid prefix
			% First char removed from text
			(not IsUrl) ->
				text_to_url_records(TextNoPrefix, [], [], Keywords)
			end
		end;
	
	%We are in the middle of a URL
	%continue adding to url until illegal char or EOF
	true ->
		%URL finished processing
		if(NextIsIllegal) ->
			if(TextIsEmpty) ->
				#url_info{url = CurrentURL};
			(not TextIsEmpty) ->
				[#url_info{url = (CurrentURL++[NextChar]), prefix = Prefix}]
				  ++ text_to_url_records(TextNew, [], [], Keywords)
			end;
	
		%Continuing adding to URL
		(not NextIsIllegal) ->
			if(TextIsEmpty) ->
				#url_info{url = (CurrentURL++[NextChar]), prefix = Prefix};
				(not TextIsEmpty) ->
					  TmpURL = CurrentURL ++ [NextChar],
					  text_to_url_records(TextNew, TmpURL, Prefix, Keywords)
			end
		end
	end.

%---------------

remove_prefix(List, []) ->
	{List, []};

remove_prefix(List, [Prefix | Prefixes]) ->
	Match = lists:prefix(Prefix, List),

	if(Match) ->
		ListNoPrefix = string:substr(List, (length(Prefix)+1), length(List)),
		{ListNoPrefix, Prefix};
	(not Match) ->
		remove_prefix(List, Prefixes)
	end.

%remove_prefix(List, Prefix) ->
%	remove_prefix(List, [Prefix]).

%-------------

remove_suffix(List, []) ->
	{List, []};

remove_suffix(List, [Suffix | Suffixes]) ->
	Match = lists:suffix(Suffix, List),

	if(Match) ->
		ListNoSuffix = string:substr(List, 1, length(List)-length(Suffix)),
		{ListNoSuffix, Suffix};
	(not Match) ->
		remove_suffix(List, Suffixes)
	end.

%remove_suffix(List, Suffix) ->
%	remove_suffix(List, [Suffix]).

%-------------


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

