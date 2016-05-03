% listanCrawler   Copyright © 2016   Eliot Roxbergh 
% 
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


-module      (listanCrawler).
-export      ([main/0, search_site/2]).

-include_lib ("kernel/include/inet.hrl").
-record(url_info, {prefix, mainURL, suffix, urlSearched, occurrences, isSearchable, isLocal}).

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
%
% tail-recursion
% __________________________________________________

% !!! Issues !!!
% as we try to match '/' as valid start of url
% we get a lot of false positives
% need more checking for this
%
% sort is broken
%
% write_to_file is very basic
% allow user to set file name or set automatically depending on date+url.
% Seems to write quotation marks around everything
%
% sometimes cuts a url to early since rough url finder and allowed chars
% something to try would be to directly take everything marked as link according to html-syntax (this will also miss some).
% furthermore one COULD try all links recieved and remove those returning 404-error or such, this is probably a bad idea
% ------------------------------







%%
%
% !! MAKE SURE TO CREATE THE FILE, not just append. write_to_file function!!!
%
%%





main() ->
	URLsInfo = search_site_for_url("http://sweclockers.com"),
	save_result(URLsInfo, true).


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
		SiteBody = get_site_body(URLsearched),
	%	SiteBody = "http://tesTeTet.se www.apa.se www.apa.se http:apa.se/a.html www.index.html www.ala.se. www.als.js",	
		URLsInfo1 = text_to_url_records(SiteBody, Keywords),
		

		URLsInfo2 = clean_search_result(URLsInfo1, URLsearched),
		URLsInfo2
	end.


% ---------------------------
% search_site_for_url
% String -> [String] -> [String]
% ----
% Find all URLs on website, unique sites found
% 	and how often they occured
% ---------------------------
search_site_for_url(URL) ->
	URLprefix = ["http://", "https://" , "www.", "//", "/"],
	search_site(URL, URLprefix).


% ---------------------------
% clean url, count

%!! use keyreplace to change values in record !!

%how does sort() work? Is there a more efficient way
%clean_search_result(URLs = [#url_info{}], URLsearched) ->
clean_search_result(URLs, URLsearched) ->
	URLsClean1 = remove_invalid_urls(URLs),
	URLsClean2 = set_urlSearched(URLsClean1, URLsearched),
	URLsClean3 = fix_local_urls(URLsClean2),
	

	Sort_url_info = fun(R0, R1) ->
		{R0#url_info.occurrences ,R0#url_info.mainURL} < {R1#url_info.occurrences, R1#url_info.mainURL} end,	

	URLsClean4 = lists:sort(Sort_url_info, URLsClean3),
	URLsClean5 = count_each_url(URLsClean4),
	URLsClean6 = strip_suffix(URLsClean5),
	%I break, but why?
	%URLsClean5 = lists:sort(Sort_url_info2, URLsClean4),
	
	URLsClean6.

	

%--------
remove_invalid_urls([]) ->
	[];

remove_invalid_urls([URLInfo | URLsInfo]) ->
	URL = URLInfo#url_info.mainURL,
	URLvalid = is_url_valid(URL),	
	if (URLvalid)  ->
		[URLInfo] ++ remove_invalid_urls(URLsInfo);
	(not URLvalid) ->
		remove_invalid_urls(URLsInfo)
	end;

remove_invalid_urls(URLInfo) ->
	URL = URLInfo#url_info.mainURL,
	URLvalid = is_url_valid(URL),
	
	if (URLvalid)  ->
		[URLInfo];
	(not URLvalid) ->
		[]
	end.

%------

% Removes last character until valid
% Check if blacklisted

is_url_valid([]) ->
	false;

is_url_valid(undefined) ->
	false;

is_url_valid(URL) ->
	IllegalChars = " ;\":,>\\|.",
	
	URLtooShort = (length(URL) < 4),
	LastCharValid = not lists:member(lists:last(URL), IllegalChars),

	if LastCharValid ->
		URLblacklisted = is_url_blacklisted(URL),	
		(LastCharValid and ((not URLtooShort) and (not URLblacklisted)));
	(not LastCharValid) ->
		[_ | AS] = URL,
		is_url_valid(AS)
	end.
	

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


fix_local_urls([]) ->
	[];

fix_local_urls([URLInfo | URLs]) ->
	Prefix = URLInfo#url_info.prefix,
	URLsearched = URLInfo#url_info.urlSearched,

	if ((Prefix == undefined) or (URLsearched == undefined)) ->
		[URLInfo#url_info{isLocal = false}] ++ fix_local_urls(URLs);
	
	true ->
		IsLocal = lists:prefix("/", Prefix),
		if (IsLocal)  ->
			[URLInfo#url_info{prefix = (URLsearched ++ "/"), isLocal = true}] ++ fix_local_urls(URLs);
		(not IsLocal) ->
			[URLInfo#url_info{isLocal = false}] ++ fix_local_urls(URLs)
		end
	end.


%--------
% Removes duplicate urls and count them
count_each_url(URLs) ->
	count_each_url(URLs, 1).

count_each_url([], _) ->
	[];

count_each_url([URLInfo | []], Counter) ->
	URLInfo#url_info{occurrences = Counter};

count_each_url([URLInfo0 | URLs], Counter) ->
	
	[URLInfo1 | _ ] = URLs,
	URL0 = URLInfo0#url_info.mainURL,
	URL1 = URLInfo1#url_info.mainURL,

	FirstEqual = string:equal(URL0, URL1),

	if(FirstEqual) ->
		  count_each_url( (URLs) , (Counter+1));
	(not FirstEqual) ->
		[URLInfo0#url_info{occurrences = Counter}] ++ count_each_url(URLs, 1)
	end.

%-----------------------

%Strips url of file ending and saves in record,
%Also sets boolean "isSearchable" in record
%	(telling us if the file can be searched for additional urls)
strip_suffix([]) ->
	[];

strip_suffix([URLInfo | URLsInfo]) ->
	URL = URLInfo#url_info.mainURL,
	Scannables = [".txt", ".html", ".htm"],
	NonScannables = [".png", ".jpg", "jpeg", ".svg", ".gif", ".webm", ".mp4", ".mp3"],

	{NewURL0, Suffix0} = remove_suffix(URL, Scannables),
	Scannable = (Suffix0 /= []),
	
	if Scannable ->
		[URLInfo#url_info{mainURL = NewURL0, suffix = Suffix0, isSearchable = true}] ++ strip_suffix(URLsInfo);
	(not Scannable) ->
		{NewURL1, Suffix1} = remove_suffix(URL, NonScannables),
		[URLInfo#url_info{mainURL = NewURL1, suffix = Suffix1, isSearchable = false}] ++ strip_suffix(URLsInfo)
	end;

strip_suffix(URLInfo) ->
	strip_suffix([URLInfo]).

%------------------------

set_urlSearched([], _) ->
	[];

set_urlSearched([URLInfo | URLsInfo], URLsearched) ->
	[URLInfo#url_info{urlSearched = URLsearched}] ++ set_urlSearched(URLsInfo, URLsearched);

set_urlSearched(URLInfo, URLsearched) ->
	set_urlSearched([URLInfo], URLsearched).




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
	CurrentIsURL = ((Prefix /= []) and (Prefix /= undefined)),
	
	%look for start of valid URL
	if (not CurrentIsURL) ->

		%remove illegal char
	   	if(NextIsIllegal) ->
			if(TextIsEmpty) ->
				ok;
			(not TextIsEmpty) ->
				text_to_url_records(TextNew, [], [], Keywords)
			end;

		%look for valid URL prefix
		true ->	
		  	{TextNoPrefix, PrefixNew} = remove_prefix(Text, Keywords),
			IsUrl = ((PrefixNew /= []) and (PrefixNew /= undefined)),
		       	% URL found!
			% Seperated prefix from text
			if (IsUrl) ->
				text_to_url_records(TextNoPrefix, [], PrefixNew, Keywords);
				
			% Did NOT start with valid prefix
			% First char removed from text
			(not IsUrl) ->
				text_to_url_records(TextNew, [], [], Keywords)
			end
		end;
	
	%We are in the middle of a URL
	%continue adding to url until illegal char or EOF
	true ->
		%URL finished processing
		if(NextIsIllegal) ->
			if(TextIsEmpty) ->
				#url_info{mainURL = CurrentURL, prefix = Prefix};
			(not TextIsEmpty) ->
				[#url_info{mainURL = (CurrentURL), prefix = Prefix}]
				  ++ text_to_url_records(TextNew, [], [], Keywords)
			end;
	
		%Continuing adding to URL
		(not NextIsIllegal) ->
			if(TextIsEmpty) ->
				#url_info{mainURL = (CurrentURL++[NextChar]), prefix = Prefix};
			(not TextIsEmpty) ->
				  text_to_url_records(TextNew, (CurrentURL++[NextChar]), Prefix, Keywords)
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

%remove_prefix(List, _) ->
%	{List, []}.

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
% save_result
% 	(as well as helper functions
% 	save_result_record and save_result_formated)
%
% url_info -> boolean -> atom
% ----
% Save results to a text file,
% either as record or more human-readable
% - depending on boolean HumanReadable
% ---------------------------

save_result(URLsInfo, true) ->
	save_result_formated(URLsInfo);
save_result(URLsInfo, false) ->
	save_result_record(URLsInfo);
save_result(_,_) ->
	error.

%file name might be url searched + date
save_result_record(URLsInfo) ->
	file:write_file("result.txt", io_lib:fwrite("~p\n", [URLsInfo])).

save_result_formated([]) ->
	ok;
save_result_formated([URLInfo | URLsInfo]) ->
	Prefix = URLInfo#url_info.prefix,
	URL = URLInfo#url_info.mainURL,
	Suffix = URLInfo#url_info.suffix,
	Nr = integer_to_list(URLInfo#url_info.occurrences),
	Result = Prefix ++ URL ++ Suffix,
	file:write_file("result_humanReadable.txt", io_lib:fwrite("~p\n", [Result]), [append]),
	file:write_file("result_humanReadable.txt", io_lib:fwrite("~p\n\n", [Nr]), [append]),
	save_result_formated(URLsInfo).


% ---------------------------
% get_site_body
% String -> String 
% ----
% Send HTTP GET request to server, return body
% ---------------------------
get_site_body(Url) ->
	application:start(inets),
	get_site_body_helper(Url, 4).


% ---------------------------
% get_site_body_helper
% String -> Integer -> Body
% ----
% Helper function
% Retry if GET request fails, until success or no tries left
% ---------------------------

get_site_body_helper(Url, TriesMax) ->
	io:fwrite("\nDownloading: ~s \n", [Url]),
	get_site_body_helper(Url, TriesMax, TriesMax).

get_site_body_helper(_, 0, _) ->
	erlang:error(http_timeout);

get_site_body_helper(Url,TriesLeft, TriesMax) ->
	WaitAfterFail = 400, %ms
	FirstRun = (TriesLeft == TriesMax),
	GetReq = httpc:request(Url),
	{Response, _ } = GetReq,
	
	if (Response == ok) ->
		{_,{_,_, Body}} = GetReq,
		io:fwrite("\nProcessing: ~s \n\n", [Url]),
		Body;

   	(Response /= ok) ->
		if(FirstRun) ->
			io:fwrite("\nProblem loading page: ~s \nRetrying..", [Url]),
			timer:sleep(WaitAfterFail),
			get_site_body_helper(Url,TriesLeft-1, TriesMax);
		(not FirstRun) ->
			io:fwrite(" .. "),
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

