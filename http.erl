-module(http).
-include_lib("kernel/include/inet.hrl").
-export([main/0]).


main() ->
	search_site("http://sweclockers.com/").


% Find all URLs on website
search_site(Url) ->
	Body = get_site_body(Url),

	%Body = (list_to_binary(BodyList)),
        %Body = "asdwww.sweclockers.com  a pahttp://swec.com apa",
	
	UrlList =  search_text_for_url(Body, "", false),
	io:fwrite(UrlList),
	UrlList.



%Search text for URLs and return those 
search_text_for_url(Text, CurrentURL, CurrentIsURL) ->	
	
	TextLen = string:len(Text),
	if(TextLen < 1) ->
		  [];

  	true ->
		%maybe fails on text starting empty
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
	
	% !! EXCEPTION ERROR no match of right hand side value !! 
			
			  	{Prefix, TextNew} = find_remove_url_prefix(Text),
				IsUrl = not string:equal(Prefix, ""),
		  
				if (IsUrl) ->
					search_text_for_url(TextNew, Prefix, true);
				(not IsUrl) ->
					search_text_for_url(TextNew, "", false)
				end
			end;
	
	
		%We are in the middle of a URL
		%continue adding to url until illegal char or EOF
		true ->
			%URL finished processing
			if(NextIsIllegal) ->
				if(TextIsEmpty) ->
					CurrentURL;
				(not TextIsEmpty) ->
					CurrentURL ++ "\n" ++ search_text_for_url(TextNew, "", false)
				end;
	
			%Continuing adding to URL
			(not NextIsIllegal) ->
				if(TextIsEmpty) ->
					  CurrentURL ++ NextChar; 
				(not TextIsEmpty) ->
					  TmpURL = CurrentURL ++ [NextChar],
					  search_text_for_url(TextNew, TmpURL, CurrentIsURL)
				end
			end
		end
	end.


%If Text starts with an url prefix (eg www. http://)
%Then remove that prefix otherwise only remove first char
find_remove_url_prefix(Text) ->

	% Order is relevant, we want to look for http:// before www.
	% ! ADD HTTPS, more? !
	ValidPrefix = ["http://", "https://" , "www."],
	TextLen = string:len(Text),
	
	%Valid if text starts with prefix
	TextMatchingPrefix = starts_with_on_list(Text, ValidPrefix),	
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
starts_with_on_list(String, [SubString | SubStrings]) ->

	FoundMatch = starts_with(String, SubString),
	
	%There are multple substrings, search recursively before giving up
	if (SubStrings /= []) ->
		if (FoundMatch) ->
			SubString;
		(not FoundMatch) ->
			starts_with_on_list(String, SubStrings)
		end;
	
	
	true ->
		if (FoundMatch) ->
			SubString;
		   
		% Not found return ""
		(not FoundMatch) ->
			""
		end
	end.


%Does string start with substring
%Matches on available characters if string is smaller
starts_with(String, SubString) ->
	CharsLeft = min(string:len(String),string:len(SubString)),
	
	%Make same length
	StringShort = string:substr(String, 1, string:len(SubString)),
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
		 	starts_with(AS,BS);
		true ->
			 false
		end
	end.




%Is char allowed in URL
unallowed_char(Char) ->
	AllowedChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~:/?#[]@!$&'()*+,;=",
	IllegalChar = not lists:member(Char, AllowedChars),

	IllegalChar.


%Send HTTP GET request to server, return body
get_site_body(Url) ->
	application:start(inets),
%	inets:start(),
	get_site_body_helper(Url, 10).


% Retry if GET request fails, until success or no tries left
get_site_body_helper(Url, TriesLeft) ->
	WaitAfterFail = 2000,
%	{Successful, {_, _, Body}} = httpc:request(Url),
	GetReq = httpc:request(Url),
	{Successful, _ } = GetReq,
	
	if (Successful == ok) ->
		{_,{_,_, Body}} = GetReq,
		Body;
	(Successful /= ok) ->
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
