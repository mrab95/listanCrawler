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

search_text_for_url_helper(Prefix, TextNew) ->
	IsUrl = not string:equal(Prefix, ""),
  
	if (IsUrl) ->
		search_text_for_url(TextNew, Prefix, true);
	(not IsUrl) ->
		search_text_for_url(TextNew, "", false)
	end.


%Search text for URLs and return those 
search_text_for_url(Text, CurrentURL, CurrentIsURL) ->	

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
		(true) ->
	
			%{Prefix, TextNew}
			Tmp = find_remove_url_prefix(Text),
			{Prefix, TextNew} = Tmp,
			search_text_for_url_helper(Prefix, TextNew)
		end;


	%We are in the middle of a URL
	%continue adding to url until illegal char or EOF
	(true) ->
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
	end.


%If Text starts with an url prefix (eg www. http://)
%Then remove that prefix otherwise only remove first char
find_remove_url_prefix(Text) ->
	ValidPrefix = ["http://", "www."],
	TextLen = string:len(Text),
	
	%Valid if text starts with prefix
	TextMatchingPrefix = starts_with_on_list(Text, ValidPrefix),	
	%io:fwrite(TextMatchingPrefix),
	PrefixIsValid = not string:equal(TextMatchingPrefix, ""),
	
	% LITE FAKTA
	% till slut så är
	% TextMatchingPrefix = http://
	% PrefixIsValid = true
	% verkar som starts with on list returnerar fel
	% !! EXCEPTION ERROR no match of right hand side value 
	
	if (PrefixIsValid) ->
		PrefixLen = string:len(TextMatchingPrefix),
		ElemToRemove = min(PrefixLen+1,TextLen+1),
		TextNew = string:substr(Text, ElemToRemove),
		io:fwrite(Text),
		io:fwrite("\n\n\n\n\n\n"),
		io:fwrite(TextNew),
		{TextMatchingPrefix, TextNew};
	

	% !! VERKAR FUNKA
   	%No prefix found remove first character
	(not PrefixIsValid) ->
%		io:fwrite("a"),

		ElemToRemove = min(TextLen+1, 2),
		TextNew =string:substr(Text, ElemToRemove),
		{"", TextNew}
	end.


%Returns substring if another string begins with it
starts_with_on_list(String, [SubString | SubStrings]) ->

	%There are multple substrings, search recursively before giving up
	if (SubStrings /= []) ->
		FoundMatch = starts_with(String, SubString),
		if (FoundMatch) ->
			SubString;
		(not FoundMatch) ->
			starts_with_on_list(String, SubStrings)
		end;
	
	
	(true) ->
		FoundMatch = starts_with(String, SubString),
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
	StringLen = string:len(String),
	SubStringLen = string:len(SubString),
	CharsLeft = min(StringLen,SubStringLen),

	% Bad argument (empty list) 
	if (CharsLeft < 1) ->
		false;

	% Last char, all chars before were equal, compare and return
  	(CharsLeft == 1) ->
		string:equals(String, SubString)
	
	% Compare char is equal, check next one if so
	 (CharsLeft > 1) ->
		[a|as] = String,
		[b|bs] = SubString,
		CharsEqual = string:equals(a,b),

		if(CharsEqual) ->
		 	 starts_with(as,bs);
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
	{Successful, {_, _, Body}} = httpc:request(Url),
	
	if (Successful == ok) ->
		Body;
	(Successful /= ok) ->
		if(TriesLeft == 1) ->
			erlang:error(http_timeout);	
		true ->
			io:fwrite("\nCannot connect, retrying.. tries left: ~p", TriesLeft-1),
			timer:sleep(WaitAfterFail),
			get_site_body_helper(Url,TriesLeft-1)
		end
	end.

	
%FOR TLS
%application:start(crypto),
%application:start(public_key),
%application:start(ssl),
%application:start(inets)
