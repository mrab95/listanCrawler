-module(http).
-include_lib("kernel/include/inet.hrl").
-export([main/0]).


main() ->
	search_site("http://sweclockers.com/").

% Get body from site
% Search for links
search_site(Url) ->
	Body = get_site_body(Url),
%	Body = (list_to_binary(BodyList)),

        %Body = "asdwww.sweclockers.com  a pahttp://swec.com apa",
	UrlList =  search_text_for_url(Body, "", false),
	(UrlList).
	%make list to list of tuples where each have occurances of url and remove duplicates
	%read text file, do the same and save


%Search text for URLs and return those 
search_text_for_url(Text, CurrentURL, CurrentIsURL) ->	

	AllTextEmpty = string:equal(Text, ""),	

	if(AllTextEmpty) ->
		  CurrentURL;

  
	(true) ->
		[NextChar | TextNew] = Text,
		NextIsIllegal = unallowed_char(NextChar),
		TextIsEmpty = string:equal(TextNew, ''),
		
		%Currently not in URL
		%look for start of valid URL
		if (not CurrentIsURL) ->
			if(NextIsIllegal) ->
				io:fwrite("b"),
				if(TextIsEmpty) ->
					CurrentURL;
				(not TextIsEmpty) ->
					search_text_for_url(TextNew, "", false)
				end;
			 
			(true) ->
				io:fwrite("a"),
				{Prefix, TextNew} = find_remove_url_prefix(Text),
				IsUrl = not string:equal(Prefix, ""),
					
				if (IsUrl) ->
					search_text_for_url(TextNew, Prefix, true);
				(not IsUrl) ->
					search_text_for_url(TextNew, "", false)
				end
			end;

		%We are in the middle of a URL
		%continue adding to url until " " or EOF
		(true) ->
			%NextIsSpace = string:equal([NextChar], " "), %ALREADY CHECKED IN unallowed_char !
			%TextIsEmpty = string:equal(TextNew, ''),

			if(NextIsIllegal) ->
				if(TextIsEmpty) ->
					CurrentURL;
				(not TextIsEmpty) ->
					CurrentURL ++ "\n" ++ search_text_for_url(TextNew, "", false)
				end;
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


%If Text starts with an url prefix (www. http://)
%Then remove that prefix otherwise only remove first char
find_remove_url_prefix(Text) ->
	ValidPrefix = ["http://", "www."],
	
	%Text starts with ValidPrefix
	ActualPrefix = starts_with_on_list(Text, ValidPrefix),	
	IsValid = not string:equal(ActualPrefix, ""),
	
	TextLen = string:len(Text),
	if (IsValid) ->
		% Remove prefix from text
		CharsToCompare = min(string:len(ActualPrefix)+1,string:len(Text)),
		{ActualPrefix, string:substr(Text, CharsToCompare)};
	(true) ->
		   if (TextLen < 2) ->
			      {"",""};
		   (true) ->
			   {"",string:substr(Text,2)} %remove first char
		end
	end.

%Does string start with any of supplied substrings,
%Returns first substring that matches this, else return ""
starts_with_on_list(String, [SubString | SubStrings]) ->
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
		(not FoundMatch) ->
			""
		end
	end.


%Does string start with supplied string,
%if string is smaller than substring just match on available characters
starts_with(String, SubString) ->
	StringFormated = String,
	%	StringFormated = list_to_binary(String),
	IndexOfSubStr = string:str(StringFormated, SubString),
	CharsToCompare = min(string:len(SubString)+1,string:len(String)),

	% True if String contains Substring, and only if SubString starts at first char
	if (IndexOfSubStr == 1) ->
		string:substr(String, CharsToCompare),
		true;
	true ->
		false
	end.

unallowed_char(Char) ->
	AllowedChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~:/?#[]@!$&'()*+,;=",
	
	IllegalChar = not lists:member(Char, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~:/?#[]@!$&'()*+,;="),
	%CharAllowedPos = string:substr(AllowedChars, [Char]),

	IllegalChar.
%	if (CharAllowedPos == 0) ->
%		   true; %Char is illegal
%	(true) ->
%		   false
%	end.




%Send get request to server (resolves DNS etc)
%Returns body of specified url
get_site_body(Url) ->
	inets:start(),
	%UserAgent = "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:40.0) Gecko/20100101 Firefox/40.1",
	% {ok, {{HttpVer, Code, Msg}, Headers, Body}} =
	{ok, {_, _, Body}} = httpc:request(Url),
	Body.















%create_udp_socket() ->
%	PortNr = trunc(random:uniform()*30000), %varför i självaste fan ska det vara '=' här
%	io:fwrite("Trying to bind port ~p \n", [PortNr]),	
%	if
%		((PortNr < 2000) or (PortNr > 30000)) -> % or (PortNr > 30000)) ->
%			io:fwrite("Invalid port, retrying..\n"),
%			main(),
%			exit(self(), normal); 
%		true ->
%			Socket = {PortBound, SocketId} = gen_udp:open(PortNr),
%			if
%				(PortBound /= ok) ->
%					main(),
%					exit(self(), normal);
%				true ->	
%					io:fwrite("Success!\n")
%			end
%	end.

%	case Socket of
%		undefined ->
%			{0, 0};
%		_ ->
%			Socket
%	end;

