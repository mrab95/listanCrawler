-module(test2).
-export([main/0]).
-record(r, {field1}).

main () ->
	Old = [#r{field1 = "old"},#r{field1 = "old"}],
	change_record(Old).


change_record(Old = [#r{} | [#r{}]]) ->
	% Change first matching keyword old
	Result = lists:keyreplace("old", #r.field1, Old, {r, undefined}),
	Result;

change_record(_) ->
	undefined.
