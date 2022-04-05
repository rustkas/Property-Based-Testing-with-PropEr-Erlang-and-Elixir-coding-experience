-module(prop_generators).

-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_dupes() ->
    ?FORALL(KV, (list({key(), val()})),
            begin
                M = maps:from_list(KV),
				% if you would like to see 
				% what {key,value} contains,
				% please, uncomment next log block
				% io:format("~n---------~n"),
				% lists:foreach(fun({K, V}) ->
				%                io:format("{Key = ~p, Value = ~p}",[K,V]) end, KV),
				%io:format("~n---------~n"),				
                _ = [maps:get(K, M) || {K, _V} <- KV], % crash if K's not in map
                collect({dupes, to_range(5, length(KV) - length(lists:ukeysort(1, KV)))}, true)
            end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
key() -> integer().

val() -> term().

to_range(M, N) ->
    Base = N div M,
    io:format("~2B = ~2B div ~B | {~2B, ~2B} = {~2B * ~B, (~B + 1)*~B}~n", 
	          [Base, N, M, Base * M, (Base + 1) * M, Base, M, Base, M]),
    {Base * M, (Base + 1) * M}.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
