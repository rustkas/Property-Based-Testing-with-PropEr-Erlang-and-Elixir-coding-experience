-module(prop_generators).
-export([key/0,val/0]).

-include_lib("proper/include/proper.hrl").


%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_dupes() ->
    ?FORALL(KV, (list({key(), val()})),
            begin
                M = maps:from_list(KV),
				% crash if K's not in map
                _ = [maps:get(K, M) || {K, _V} <- KV], 
                true
            end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
key() -> integer().

val() -> term().

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
