-module(prop_solutions).

-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_word_count() ->
    ?FORALL(String, (non_empty(string())),
            (solutions:word_count(String) =:= alt_word_count(String))).

alt_word_count(String) -> space(String).

space([]) -> 0;
space([$\s | Str]) -> space(Str);
space(Str) -> word(Str).

word([]) -> 1;
word([$\s | Str]) -> 1 + space(Str);
word([_ | Str]) -> word(Str).
