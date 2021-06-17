-module(solutions).

-export([word_count/1]).

word_count(String) ->
    Stripped = string:trim(dedupe_spaces(String),
                           both,
                           " "),
    Spaces = lists:sum([1
                        || Char <- Stripped, Char =:= $\s]),
    case Stripped of
        "" -> 0;
        _ -> Spaces + 1
    end.

dedupe_spaces([]) -> [];
dedupe_spaces([$\s, $\s | Rest]) ->
    dedupe_spaces([$\s | Rest]);
dedupe_spaces([H | T]) -> [H | dedupe_spaces(T)].
