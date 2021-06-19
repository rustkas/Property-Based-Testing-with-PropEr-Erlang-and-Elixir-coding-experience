-module(prop_generators).

-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_escape() -> ?FORALL(Str, (string()), (aggregate(classes(Str), escape(Str)))).


%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
escape(_) -> true. % we don't care about this for this example

classes(Str) ->
    L = letters(Str),
    N = numbers(Str),
    P = punctuation(Str),
    O = length(Str) - (L + N + P),
    [{letters, to_range(5, L)}, {numbers, to_range(5, N)}, {punctuation, to_range(5, P)}, {others, to_range(5, O)}].

letters(Str) -> length([1 || Char <- Str, Char >= $A andalso Char =< $Z orelse Char >= $a andalso Char =< $z]).

numbers(Str) -> length([1 || Char <- Str, Char >= $0, Char =< $9]).

punctuation(Str) -> length([1 || Char <- Str, lists:member(Char, ".,;:'\"-")]).

to_range(M, N) ->
    Base = N div M,
    %io:format("~2B = ~2B div ~B | {~2B, ~2B} = {~2B * ~B, (~B + 1)*~B}~n", 
	%          [Base, N, M, Base * M, (Base + 1) * M, Base, M, Base, M]),
    {Base * M, (Base + 1) * M}.