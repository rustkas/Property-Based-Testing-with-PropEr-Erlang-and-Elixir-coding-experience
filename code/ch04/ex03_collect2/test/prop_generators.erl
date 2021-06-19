-module(prop_generators).

-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_collect2() ->
    ?FORALL(Bin, (binary()),
            (collect(to_range(10, byte_size(Bin)),
                     is_binary(Bin)))).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
to_range(M, N) ->
    Base = N div M,
    io:format("~2B = ~2B div ~B | {~2B, ~B} = {~2B * ~B, (~B + 1)*~B~n",
              [Base, N, M, Base * M, (Base + 1) * M, Base, M, Base, M]),
    {Base * M, (Base + 1) * M}.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
