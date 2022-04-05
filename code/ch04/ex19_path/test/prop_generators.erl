-module(prop_generators).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_path() ->
    ?FORALL(Path, path(),
      begin
	    io:format("~p~n", [Path]),
        true
      end).


%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
move(left, {X,Y}) -> {X-1,Y};
move(right, {X,Y}) -> {X+1,Y};
move(up, {X,Y}) -> {X,Y+1};
move(down, {X,Y}) -> {X,Y-1}.


%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
path() ->
           %   Current, Acc,  VisitedMap     ToIgnore
           path({0,0}, [], #{{0,0} => seen}, []).

path(_Current, Acc, _Seen, [_,_,_,_]) -> % all directions tried
    Acc; % we give up
path(Current, Acc, Seen, Ignore) ->
    non_empty(
    frequency([
	    {1, Acc}, % probabilistic stop
	    {15, increase_path(Current, Acc, Seen, Ignore)}
	])).

increase_path(Current, Acc, Seen, Ignore) ->
    DirectionGen = oneof([left, right, up, down] -- Ignore),
    ?LET(Direction, DirectionGen,
      begin
        NewPos = move(Direction, Current),
        case Seen of
            #{NewPos := _} -> % exists
                path(Current, Acc, Seen, [Direction|Ignore]); % retry
            _ ->
                path(NewPos, [Direction|Acc], Seen#{NewPos => seen}, [])
        end
      end).