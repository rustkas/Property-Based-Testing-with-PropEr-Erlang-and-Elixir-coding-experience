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
    ?SIZED(Size,
           %   Max, Current, Acc,  VisitedMap     ToIgnore
           path(Size, {0,0}, [], #{{0,0} => seen}, [])).

path(0, _Current, Acc, _Seen, _Ignore) -> % directions limit
        Acc; % max depth reached
path(_Max, _Current, Acc, _Seen, [_,_,_,_]) -> % all directions tried
    Acc; % we give up
path(Max, Current, Acc, Seen, Ignore) ->
        increase_path(Max, Current, Acc, Seen, Ignore).

increase_path(Max, Current, Acc, Seen, Ignore) ->
        DirectionGen = oneof([left, right, up, down] -- Ignore),
    ?LET(Direction, DirectionGen,
              begin
                   NewPos = move(Direction, Current),
          case Seen of
            #{NewPos := _} -> % exists
                path(Max, Current, Acc, Seen, [Direction|Ignore]); % retry
            _ ->
                path(Max-1, NewPos, [Direction|Acc],
                     Seen#{NewPos => seen}, [])
         end
      end).
