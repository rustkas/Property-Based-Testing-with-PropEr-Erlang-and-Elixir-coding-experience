-module(prop_exercises).

-include_lib("proper/include/proper.hrl").

%% The tree generates a data type that represents the following types:
-type tree() :: tree(term()).
-type tree(T) ::
    {node, Value :: T, Left :: tree(T) | undefined, Right :: tree(T) | undefined}.

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_am_stamp1() ->
    ?FORALL(Stamp = {H,M,S},
            am_stamp1(),
            begin
                %io:format("~p~n", [Stamp]),
				{H,M,S} = Stamp,
				H < 12 andalso M >= 0 andalso M =< 59 andalso S >=0 andalso S =< 59
            end).

prop_am_stamp2() ->
    ?FORALL(Stamp = {H,M,S},
            am_stamp2(),
            begin
                %io:format("~p~n", [Stamp]),
                {H,M,S} = Stamp,
				H < 12 andalso M >= 0 andalso M =< 59 andalso S >=0 andalso S =< 59
            end).

prop_stamps1() ->
    ?FORALL( {Stamp1 = {H1,M1,S1}, Stamp2 = {H2,M2,S2}},
            stamps1(),
            begin
                %io:format("~p : ~p~n", [Stamp1,Stamp2]),
				true
            end).

prop_stamps2() ->
    ?FORALL( {Stamp1 = {H1,M1,S1}, Stamp2 = {H2,M2,S2}},
            stamps2(),
            begin
                %io:format("~p : ~p~n", [Stamp1,Stamp2]),
				H1 =< H2 orelse M1 =< M2 orelse S1 =< S2
            end).

prop_no_standup1()->
    ?FORALL( Stamp = {_H,_M,_S},
            no_standup1(),
            begin
                %io:format("~p~n", [Stamp]),
			    true	
            end).

prop_no_standup2()->
    ?FORALL( Stamp = {_H,_M,_S},
            no_standup2(),
            begin
                %io:format("~p~n", [Stamp]),
			    true	
            end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

stamp() -> {hour(), min(), sec()}.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

hour() -> choose(0,23).
min()  -> choose(0,59).
sec()  -> choose(0,59).

%% Return hours in the morning
am_stamp1() ->
    ?SUCHTHAT({H,_,_}, stamp(), H < 12).
am_stamp2() ->
    ?LET({H,M,S}, stamp(), {H rem 12, M, S}).

%% Return two ordered timestamps
stamps1() ->
    ?SUCHTHAT({S1, S2}, {stamp(), stamp()}, 
		begin
		{H1,M1,SEK1} = S1, 
		{H2,M2,SEK2} = S2,
		S1 =< S2 andalso H1 =< H2 andalso M1 =< M2 andalso SEK1 =< SEK2
		end
		).
stamps2() ->
    ?LET({S1, S2}, {stamp(), stamp()}, {min(S1,S2), max(S1,S2)}).

%% Return any time that does not overlap standup meetings
no_standup1() ->
    ?SUCHTHAT({H,M,_}, stamp(), H =/= 9 orelse M > 10).
no_standup2() ->
    ?LET({H,M,S}, stamp(),
         case H of
            9 when M =< 10 -> {8, M, S};
            _ -> {H,M,S}
         end).