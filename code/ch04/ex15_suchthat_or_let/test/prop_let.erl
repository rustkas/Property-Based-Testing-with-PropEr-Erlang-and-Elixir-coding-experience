-module(prop_let).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_stress_test1() ->
    ?FORALL(Vector, ?SIZED(Size,vector(Size*350, even())),
        begin
            lists:all(fun(Elem)-> Elem rem 2 =:= 0 end,Vector)
        end).


prop_stress_test2() ->
    ?FORALL(Vector, ?SIZED(Size,vector(Size*350, uneven())),
        begin
            lists:all(fun(Elem)-> Elem rem 2 =/= 0 end,Vector)
        end).
		
prop_even() ->
    ?FORALL(Item, even(),
        begin
            Item rem 2 =:= 0
        end).

prop_uneven() ->
    ?FORALL(Item, uneven(),
        begin
            Item rem 2 =/= 0
        end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
even() -> ?LET(N, integer(), N*2).
uneven() -> ?LET(N, integer(), N*2 + 1).


%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

