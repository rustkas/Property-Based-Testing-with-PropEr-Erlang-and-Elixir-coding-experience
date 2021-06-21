%%% Checking for the existence of an error when trying to create a generator

-module(prop_generators).

-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_range() ->
    ?FORALL(List,
            check_length(list(range(-10, 0))),
            lists:all(fun(Elem) -> is_integer(Elem) end, List)).

prop_type() ->
    ?FORALL(List,
            check_length(list(string())),
            lists:any(fun(Elem) -> is_integer(Elem) end, List)).

prop_list_content_integer() ->
    ?FORALL(List,
            check_list_content_integer(list(string())),
            lists:any(fun(Elem) -> is_integer(Elem) end, List)).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
check_length(Gen) ->
    ?SUCHTHAT(L, Gen, length(L) > 10).

check_list_content_integer(Gen) ->
    ?SUCHTHAT(L, Gen, lists:any(fun(Elem) -> is_integer(Elem) end, L)).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
