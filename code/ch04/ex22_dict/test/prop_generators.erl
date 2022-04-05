-module(prop_generators).

-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_dict_gen() ->
    ?FORALL(D,
            dict_gen(),
            begin
                %io:format("~p~n",[D]),
                Result = dict:size(D) < 5,
                if Result ->
                       %io:format("~p~n", [D]),
                       true;
                   true ->
                       true
                end
            end).

prop_dict_symb() ->
    ?FORALL(DSymb,
            dict_symb(),
            begin
                %io:format("~p~n", [DSymb]),
                Result = dict:size(eval(DSymb)) < 5,
                if Result ->
                       %io:format("~p~n", [DSymb]),
                       true;
                   true ->
                       false
                end
            end).

prop_dict_autosymb() ->
    ?FORALL(D,
            dict_autosymb(),
            begin
                %io:format("~p~n", [D]),
                Result = dict:size(D) < 5,
                if Result ->
                       %io:format("~p~n", [D]),
                       true;
                   true ->
                       false
                end
            end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
dict_gen() ->
    ?LET(X, list({integer(), integer()}), dict:from_list(X)).

dict_symb() ->
    ?SIZED(Size, dict_symb(Size, {call, dict, new, []})).

dict_symb(0, Dict) ->
    Dict;
dict_symb(N, Dict) ->
    dict_symb(N - 1, {call, dict, store, [integer(), integer(), Dict]}).

dict_autosymb() ->
    ?SIZED(Size, dict_autosymb(Size, {'$call', dict, new, []})).

dict_autosymb(0, Dict) ->
    Dict;
dict_autosymb(N, Dict) ->
    dict_autosymb(N - 1, {'$call', dict, store, [integer(), integer(), Dict]}).
