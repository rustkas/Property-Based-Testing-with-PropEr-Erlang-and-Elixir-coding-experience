-module(prop_exercises).

-include_lib("proper/include/proper.hrl").

%% The tree generates a data type that represents the following types:
-type tree() :: tree(term()).
-type tree(T) ::
    {node, Value :: T, Left :: tree(T) | undefined, Right :: tree(T) | undefined}.

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
% Do not run it - it is infinite tree maker.
% For illustration only.
prop_tree() ->
    ?FORALL(Type,
            tree(),
            begin
                io:format("~p~n", [Type]),
                true
            end).

prop_lazy_tree() ->
    ?FORALL(Type,
            lazy_tree(),
            begin
                io:format("~p~n", [Type]),
                true
            end).

prop_limited_tree() ->
    ?FORALL(Type,
            limited_tree(),
            begin
                io:format("~p~n", [Type]),
                true
            end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

tree() ->
    tree(term()).

tree(Type) ->
    frequency([{1, {node, Type, tree(Type), undefined}},
               {1, {node, Type, undefined, tree(Type)}},
               {5, {node, Type, tree(Type), tree(Type)}}]).

lazy_tree() ->
    lazy_tree(term()).

lazy_tree(Type) ->
    frequency([{3, {node, Type, undefined, undefined}},
               {2, {node, Type, ?LAZY(tree(Type)), undefined}},
               {2, {node, Type, undefined, ?LAZY(tree(Type))}},
               {3, {node, Type, ?LAZY(tree(Type)), ?LAZY(tree(Type))}}]).

limited_tree() ->
    limited_tree(boolean()).

limited_tree(Type) ->
    ?SIZED(Size, limited_tree(Size, Type)).

limited_tree(Size, Type) when Size =< 1 ->
    {node, Type, undefined, undefined};
limited_tree(Size, Type) ->
    frequency([{1, {node, Type, ?LAZY(limited_tree(Size - 1, Type)), undefined}},
               {1, {node, Type, undefined, ?LAZY(limited_tree(Size - 1, Type))}},
               {5,
                {node,
                 Type,
                 %% Divide to avoid exponential growth
                 fixed_list([?LAZY(limited_tree(Size div 2, Type)),
                             ?LAZY(limited_tree(Size div 2, Type))])}}]).
