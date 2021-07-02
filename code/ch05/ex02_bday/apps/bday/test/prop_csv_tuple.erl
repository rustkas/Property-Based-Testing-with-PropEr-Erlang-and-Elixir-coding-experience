-module(prop_csv_tuple).

-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_roundtrip() ->
    ?FORALL(TupleLists,
            csv_source(),
            TupleLists
            =:= bday_csv_tuple:decode(
                    bday_csv_tuple:encode(TupleLists))).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
textdata() ->
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    ":;<=>?@ !#$%&'()*+-./[\\]^_`{|}~".

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

unquoted_text() ->
    list(elements(textdata())).

quotable_text() ->
    list(elements([$\r, $\n, $", $,] ++ textdata())).

field() ->
    oneof([unquoted_text(), quotable_text()]).

name() ->
    field().

header(Size) ->
    vector(Size, name()).

record(Size) ->
    vector(Size, field()).

entry(Size, Keys) ->
    ?LET(Vals, record(Size), lists:zip(Keys, Vals)).

csv_source() ->
    ?LET(Size,
         pos_integer(),
         ?LET(Keys, header(Size + 1), list(entry(Size + 1, Keys)))).
