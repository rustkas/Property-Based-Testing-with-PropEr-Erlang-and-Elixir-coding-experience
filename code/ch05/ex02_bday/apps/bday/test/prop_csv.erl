-module(prop_csv).

-include_lib("proper/include/proper.hrl").

-export([csv_source/0]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_roundtrip() ->
    ?FORALL(Maps,
            csv_source(),
            Maps
            =:= bday_csv:decode(
                    bday_csv:encode(Maps))).

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

header(Size) ->
    vector(Size, name()).

record(Size) ->
    vector(Size, field()).

name() ->
    field().

entry(Size, Keys) ->
    ?LET(Vals,
         record(Size),
         maps:from_list(
             lists:zip(Keys, Vals))).

csv_source() ->
    ?LET(Size,
         pos_integer(),
         ?LET(Keys, header(Size + 1), list(entry(Size + 1, Keys)))).
