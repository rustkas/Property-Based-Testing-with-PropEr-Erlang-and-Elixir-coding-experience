%% 
%% <a href="https://datatracker.ietf.org/doc/html/rfc4180">Common Format and MIME Type for Comma-Separated Values (CSV) Files</a>
-module(prop_csv).
-include_lib("proper/include/proper.hrl").
-export([csv_source/0]).


%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_roundtrip() ->
    ?FORALL(Maps, csv_source(),
            bday_csv:decode(bday_csv:encode(Maps)) =:= Maps).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

csv_source() ->
       ?LET(Size, pos_integer(),
                 ?LET(Keys, header(Size+1),
                            list(entry(Size+1, Keys)))).

entry(Size, Keys) ->
       ?LET(Vals, record(Size),
                  maps:from_list(lists:zip(Keys, Vals))).

header(Size) -> vector(Size, name()).

record(Size) -> vector(Size, field()).

name() -> field().

field() -> oneof([unquoted_text(), quotable_text()]).

unquoted_text() -> list(elements(textdata())).

quotable_text() -> list(elements([$\r, $\n, $", $,] ++ textdata())).

textdata() ->
       "A".
	   %"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
       %":;<=>?@ !#$%&'()*+-./[\\]^_`{|}~" ++ [\"\"].


%-spec textdata() -> Returns when
%  Returns :: [ !#$%&'()*+-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~].
%textdata_hex() ->
%       [16#20,16#21] ++ lists:seq(16#23,16#2B) ++ lists:seq(16#2D,16#7E).