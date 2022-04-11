%% 
%% <a href="https://datatracker.ietf.org/doc/html/rfc4180">Common Format and MIME Type for Comma-Separated Values (CSV) Files</a>
-module(prop_csv).
-include_lib("proper/include/proper.hrl").
-export([csv_source/0, quotable_text/0]).


%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_roundtrip() ->
    ?FORALL(Lists, csv_source(),
            bday_csv:decode(bday_csv:encode(Lists)) == Lists).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

entry(Size, Keys) ->
       ?LET(Vals, record(Size),
                  lists:zip(Keys, Vals)).

csv_source() ->
       ?LET(Size, pos_integer(),
                 ?LET(Keys, header(Size+1),
                            list(entry(Size+1, Keys)))).

header(Size) -> vector(Size, name()).

record(Size) -> vector(Size, field()).

name() -> field().

field() -> oneof([unquoted_text(), quotable_text()]).

unquoted_text() -> list(elements(textdata())).

quotable_text() -> 
		?LET(List, list(elements([$\r, $\n, $", $,] ++ textdata())),
					[$"] ++ List ++ [$"]).


textdata() ->
       "A".
	   %"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
       %":;<=>?@ !#$%&'\"()*+-./[\\]^_`{|}~".


%-spec textdata() -> Returns when
%  Returns :: [ !#$%&'()*+-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~].
%textdata_hex() ->
%       [16#20,16#21] ++ lists:seq(16#23,16#2B) ++ lists:seq(16#2D,16#7E).