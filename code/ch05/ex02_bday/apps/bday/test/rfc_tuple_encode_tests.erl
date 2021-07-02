-module(rfc_tuple_encode_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

rfc_empty_01_test() ->
    InputList = [{[95], []}, {[], []}],
    Result = bday_csv_tuple:encode(InputList).
    %?debugFmt("~p~n", [Result]).


rfc_empty_02_test() ->
    InputList = [{[],[]},{[],[]}],
    Result = bday_csv_tuple:encode(InputList),
    ?debugFmt("~p~n", [Result]).
-endif.
