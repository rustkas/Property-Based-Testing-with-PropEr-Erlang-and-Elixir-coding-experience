-module(rfc_tuple_encode_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

rfc_empty_09_test() ->
    InputList =
        [{[], []},
         {[], []},
         {[66], []},
         {[], []},
         {[65], []},
         {[67], []},
         {[68], []},
         {[], []}],
    Result = bday_csv_tuple:encode(InputList),
    %?assertEqual("hDV#D/,r-(K?@oq6|8lEj<.:wWmAFeQN!~MiA\r\nV~yeYx9(M{Q4X_0\\'|~l,uIf#8$_@8T.F23P7v I\r\n",
    %             Result).%?debugFmt("~p~n", [Result]).
    ?debugFmt("~p~n", [Result]).

-endif.
