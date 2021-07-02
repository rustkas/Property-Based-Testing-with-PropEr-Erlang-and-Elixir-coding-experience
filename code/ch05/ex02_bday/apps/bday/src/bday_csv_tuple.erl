-module(bday_csv_tuple).

-export([encode/1, decode/1]).

%% @doc Take a list of maps with the same keys and transform them
%% into a string that is valid CSV, with a header.
-spec encode([{string(), string()}]) -> string().
encode([]) ->
    "";
encode(TupleList) ->
    Keys = get_csv_keys(TupleList),
    Values = get_csv_values(TupleList),
    Result = lists:flatten([Keys, "\r\n", Values]),
    Result.

%% @doc Take a string that represents a valid CSV data dump
%% and turn it into a list of maps with the header entries as keys
-spec decode(string()) -> [[{string(), string()}]].
decode("") ->
    [];
decode(CSV) ->
    {Headers, Rest} = decode_header(CSV),

    Rows = decode_rows(Rest),

    [lists:zip(Headers, Row) || Row <- Rows].

%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%

%% @private divide list to two
divide_list(N, List) when N >= 1, length(List) >= 1 ->
    divide_list(N, List, []).

%% @private divide list to two
divide_list(N, [], Acc) ->
    Acc;
divide_list(N, List, Acc) ->
    {NewSubList, NewList} = lists:split(N, List),
    divide_list(N, NewList, [NewSubList | Acc]).

%% @private return sorted keys
get_csv_keys(TupleList) ->
    UsortedKeys = proplists:get_keys(TupleList),
    EscapedKeys = lists:map(fun(Key) -> escape(Key) end, UsortedKeys),
    SortedKeys = lists:sort(EscapedKeys),
    JoinedList = lists:join(",", SortedKeys),
    lists:flatten(JoinedList).

%% @private return string of values
get_csv_values(TupleList) ->
    AllValuesList = lists:map(fun(Tuple) -> element(2, Tuple) end, TupleList),
    AllEscapedValuesList =
        lists:map(fun(Value) -> escape(Value) end, AllValuesList),
    DividedLists = divide_list(3, AllEscapedValuesList),
    DividedListsWithRowEnd =
        lists:map(fun(ListItem) ->
                     JoinedList = lists:join(",", ListItem),
                     lists:append(JoinedList, "\r\n")
                  end,
                  DividedLists),

    OneString = lists:flatten(DividedListsWithRowEnd),
    OneString.

%% @private return a possibly escaped (if necessary) field or name
-spec escape(string()) -> string().
escape(Field) ->
    case escapable(Field) of
        true ->
            "\"" ++ do_escape(Field) ++ "\"";
        false ->
            Field
    end.

%% @private checks whether a string for a field or name needs escaping
-spec escapable(string()) -> boolean().
escapable(String) ->
    lists:any(fun(Char) -> lists:member(Char, [$", $,, $\r, $\n]) end, String).

%% @private replace escapable characters (only `"') in CSV.
%% The surrounding double-quotes are not added; caller must add them.
-spec do_escape(string()) -> string().
do_escape([]) ->
    [];
do_escape([$", $" | Str]) ->
    [$", $", $", $" | do_escape(Str)];
do_escape([$" | Str]) ->
    [$", $" | do_escape(Str)];
do_escape([Char | Rest]) ->
    [Char | do_escape(Rest)].

%% @private Decode the entire header line, returning all names in order
-spec decode_header(string()) -> {[string()], string()}.
decode_header(String) ->
    decode_row(String).

%% @private Decode all rows into a list.
-spec decode_rows(string()) -> [string()].
decode_rows(String) ->
    case decode_row(String) of
        {Row, ""} ->
            [Row];
        {Row, Rest} ->
            [Row | decode_rows(Rest)]
    end.

%% @private Decode an entire row, with all values in order
-spec decode_row(string()) -> {[string()], string()}.
decode_row(String) ->
    decode_row(String, []).

%% @private Decode an entire row, with all values in order
-spec decode_row(string(), [string()]) -> {[string()], string()}.
decode_row(String, Acc) ->
    case decode_field(String) of
        {ok, Field, Rest} ->
            decode_row(Rest, Acc ++ [Field]);
        {done, Field, Rest} ->
            Result = Acc ++ [Field],
            {Result, Rest}
    end.

%% @private Decode a field; redirects to decoding quoted or unquoted text
-spec decode_field(string()) -> {ok | done, string(), string()}.
decode_field([$" | Rest] = Input) ->
    decode_quoted(Input);
decode_field(String) ->
    decode_unquoted(String).

%% @private Decode an unquoted string
-spec decode_unquoted(string()) -> {ok | done, string(), string()}.
decode_unquoted(String) ->
    decode_unquoted(String, []).

%% @private Decode an unquoted string
% `ok` returns when we have more data at the right
% `done` returns when we have no data at the right
-spec decode_unquoted(string(), [char()]) -> {ok | done, string(), string()}.
decode_unquoted([], Acc) ->
    {done, Acc, ""};
decode_unquoted([$\r, $\n | Rest], Acc) ->
    {done, Acc, Rest};
decode_unquoted([$, | Rest], Acc) ->
    {ok, Acc, Rest};
decode_unquoted([Char | Rest], Acc) ->
    decode_unquoted(Rest, Acc ++ [Char]).

%% @private Decode a quoted string
-spec decode_quoted(string()) -> {ok | done, string(), string()}.
decode_quoted(String) ->
    decode_quoted(String, []).

-spec decode_quoted(string(), [char()]) -> {ok | done, string(), string()}.
decode_quoted([$"], Acc) ->
    {done, Acc ++ [$"], ""};
decode_quoted([$", $\r, $\n | Rest], Acc) ->
    {done, Acc ++ [$"], Rest};
decode_quoted([$", $, | Rest], Acc) ->
    {ok, Acc ++ [$"], Rest};
decode_quoted([$", $" | Rest], Acc) ->
    decode_quoted(Rest, Acc ++ [$"]);
decode_quoted([Char | Rest], Acc) ->
    decode_quoted(Rest, Acc ++ [Char]).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%
%%% Decoding %%%
%%%%%%%%%%%%%%%%

decode_unquoted_01_test() ->
    Result = decode_unquoted(""),
    ?assertEqual({done, [], []}, Result).

decode_unquoted_02_test() ->
    Result = decode_unquoted("\r\n"),
    ?assertEqual({done, [], []}, Result).

decode_unquoted_03_test() ->
    Result = decode_unquoted(","),
    ?assertEqual({ok, [], []}, Result).

decode_unquoted_04_test() ->
    Result = decode_unquoted(",\r\n"),
    ?assertEqual({ok, [], [$\r, $\n]}, Result).

decode_unquoted_05_test() ->
    Result = decode_unquoted("aaa"),
    ?assertEqual({done, "aaa", []}, Result).

decode_unquoted_06_test() ->
    Result = decode_unquoted("aaa,bbb"),
    ?assertEqual({ok, "aaa", "bbb"}, Result).

decode_quoted_01_test() ->
    String = "\"a\r\nbc\"",
    Result = decode_field(String),
    ?assertEqual({done, "\"a\r\nbc\"", ""}, Result).

decode_quoted_02_test() ->
    String = "\"a\"\"a\"\r\nbc",
    Result = decode_field(String),
    ?assertEqual({done, "\"a\"a\"", "bc"}, Result).

decode_field_01_test() ->
    Result = decode_field("aaa,bbb"),
    ?assertEqual({ok, "aaa", "bbb"}, Result).

decode_row_2_01_test() ->
    F = fun Decode_row(String, Acc) ->
                case decode_field(String) of
                    {ok, Field, Rest} ->
                        %?debugFmt("~p~n", [Field]),
                        Decode_row(Rest, [Field | Acc]);
                    {done, Field, Rest} ->
                        %?debugFmt("~p~n", [Field]),
                        Result = lists:reverse([Field | Acc]),
                        {Result, Rest}
                end
        end,
    String = "aaa,bbb",
    Result = F(String, []),
    ?assertEqual({["aaa", "bbb"], []}, Result).

decode_row_1_01_test() ->
    DecodeRow_2 =
        fun Decode_row(String, Acc) ->
                case decode_field(String) of
                    {ok, Field, Rest} ->
                        Decode_row(Rest, [Field | Acc]);
                    {done, Field, Rest} ->
                        Result = lists:reverse([Field | Acc]),
                        {Result, Rest}
                end
        end,
    DecodeRow_1 =
        fun Decode_row(String) ->
                DecodeRow_2(String, [])
        end,

    String = "aaa,bbb",
    Result = DecodeRow_1(String),
    ?assertEqual({["aaa", "bbb"], []}, Result).

decode_row_01_test() ->
    Result = decode_row("aaa,bbb"),
    %?debugFmt("~p~n", [Result]),
    ?assertEqual({["aaa", "bbb"], []}, Result).

decode_rows_01_test() ->
    Decode_rowsFun =
        fun Decode_rows(String) ->
                case decode_row(String) of
                    {Row, ""} ->
                        %?debugFmt("Function Result = ~p~n", [Row]),
                        Row;
                    {Row, Rest} ->
                        %?debugFmt("Current Value = ~p~n", [Row]),
                        [Row, Decode_rows(Rest)]
                end
        end,
    _Result = Decode_rowsFun("aaa,bbb,ccc\r\nzzz,yyy,xxx").

    %?debugFmt("Main Result = ~p~n", [_Result]).

decode_rows_02_test() ->
    Decode_rowsFun =
        fun Decode_rows(String) ->
                case decode_row(String) of
                    {Row, ""} ->
                        %?debugFmt("Function Result = ~p~n", [Row]),
                        [Row];
                    {Row, Rest} ->
                        %?debugFmt("Current Value = ~p~n", [Row]),
                        [Row | Decode_rows(Rest)]
                end
        end,
    Result = Decode_rowsFun("aaa,bbb,ccc\r\nzzz,yyy,xxx\r\nzzz,yyy,xxx").

        %?debugFmt("Main Result = ~p~n", [Result]).

decode_rows_03_test() ->
    CSV = "aaa,bbb,ccc\r\nzzz,yyy,xxx",
    Rows = decode_rows(CSV),
    ?assertEqual([["aaa", "bbb", "ccc"], ["zzz", "yyy", "xxx"]], Rows).

decode_rows_03_01_test() ->
    CSV = "aaa,bbb,ccc\r\nzzz,yyy,xxx\r\n",
    Rows = decode_rows(CSV),
    ?assertEqual([["aaa", "bbb", "ccc"], ["zzz", "yyy", "xxx"]], Rows).

decode_rows_03_02_test() ->
    CSV = "\"ab\r\nc1\"",
    Rows = decode_rows(CSV),
    ?assertEqual([["\"ab\r\nc1\""]], Rows).

decode_rows_03_03_test() ->
    CSV = "\"Joe\r\nArmstrong\"\r\nErlang",
    Rows = decode_rows(CSV),
    ?assertEqual([["\"Joe\r\nArmstrong\""], ["Erlang"]], Rows).

decode_rows_03_04_test() ->
    CSV = "\"Joe\r\nArmstrong\"\r\n\"Fred\r\nHebert\"",
    Rows = decode_rows(CSV),
    ?assertEqual([["\"Joe\r\nArmstrong\""], ["\"Fred\r\nHebert\""]], Rows).

decode_header_01_test() ->
    Result = decode_header("aaa,bbb,ccc\r\nzzz,yyy,xxx"),
    ?assertEqual({["aaa", "bbb", "ccc"], "zzz,yyy,xxx"}, Result).

decode_header_01_01_test() ->
    Result = decode_header("aaa,bbb,ccc\r\nzzz,yyy,xxx\r\n"),
    ?assertEqual({["aaa", "bbb", "ccc"], "zzz,yyy,xxx\r\n"}, Result).

decode_01_test() ->
    Result = decode(""),
    ?assertEqual([], Result).

decode_02_test() ->
    CSV = "aaa,bbb,ccc\r\nzzz,yyy,xxx",
    {Headers, Rest} = decode_header(CSV),
    Rows = decode_rows(Rest),
    ?assertEqual(["aaa", "bbb", "ccc"], Headers),
    ?assertEqual([["zzz", "yyy", "xxx"]], Rows).

decode_02_01_test() ->
    CSV = "aaa,bbb,ccc\r\nzzz,yyy,xxx\r\n",
    {Headers, Rest} = decode_header(CSV),
    Rows = decode_rows(Rest),
    ?assertEqual(["aaa", "bbb", "ccc"], Headers),
    ?assertEqual([["zzz", "yyy", "xxx"]], Rows).

decode_03_test() ->
    CSV = "aaa,bbb,ccc\r\nzzz,yyy,xxx\r\nzzz,yyy,xxx",
    {Headers, Rest} = decode_header(CSV),
    Rows = decode_rows(Rest),
    ?assertEqual(["aaa", "bbb", "ccc"], Headers),
    ?assertEqual([["zzz", "yyy", "xxx"], ["zzz", "yyy", "xxx"]], Rows).

decode_03_01_test() ->
    CSV = "aaa,bbb,ccc\r\nzzz,yyy,xxx\r\nzzz,yyy,xxx\r\n",
    {Headers, Rest} = decode_header(CSV),
    Rows = decode_rows(Rest),
    ?assertEqual(["aaa", "bbb", "ccc"], Headers),
    ?assertEqual([["zzz", "yyy", "xxx"], ["zzz", "yyy", "xxx"]], Rows).

decode_04_test() ->
    CSV = "aaa,bbb,ccc\r\nzzz,yyy,xxx\r\nzzz,yyy,xxx",
    {Headers, Rest} = decode_header(CSV),
    Rows = decode_rows(Rest),
    Zip = lists:flatten([lists:zip(Headers, Row) || Row <- Rows]),

    ?assertEqual([{"aaa", "zzz"},
                  {"bbb", "yyy"},
                  {"ccc", "xxx"},
                  {"aaa", "zzz"},
                  {"bbb", "yyy"},
                  {"ccc", "xxx"}],
                 Zip).

decode_04_01_test() ->
    CSV = "aaa,bbb,ccc\r\nzzz,yyy,xxx\r\nzzz,yyy,xxx\r\n",
    {Headers, Rest} = decode_header(CSV),
    Rows = decode_rows(Rest),
    Zip = lists:flatten([lists:zip(Headers, Row) || Row <- Rows]),

    ?assertEqual([{"aaa", "zzz"},
                  {"bbb", "yyy"},
                  {"ccc", "xxx"},
                  {"aaa", "zzz"},
                  {"bbb", "yyy"},
                  {"ccc", "xxx"}],
                 Zip).

%%%%%%%%%%%%%%%%
%%% Encoding %%%
%%%%%%%%%%%%%%%%

get_keys_01_test() ->
    TupleList =
        [{"aaa", "zzz"},
         {"bbb", "yyy"},
         {"ccc", "xxx"},
         {"aaa", "zzz"},
         {"bbb", "yyy"},
         {"ccc", "xxx"}],
    UsortedKeys = proplists:get_keys(TupleList),
    SortedKeys = lists:sort(UsortedKeys),
    ?assertEqual(["aaa", "bbb", "ccc"], SortedKeys).

    %?debugFmt("Keys = ~p~n", [Keys]).

get_keys_02_test() ->
    TupleList =
        [{"aaa", "zzz"},
         {"bbb", "yyy"},
         {"ccc", "xxx"},
         {"aaa", "zzz"},
         {"bbb", "yyy"},
         {"ccc", "xxx"}],

    Keys = get_csv_keys(TupleList),
    ?assertEqual("aaa,bbb,ccc", Keys).

    %?debugFmt("Keys = ~p~n", [Keys]).

get_values_01_test() ->
    TupleList =
        [{"aaa", "zzz"},
         {"bbb", "yyy"},
         {"ccc", "xxx"},
         {"aaa", "zzz"},
         {"bbb", "yyy"},
         {"ccc", "xxx"}],
    Keys = get_csv_keys(TupleList),
    AllValuesList =
        [[[Values] || Values <- proplists:get_all_values(Key, TupleList)]
         || Key <- Keys],
    AllValuesList.

    %?debugFmt("AllValuesList = ~p~n", [AllValuesList]).

divide_list_01_test() ->
    Result = divide_list(1, [1, 1]),
    ?assertEqual([[1], [1]], Result).

divide_list_02_test() ->
    Result = divide_list(2, [1, 2, 1, 2]),
    ?assertEqual([[1, 2], [1, 2]], Result).

divide_list_03_test() ->
    Result = divide_list(3, [1, 2, 3, 1, 2, 3]),
    ?assertEqual([[1, 2, 3], [1, 2, 3]], Result).

divide_list_04_test() ->
    List = ["zzz", "yyy", "xxx", "zzz", "yyy", "xxx"],
    Result = divide_list(3, List),
    ?assertEqual([["zzz", "yyy", "xxx"], ["zzz", "yyy", "xxx"]], Result).

get_values_02_test() ->
    TupleList =
        [{"aaa", "zzz"},
         {"bbb", "yyy"},
         {"ccc", "xxx"},
         {"aaa", "zzz"},
         {"bbb", "yyy"},
         {"ccc", "xxx"}],

    AllValuesList = lists:map(fun(Tuple) -> element(2, Tuple) end, TupleList),
    AllEscapedValuesList =
        lists:map(fun(Value) -> escape(Value) end, AllValuesList),
    DividedLists = divide_list(3, AllEscapedValuesList),
    DividedListsWithRowEnd =
        lists:map(fun(ListItem) ->
                     JoinedList = lists:join(",", ListItem),
                     lists:append(JoinedList, "\r\n")
                  end,
                  DividedLists),

    OneString = lists:flatten(DividedListsWithRowEnd),
    ?assertEqual("zzz,yyy,xxx\r\nzzz,yyy,xxx\r\n", OneString).

    %?debugFmt("AllEscapedValuesList = ~p~n", [AllEscapedValuesList]),
    %?debugFmt("DividedListsWithRowEnd = ~p~n", [DividedListsWithRowEnd]).
    %?debugFmt("OneString = ~p~n", [OneString]).

get_csv_values_01_test() ->
    TupleList =
        [{"aaa", "zzz"},
         {"bbb", "yyy"},
         {"ccc", "xxx"},
         {"aaa", "zzz"},
         {"bbb", "yyy"},
         {"ccc", "xxx"}],
    OneString = get_csv_values(TupleList),
    ?assertEqual("zzz,yyy,xxx\r\nzzz,yyy,xxx\r\n", OneString).

emulate_encode_01_test() ->
    TupleList =
        [{"aaa", "zzz"},
         {"bbb", "yyy"},
         {"ccc", "xxx"},
         {"aaa", "zzz"},
         {"bbb", "yyy"},
         {"ccc", "xxx"}],

    Keys = get_csv_keys(TupleList),
    Values = get_csv_values(TupleList),
    Result = lists:flatten([Keys, "\r\n", Values]),
    %?debugFmt("Ecode = ~p~n", [Result]).
    ?assertEqual("aaa,bbb,ccc\r\nzzz,yyy,xxx\r\nzzz,yyy,xxx\r\n", Result).

encode_01_test() ->
    TupleList =
        [{"aaa", "zzz"},
         {"bbb", "yyy"},
         {"ccc", "xxx"},
         {"aaa", "zzz"},
         {"bbb", "yyy"},
         {"ccc", "xxx"}],

    Result = encode(TupleList),
    %?debugFmt("Ecode = ~p~n", [Result]).
    ?assertEqual("aaa,bbb,ccc\r\nzzz,yyy,xxx\r\nzzz,yyy,xxx\r\n", Result).

do_escape_01_test() ->
    String = "b\"\"bb",
    Result = do_escape(String),
    ?assertEqual("b\"\"\"\"bb", Result).

-endif.
