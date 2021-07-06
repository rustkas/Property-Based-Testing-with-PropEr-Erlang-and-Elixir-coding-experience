-module(bday_csv_tuple).

-export([encode/1, decode/1]).

%% @doc Take a list of maps with the same keys and transform them
%% into a string that is valid CSV, with a header.
-spec encode([{string(), string()}]) -> string().
encode([]) ->
    "";
encode(DeepTupleList) ->
    Keys = get_csv_keys(DeepTupleList),
    Values = get_csv_values(DeepTupleList),
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
    ZipList = [lists:zip(Headers, Row) || Row <- Rows],
    TupleList = lists:flatten(ZipList).

%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%

%% @private divide list to two
divide_list(N, List) ->
    divide_list(N, List, []).

%% @private divide list to two
divide_list(N, [], Acc) ->
    Acc;
divide_list(N, List, Acc) ->
    {NewSubList, NewList} = lists:split(N, List),
    divide_list(N, NewList, [NewSubList | Acc]).

%% @private return sorted keys
-spec get_csv_keys(DeepTupleList) -> SplitString
    when DeepTupleList :: [[{string(), string()}]],
         SplitString :: string().
get_csv_keys(DeepTupleList) ->
    FirstList = hd(DeepTupleList),
    Keys = lists:map(fun(Elem) -> element(1, Elem) end, FirstList),
    JoinedList = lists:join(",", Keys),
    KeysString = lists:flatten(JoinedList),
    KeysString.

%% @private return string of values
-spec get_csv_values(DeepTupleList) -> SplitString
    when DeepTupleList :: [[{string(), string()}]],
         SplitString :: string().
get_csv_values(DeepTupleList) ->
    ListLists =
        [lists:map(fun(Elem) -> element(2, Elem) end, TupleList)
         || TupleList <- DeepTupleList],
    ValuesLists = [lists:join(",", List) || List <- ListLists],
    JoinedList = lists:join("\r\n", ValuesLists),
    ValuesString = lists:flatten(JoinedList),
    ValuesString.

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
%%% Encoding %%%
%%%%%%%%%%%%%%%%

-include("encode.tests").

%%%%%%%%%%%%%%%%
%%% Decoding %%%
%%%%%%%%%%%%%%%%

-include("decode.tests").

-endif.
