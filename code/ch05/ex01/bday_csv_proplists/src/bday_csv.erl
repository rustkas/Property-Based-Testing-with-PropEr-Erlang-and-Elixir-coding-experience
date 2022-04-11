-module(bday_csv).

-export([encode/1, decode/1, decode/2]).

%% @doc Take a list of maps with the same keys and transform them
%% into a string that is valid CSV, with a header.

-spec encode([{HeaderItem, DataItem}]) -> CSV
    when HeaderItem :: string(),
         DataItem :: string(),
         CSV :: string().
encode([]) ->
    "";
encode(DeepTupleList) ->

	Regex = "(?<=\")[0-9a-zA-Z:;<=>?@ !#$%&'\"\r\n()*+-./[\\]^_`{|}~]*(?=\")",
    {ok, MP} = re:compile(Regex),
	
    Keys = get_csv_keys(DeepTupleList,MP),
    Values = get_csv_values(DeepTupleList,MP),
    Result = lists:flatten([Keys, "\r\n", Values]),
    Result.


%% @doc Take a string that represents a valid CSV data dump
%% and turn it into a list of maps with the header entries as keys.
%% @param CSV input CSV string
%% @equiv decode(CSV, true)

-spec decode(CSV) -> Result when
    CSV :: string(),
	Result :: [[{HeaderItem, DataItem}]] | [Row] | Row,
	HeaderItem :: string(),
    DataItem :: string(),
	Row :: [RowItem],
	RowItem :: string().
decode(CSV) ->
    decode(CSV, true).

%% @doc Take a string that represents a valid CSV data dump
%% and turn it into a list of maps with the header entries as keys.
%% @param CSV input CSV string
%% @param IsHeader is an optional header line appearing as the first line

-spec decode(CSV, IsHeader) -> Result when
    CSV :: string(),
	IsHeader :: boolean(),
	Result :: [[{HeaderItem, DataItem}]] | [Row] | Row,
	HeaderItem :: string(),
    DataItem :: string(),
	Row :: [RowItem],
	RowItem :: string().
decode("", _IsHeader) ->
    [];
decode(CSV, IsHeader) ->
    
	Result = case IsHeader of 
		false ->
			Rows = decode_rows(CSV),
			Rows;
		true -> 
			{Header, Rest} = decode_header(CSV),
			HeadherLength = length(Header),
			Rows = decode_rows(Rest,true),
			ZipList = [lists:zip(Header, Row) || Row <- Rows, HeadherLength == length(Row)],
			ZipList
	end,
	Result.


%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%%   Encode   %%%
%%%%%%%%%%%%%%%%%%

%% @private 
%% @doc return sorted keys

-spec get_csv_keys(DeepTupleList,MP) -> SplitString when 
	DeepTupleList :: [[{Key, Value}]],
    MP :: re:mp(),
	Key :: string(),
	Value :: string(),
	SplitString :: string().
get_csv_keys(DeepTupleList,MP) ->
    FirstList = hd(DeepTupleList),
    Keys =
        lists:map(fun(Elem) ->
                     Key = element(1, Elem),
                     EscapedKey = sanitize(Key, MP),
                     EscapedKey
                  end,
                  FirstList),
    JoinedList = lists:join(",", Keys),
    KeysString = lists:flatten(JoinedList),
    KeysString.

%% @private 
%% @doc return string of values

-spec get_csv_values(DeepTupleList,MP) -> SplitString when 
	DeepTupleList :: [[{Key, Value}]],
    MP :: re:mp(),
	Key :: string(),
	Value :: string(),
	SplitString :: string().
get_csv_values(DeepTupleList,MP) ->
	ListLists =
        [lists:map(fun(Elem) ->
                      Value = element(2, Elem),
                      EscapedValue = sanitize(Value, MP),
                      EscapedValue
                   end,
                   TupleList)
         || TupleList <- DeepTupleList],
    ValuesLists = [lists:join(",", List) || List <- ListLists],
    JoinedList = lists:join("\r\n", ValuesLists),
    ValuesString = lists:flatten(JoinedList),
    ValuesString.

%% @private 
%% @doc Analyze, repace double quote to one quote and generate escape string (if necessary)
%% @returns a possibly escaped (if necessary) field or name

-spec sanitize(InputString, MP) -> Result when
 InputString :: string(),
 MP :: re:mp(),
 Result :: string().
sanitize(InputValue, MP) ->
	RegexResult = re:run(InputValue, MP, [{capture, all, list}]),
	Result = case RegexResult of
		nomatch ->
			InputValue;
		{match, [Captured]} ->
			SearchPattern = "\"",
			SanitizedString = string:replace(Captured, SearchPattern, "\"\"", all),
			NewList = [$"] ++ unicode:characters_to_list(SanitizedString) ++ [$"],
			NewList
	end,
	Result.
	
%%%%%%%%%%%%%%%%%%
%%%   Decode   %%%
%%%%%%%%%%%%%%%%%%


%% @private 
%% @doc Decode the entire header line, returning all names in order and rest of that string.
%% @equiv decode_row(String)

-spec decode_header(InputString) -> Result when
 InputString :: string(),
 Result :: {Header, Rest},
 Header :: [Row] | Row,
 Row :: [RowItem],
 RowItem :: string(),
 Rest :: string().
decode_header(String) ->
    decode_row(String).

%% @private 
%% @doc Decode all rows into a list.
%% @param HasMore is result has more than one field
%% @returns a list of lists or a list
%% @equiv decode_rows(InputString, false)

-spec decode_rows(InputString) -> Result when
 InputString :: string(),
 Result :: [Row] | Row,
 Row :: [RowItem],
 RowItem :: string().
decode_rows(String) ->
    decode_rows(String, false).

%% @private 
%% @doc Decode all rows into a list.
%% @param HasMore is result has more than one field
%% @returns a list of lists or a list

-spec decode_rows(InputString, HasMore) -> Result when
 InputString :: string(),
 HasMore :: boolean(),
 Result :: [Row] | Row,
 Row :: [RowItem],
 RowItem :: string().
decode_rows(String, HasMore) ->
    case decode_row(String) of
        {Row, ""} ->
            if HasMore == true ->
                   [Row];
               HasMore == false ->
                   Row
            end;
        {Row, Rest} ->
            [Row | decode_rows(Rest, true)]
    end.

%% @private 
%% @doc Decode an entire row, with all values in order
%% @equiv decode_row(String, [])

-spec decode_row(InputString) -> ResultTuple when
 InputString :: string(),
 ResultTuple :: {Row, Rest},
 Row :: [RowItem],
 Rest :: string(),
 RowItem :: string(). 
decode_row(String) ->
    decode_row(String, []).

%% @private 
%% @doc Decode an entire row, with all values in order

-spec decode_row(InputString, Accumulator) -> ResultTuple when
 InputString :: string(),
 Accumulator :: [string()],
 ResultTuple :: {Row, Rest},
 Row :: [RowItem],
 Rest :: string(),
 RowItem :: string().
decode_row(String, Acc) ->
    case decode_field(String) of
        {go_on, Field, Rest} ->
            decode_row(Rest, Acc ++ [Field]);
        {done, Field, Rest} ->
            Result = Acc ++ [Field],
            {Result, Rest}
    end.

%% @private 
%% @doc Decode a field; redirects to decoding quoted or unquoted text
%% `go_on` returns when `Input` has more data at the right
%% `done` returns when `Input` has no data at the right

-spec decode_field(InputString) -> ResultTuple when
 InputString :: string(),
 ResultTuple :: {go_on, FiedValue, Rest} | {done, FiedValue, Rest},
 FiedValue :: string(),
 Rest :: string().
decode_field([Head | _Rest] = Input) when Head =:= $" ->
    decode_quoted(Input);
decode_field(String) ->
    decode_unquoted(String).

%% @private 
%% @doc Decode a quoted string

-spec decode_quoted(InputString) -> ResultTuple when
 InputString :: [Head | Rest],
 Head :: $",
 Rest :: [char()],
 ResultTuple :: {go_on, ParsedString, Rest} | {done, ParsedString, Rest},
 ParsedString :: nonempty_string(),
 Rest :: string().
decode_quoted([Head | Rest]) when Head =:= $" ->
    decode_quoted(Rest, [Head]).

-spec decode_quoted(InputString, [Character]) -> ResultTuple when
 InputString :: string(),
 Character :: char(),
 ResultTuple :: {go_on | done, ParsedString, Rest},
 ParsedString :: nonempty_string(),
 Rest :: string().
decode_quoted([$"], Acc) ->
	{done, Acc ++ [$"], ""};
decode_quoted([$", $\r, $\n | Rest], Acc) ->
	{done, Acc ++ [$"], Rest};
decode_quoted([$",$, | Rest], Acc) ->
    {go_on, Acc ++ [$"], Rest};
decode_quoted([$",$" | Rest], Acc) ->
	decode_quoted(Rest, Acc ++ [$"]);
decode_quoted([Char | Rest], Acc) ->
    decode_quoted(Rest, Acc ++ [Char]).	
	
%% @private 
%% @doc Decode an unquoted string

-spec decode_unquoted(InputString) -> ResultTuple when
 InputString :: string(),
 ResultTuple :: {go_on, ParsedString, Rest} | {done, ParsedString, Rest},
 ParsedString :: nonempty_string(),
 Rest :: string().
decode_unquoted(String) ->
    decode_unquoted(String, []).

%% @private 
%% @doc Decode an unquoted string
% `go_on` returns when we have more data at the right
% `done` returns when we have no data at the right

-spec decode_unquoted(InputString, Accumulator) -> ResultTuple when
 InputString :: string(),
 Accumulator :: string(),
 ResultTuple :: {go_on, ParsedString, Rest} | {done, ParsedString, Rest},
 ParsedString :: nonempty_string(),
 Rest :: string().
decode_unquoted([], Acc) ->
    {done, Acc, ""};
decode_unquoted([$\r, $\n | Rest], Acc) ->
    {done, Acc, Rest};
decode_unquoted([$, | Rest], Acc) ->
    {go_on, Acc, Rest};
decode_unquoted([Char | Rest], Acc) ->
    decode_unquoted(Rest, Acc ++ [Char]).


%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%
%%% Encoding %%%
%%%%%%%%%%%%%%%%

-include("../test/tests/encode.tests").

%%%%%%%%%%%%%%%%
%%% Decoding %%%
%%%%%%%%%%%%%%%%

-include("../test/tests/decode.tests").

-endif.	