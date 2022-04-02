-module(bday_csv_tuple_regex).

-export([encode/1, decode/1]).


%% @doc Take a list of maps with the same keys and transform them
%% into a string that is valid CSV, with a header.
-spec encode([{HeaderItem, DataItem}]) -> CSV
    when HeaderItem :: string(),
         DataItem :: string(),
         CSV :: string().
encode([]) ->
    "";
encode(DeepTupleList) ->
    Keys = get_csv_keys(DeepTupleList),
    Values = get_csv_values(DeepTupleList),
    Result = lists:flatten([Keys, "\r\n", Values]),
    Result.

%% @doc Take a string that represents a valid CSV data dump
%% and turn it into a list of maps with the header entries as keys
-spec decode(CSV) -> [[{HeaderItem, DataItem}]]
    when CSV :: string(),
         HeaderItem :: string(),
         DataItem :: string().
decode("") ->
    [];
decode(CSV) ->
    %io:format("DECODE: ~p~n", [CSV]),
    Text = CSV,
	
	TunedText = tune_string(Text),
	SplittedText = split_text(TunedText),
    SplitMP = build_split_rows_mp(),
	RemoveUselessDoubleQuoteMP = build_remove_double_quote_mp(),
	
	HeaderList = get_header(SplittedText,SplitMP,RemoveUselessDoubleQuoteMP),
    DataList = get_data(SplittedText,SplitMP,RemoveUselessDoubleQuoteMP),
	HeaderListLength = length(HeaderList),
	
	case length(DataList) > 0 of 
		    false -> throw({badarg, Text});
			_-> true
	end,		
	
	lists:foreach(fun(DataItem)->
		DataListLength = length(DataItem),
		
		case DataListLength == HeaderListLength of 
			false -> throw({badarg, io_lib:format("Wrong data = ~p, Length =~p~n",[DataItem, DataListLength])});
			_ -> true
		end
	end, DataList),
	
	ZipList = [lists:zip(HeaderList, Row) || Row <- DataList],
    Result = ZipList,
    Result.

%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%

%% @private return sorted keys
-spec get_csv_keys(DeepTupleList) -> SplitString
    when DeepTupleList :: [[{string(), string()}]],
         SplitString :: string().
get_csv_keys(DeepTupleList) ->
    FirstList = hd(DeepTupleList),
    Keys =
        lists:map(fun(Elem) ->
                     Key = element(1, Elem),
                     EscapedKey = escape(Key),
                     EscapedKey
                  end,
                  FirstList),
    JoinedList = lists:join(",", Keys),
    KeysString = lists:flatten(JoinedList),
    KeysString.

%% @private return string of values
-spec get_csv_values(DeepTupleList) -> SplitString
    when DeepTupleList :: [[{string(), string()}]],
         SplitString :: string().
get_csv_values(DeepTupleList) ->
    ListLists =
        [lists:map(fun(Elem) ->
                      Value = element(2, Elem),
                      EscapedValue = escape(Value),
                      EscapedValue
                   end,
                   TupleList)
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
do_escape([$" | Str]) ->
    [$", $" | do_escape(Str)];
do_escape([Char | Rest]) ->
    [Char | do_escape(Rest)].

get_mp(Pattern) ->
    {ok, MP} = re:compile(Pattern),
    MP.

get_mp(Pattern,Options) ->
    {ok, MP} = re:compile(Pattern,Options),
    MP.

build_split_rows_mp()->
{ok, SplitRowMP} = re:compile("
(?<=^|,)
(
\"                      # opening double quote	
.*?                     # quoted sequence if double quote is present. lazy quantifier
\"                      # closing double quote
|[^,\"\r\n]*?
)
(?=$|,)
",
	[extended,multiline,dotall]),
SplitRowMP.

	
build_remove_double_quote_mp()->
	{ok, RemoveUselessDoubleQuoteMP} =
        re:compile("
    (^[\"])         # opening double quote
    (.*?)           # any quited characters
    [\"]$           # closing double quote
    ",
                   [extended,multiline,dotall]),
	RemoveUselessDoubleQuoteMP.		

remove_inner_double_quote(Text)->
    {ok, WithoutInnerDoubleQuoteMP} =
        re:compile("
([\"])    # opening double quote
([^\"]*?) # zero or many characteres. lazy quantifier
([\"])    # double quote
([\"])    # double quote
([^\"]*?) # zero or many characteres. lazy quantifier
([\"])    # closing double quote
",
                   [extended]),

    WithoutInnerDoubleQuoteText =
        re:replace(Text,
                   WithoutInnerDoubleQuoteMP,
                   "\\1\\2\\4\\5\\6",
                   [global, {return, list}]),
	WithoutInnerDoubleQuoteText.			   

remove_useless_double_quote(RemoveUselessDoubleQuoteMP, StringList) ->
    NewStringList =
        lists:map(fun(DoubleQuoteText) ->
                     WithoutUselessDoubleQuoteText =
                         re:replace(DoubleQuoteText,
                                    RemoveUselessDoubleQuoteMP,
                                    "\\2",
                                    [{return, list}]),
                     WithoutUselessDoubleQuoteText
                  end,
                  StringList),
    NewStringList.

tune_string(Text)->
    WithoutInnerDoubleQuoteText = remove_inner_double_quote(Text),
    % remove last CRLF
	TunedText = re:replace(WithoutInnerDoubleQuoteText, get_mp("\\r\\n$",[multiline]), "", [{return, list}]),
    TunedText.

split_text(Text)->
    {ok, SplitMP} =
        re:compile("
(^|,)	
([\"]?)?                         # optional opening double quote	
[^,\"\r\n]*?
(?(2)[,\"\r\n]*?|[^,\"\r\n]*?)*? # quoted sequence if double quote is present. lazy quantifier
                                 # zero or many characteres. lazy quantifier
(?(2)[\"])                       # optional closing double quote
(?=\\Q\r\n\\E)                   # looking ahead expression for CRLF
\\K                              # 'skip all before' \K shorthand
\\Q\r\n\\E                       # CRLF Header/Data delimiter
",
                   [extended,multiline,dotall]),
    StringList = re:split(Text, SplitMP, [{parts, 2},{return, list}]),
	StringList.
	
get_header_string(SplittedText)->
	HeaderString = hd(SplittedText),
    HeaderString.

get_data_string(SplittedText)->
	DataString = hd(lists:reverse(SplittedText)),
    DataString.

split_row(Text,MP)->
    {match,HeaderList} = re:run(Text, MP,[global,{capture,first,list}]),
    RowList = lists:map(fun(Elem) -> hd(Elem) end, HeaderList),
    RowList.

get_header(SplittedText,SplitMP,RemoveUselessDoubleQuoteMP)->
    HeaderString = get_header_string(SplittedText),
    HeaderList = split_row(HeaderString, SplitMP),
	
    TunedHeaderList =
        remove_useless_double_quote(RemoveUselessDoubleQuoteMP, HeaderList),
    TunedHeaderList.

get_data(SplittedText,SplitMP,RemoveUselessDoubleQuoteMP)->
    DataString = get_data_string(SplittedText),
	TrimmedString = string:trim(DataString),

% TODO: split block of data	
	   {ok, DataSplitMP} =
        re:compile("
(^|,)	
([\"]?)?                         # optional opening double quote	
[^,\"\r\n]*?
(?(2)[,\"\r\n]*?|[^,\"\r\n]*?)*? # quoted sequence if double quote is present. lazy quantifier
                                 # zero or many characteres. lazy quantifier
(?(2)[\"])                       # optional closing double quote
(?=\\Q\r\n\\E)                   # looking ahead expression for CRLF
\\K                              # 'skip all before' \K shorthand
(?<=
\\Q\r\n\\E                       # CRLF Header/Data delimiter
([\"]?)?                         # optional opening double quote	
[^,\"\r\n]*?
(?(2)[,\"\r\n]*?|[^,\"\r\n]*?)*? # quoted sequence if double quote is present. lazy quantifier
                                 # zero or many characteres. lazy quantifier
(?(2)[\"])                       # optional closing double quote
)
",
                   [extended,multiline,dotall]),
	
    RowStrings = re:split(TrimmedString, get_mp("\\r\\n"), [{return, list}]),
	
	
    RowsList =
        lists:map(fun(RowElem) ->
					 ItemList = split_row(RowElem, SplitMP),
                     TunedItemList =
                         remove_useless_double_quote(RemoveUselessDoubleQuoteMP, ItemList),
                     TunedItemList
                  end,
                  RowStrings),
    RowsList.

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%
%%% Encoding %%%
%%%%%%%%%%%%%%%%

%-include("tests/tuple/encode.tests").

%%%%%%%%%%%%%%%%
%%% Decoding %%%
%%%%%%%%%%%%%%%%

-include("tests/regex/decode.tests").

-endif.
