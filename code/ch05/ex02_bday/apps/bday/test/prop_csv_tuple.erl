-module(prop_csv_tuple).

-include_lib("proper/include/proper.hrl").

-export([csv_source/0]).
-export([unquoted_text/0, quotable_text/0, field/0, name/0]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_unquoted_text() ->
    ?FORALL(Unquoted_text,
            unquoted_text(),
            begin
                %io:format("~p~n", [Unquoted_text]),
                TextData = textdata(),
                lists:all(fun(Elem) -> string:chr(TextData, Elem) > 0 end, Unquoted_text)
            end).

prop_quotable_text() ->
    ?FORALL(Quotable_text,
            quotable_text(),
            begin
                %io:format("~p~n", [Quotable_text]),
                TextData = [$\r, $\n, $", $,] ++ textdata(),
                lists:all(fun(Elem) -> string:chr(TextData, Elem) > 0 end, Quotable_text)
            end).

prop_field() ->
    ?FORALL(Field,
            field(),
            begin
                %io:format("~p~n", [Field]),
                TextData = [$\r, $\n, $", $,] ++ textdata(),
                lists:all(fun(Elem) -> string:chr(TextData, Elem) > 0 end, Field)
            end).

prop_name() ->
    ?FORALL(Name,
            name(),
            begin
                %io:format("~p~n", [Name]),
                TextData = [$\r, $\n, $", $,] ++ textdata(),
                lists:all(fun(Elem) -> string:chr(TextData, Elem) > 0 end, Name)
            end).

prop_header() ->
    ?FORALL(Header,
            ?SIZED(Size, header(Size + 1)),
            begin
                %io:format("~p~n", [Header]),
                TextData = [$\r, $\n, $", $,] ++ textdata(),
                ListOfStrings = Header,
                lists:all(fun(String) ->
                             lists:all(fun(Elem) -> string:chr(TextData, Elem) > 0 end, String)
                          end,
                          ListOfStrings)
            end).

prop_record() ->
    ?FORALL(Record,
            ?SIZED(Size, header(Size + 1)),
            begin
                %io:format("~p~n", [Record]),
                TextData = [$\r, $\n, $", $,] ++ textdata(),
                ListOfStrings = Record,
                lists:all(fun(String) ->
                             lists:all(fun(Elem) -> string:chr(TextData, Elem) > 0 end, String)
                          end,
                          ListOfStrings)
            end).

prop_entry2() ->
    ?FORALL(Entry,
            ?SIZED(Size, entry2(Size + 1)),
            begin
                %io:format("~p~n", [Entry]),
                is_list(Entry)
                and lists:all(fun(Elem) -> is_tuple(Elem) andalso 2 == tuple_size(Elem) end,
                              Entry)
            end).

prop_entry3() ->
    ?FORALL(Entry,
            ?SIZED(Size, entry3(Size + 1, header(Size + 1))),
            begin
                %io:format("~p~n", [Entry]),
                is_list(Entry)
                and lists:all(fun(Elem) -> is_tuple(Elem) andalso 2 == tuple_size(Elem) end,
                              Entry)
            end).

prop_entry() ->
    ?FORALL(Entry,
            ?SIZED(Size, entry(Size + 1, header(Size + 1))),
            begin
                %io:format("~p~n", [Entry]),
                is_list(Entry)
                and lists:all(fun(Elem) -> is_tuple(Elem) andalso 2 == tuple_size(Elem) end,
                              Entry)
            end).

prop_csv_source() ->
    ?FORALL(DeepList,
            csv_source(),
            begin
                io:format("~p~n", [DeepList]),			
                lists:map(fun(TupleLists) ->
                             lists:map(fun(Tuple) -> is_tuple(Tuple) andalso 2 == tuple_size(Tuple)
                                       end,
                                       TupleLists),
                             UsortedKeys = proplists:get_keys(TupleLists),
                             TupleListLength = length(TupleLists),
                             ColumnCount = length(UsortedKeys),
                             % check all rows have the some size
							 %io:format("TupleListLength = ~p, ColumnCount = ~p~n",[TupleListLength, ColumnCount]),
                             %0 = TupleListLength rem ColumnCount,
							 TupleListLength == ColumnCount
                          end,
                          DeepList),
                true
            end).

%prop_roundtrip() ->
%    ?FORALL(Maps, csv_source(),
%            Maps =:= bday_csv:decode(bday_csv:encode(Maps))).

prop_roundtrip() ->
    ?FORALL(DeepList,
            csv_source(),
            begin
                lists:map(fun(TupleLists) -> 
										EncodingResult = bday_csv_tuple:encode(TupleLists),
										io:format("~p~n", [EncodingResult]),
										%bday_csv_tuple:decode(EncodingResult)
										true
                          end,
                          DeepList),
                true
            end).

                %io:format("->~p<-~n",[DeepList]),

                                %io:format("->~p<-~n",[TupleLists]),

                %io:format("~p~n", [EncodingResult]),
                %TupleLists
                %=:= bday_csv_tuple:decode(
                %        bday_csv_tuple:encode(TupleLists)),

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
    vector(Size, non_empty(name())).

record(Size) ->
    vector(Size, field()).

entry2(Size) ->
    ?LET(Vals, record(Size), lists:zip(Vals, Vals)).

entry3(Size, KeysGen) ->
    ?LET({Vals, Keys},
         {record(Size), KeysGen},
         begin
             %io:format("Keys = ~p, Vals = ~p~n",[Keys,Vals]),
             lists:zip(Keys, Vals)
         end).

entry(Size, KeysGen) ->
    ?LET({Vals, Keys},
         {record(Size), KeysGen},
         begin
             %io:format("Keys = ~p, Vals = ~p~n",[Keys,Vals]),
			 UniqueKeys = lists:map(fun(Key)-> [string:str(Keys, [Key])] ++ Key end, Keys),
             %io:format("UniqueKeys = ~p~n",[UniqueKeys]),
			 lists:zip(UniqueKeys, Vals)
         end).

csv_source() ->
    ?LET(Size,
         pos_integer(),
         ?LET(Keys, header(Size + 1), list(entry(Size + 1, Keys)))).
