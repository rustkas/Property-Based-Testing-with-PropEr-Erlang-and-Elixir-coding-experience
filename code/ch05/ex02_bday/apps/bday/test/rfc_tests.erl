-module(rfc_tests).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

rfc_record_per_line_test() ->
    ?assertEqual([#{"aaa" => "zzz",
                    "bbb" => "yyy",
                    "ccc" => "xxx"}],
                 bday_csv:decode("aaa,bbb,ccc\r\nzzz,yyy,xxx\r\n")).

rfc_optional_trailing_crlf_test() ->
    ?assertEqual([#{"aaa" => "zzz",
                    "bbb" => "yyy",
                    "ccc" => "xxx"}],
                 bday_csv:decode("aaa,bbb,ccc\r\nzzz,yyy,xxx")).

rfc_double_quote_test() ->
    ?assertEqual([#{"aaa" => "zzz",
                    "bbb" => "yyy",
                    "ccc" => "xxx"}],
                 bday_csv:decode("\"aaa\",\"bbb\",\"ccc\"\r\nzzz,yyy,xxx")).

rfc_crlf_escape_test() ->
    ?assertEqual([#{"aaa" => "zzz",
                    "b\r\nbb" => "yyy",
                    "ccc" => "xxx"}],
                 bday_csv:decode("\"aaa\",\"b\r\nbb\",\"ccc\"\r\nzzz,yyy,xxx")).

rfc_double_quote_escape_test() ->
    %% Since we decided headers are mandatory, this test adds a line
    %% with empty values (CLRF,,) to the example from the RFC.
    ?assertEqual([#{"aaa" => "",
                    "b\"bb" => "",
                    "ccc" => ""}],
                 bday_csv:decode("\"aaa\",\"b\"\"bb\",\"ccc\"\r\n,,")).

%% @doc this counterexample is taken literally from the RFC and cannot
%% work with the current implementation because maps have no dupe keys
dupe_keys_unsupported_test() ->
    CSV = "field_name,field_name,field_name\r\n"
          "aaa,bbb,ccc\r\n"
          "zzz,yyy,xxx\r\n",
    [Map1, Map2] = bday_csv:decode(CSV),
    ?debugFmt("Map1 = ~p~nMap2 = ~p~n", [Map1, Map2]),
    %?debugFmt("Map2 = ~p~n",[Map2]),
    ?assertEqual(1, length(maps:keys(Map1))),
    ?assertEqual(1, length(maps:keys(Map2))),
    ?assertMatch(#{"field_name" := _}, Map1),
    ?assertMatch(#{"field_name" := _}, Map2).

-endif.
