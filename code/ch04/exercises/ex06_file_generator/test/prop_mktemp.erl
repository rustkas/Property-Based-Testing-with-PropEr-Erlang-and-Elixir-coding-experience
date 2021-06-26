-module(prop_mktemp).

-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_rand() ->
    ?FORALL(_Tmp,
            boolean(),
            begin
                _Rand =
                    integer_to_list(binary:decode_unsigned(
                                        crypto:strong_rand_bytes(8)),
                                    36),
                %io:format("~p~n",[_Rand]),
                true
            end).

prop_basedir() ->
    ?FORALL(_Tmp,
            boolean(),
            begin
                Prefix = "tmp",
                _TempDir = filename:basedir(user_cache, Prefix),
                %io:format("~p~n",[_TempDir]),
                true
            end).

prop_tmp_file_name() ->
    ?FORALL(_Tmp,
            boolean(),
            begin
                Rand =
                    integer_to_list(binary:decode_unsigned(
                                        crypto:strong_rand_bytes(8)),
                                    36),
                Prefix = "tmp",
                TempDir = filename:basedir(user_cache, Prefix),
                _TempFileName = filename:join(TempDir, Rand),
                %io:format("~p~n",[_TempFileName]),
                true
            end).

prop_tmp_ensure_dir() ->
    ?FORALL(_Tmp,
            boolean(),
            begin
                Prefix = "tmp",
                TempDir = filename:basedir(user_cache, Prefix),
                MainTmpDir = filename:dirname(TempDir),
                os:cmd("mkdir " ++ "\"" ++ TempDir ++ "\""),

                true = filelib:is_dir(TempDir),
                ok = file:del_dir_r(TempDir),
                ok = file:del_dir(MainTmpDir),
                true
            end).

prop_tmp_write_to_file() ->
    ?FORALL(_Tmp,
            boolean(),
            begin
                Rand =
                    integer_to_list(binary:decode_unsigned(
                                        crypto:strong_rand_bytes(8)),
                                    36),
                Prefix = "tmp",
                TempDir = filename:basedir(user_cache, Prefix),
                os:cmd("mkdir " ++ "\"" ++ TempDir ++ "\""),

                TempFilePath = filename:join(TempDir, Rand),
                {ok, IoDevice} =
                    file:open(TempFilePath, [write, read, raw, {read_ahead, 64 * 1024}]),
                ok = file:write_file(TempFilePath, <<>>),

                TempDir = filename:dirname(TempFilePath),

                ok = file:close(IoDevice),
                ok = file:delete(TempFilePath),
                ok = file:del_dir_r(TempDir),
                MainTmpDir = filename:dirname(TempDir),
                ok = file:del_dir(MainTmpDir),
                true
            end).

% simplify tmp file creation
prop_tmp_write_to_file2() ->
    ?FORALL(_Tmp,
            boolean(),
            begin
                Rand =
                    integer_to_list(binary:decode_unsigned(
                                        crypto:strong_rand_bytes(8)),
                                    36),
                Prefix = "tmp",
                TempDir = filename:basedir(user_cache, Prefix),
                os:cmd("mkdir " ++ "\"" ++ TempDir ++ "\""),

                TempFilePath = filename:join(TempDir, Rand),
                ok = file:write_file(TempFilePath, <<>>),

                ok = file:delete(TempFilePath),
                ok = file:del_dir_r(TempDir),
                MainTmpDir = filename:dirname(TempDir),
                ok = file:del_dir(MainTmpDir),
                true
            end).
