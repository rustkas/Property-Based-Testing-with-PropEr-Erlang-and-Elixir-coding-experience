-module(ex06_file_generator).

-export([mktemp/0]).

mktemp() ->
    mktemp("tmp").

-spec mktemp(Prefix) -> Result
    when Prefix :: string(),
         Result :: (TempFile :: file:filename()).
mktemp(Prefix) ->
    Rand =
        integer_to_list(binary:decode_unsigned(
                            crypto:strong_rand_bytes(8)),
                        36),
    TempDir = filename:basedir(user_cache, Prefix),
    os:cmd("mkdir " ++ "\"" ++ TempDir ++ "\""),

    TempFilePath = filename:join(TempDir, Rand),
    %io:format("TempFilePath = ~p~n",[TempFilePath]),
    TempFilePath.
