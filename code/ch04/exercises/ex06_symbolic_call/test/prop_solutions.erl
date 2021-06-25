-module(prop_solutions).

-compile([{nowarn_unused_function, [{ file_open, 2}, {file_write, 2}]}]).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_make_tmp_file() ->
    ?FORALL(_TempFilePath, make_tmp_file(),
        begin
		    %io:format("~p~n",[TempFilePath]),
            %ok = file:delete(TempFilePath),
            %TempDir = filename:dirname(TempFilePath),
			%ok = file:del_dir_r(TempDir),
			%MainTmpDir = filename:dirname(TempDir),
			%ok = file:del_dir(MainTmpDir),
			true			
        end).


%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

mktemp() ->
    mktemp("tmp").

-spec mktemp(Prefix) -> Result
   when Prefix   :: string(),
        Result   :: TempFile  :: file:filename().
		
mktemp(Prefix) ->
    Rand = integer_to_list(binary:decode_unsigned(crypto:strong_rand_bytes(8)), 36),
	TempDir = filename:basedir(user_cache, Prefix),
	os:cmd("mkdir " ++ "\"" ++ TempDir ++ "\""),
			
	TempFilePath = filename:join(TempDir, Rand),
	TempFilePath.


%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
make_tmp_file()->
	TmpFile = mktemp(),
	ok = file:write_file(TmpFile, <<>>),
	%io:format("Before TmpFile = ~p~n",[TmpFile]),
	file(TmpFile)
	%io:format("After TmpFile = ~p~n",[TmpFile]),
	%TmpFile,.
	.

file_open(Name, Opts) ->
    io:format("File Open. Name = ~p, Opts = ~p~n",[Name, Opts]),
    {ok, Fd} = file:open(Name, Opts),
    %% ensure the file is refreshed on each test run
    file:truncate(Fd),
    Fd.

file_write(Fd, Data) ->
    io:format("File Write. Fd = ~p, Data = ~p~n",[Fd, Data]),
    ok = file:write(Fd, Data),
    Fd.

file(Name) ->
    ?SIZED(
       Size,
       lines(Size, {call, ?MODULE, file_open, [Name, [read,write,raw]]})
    ).

lines(Size, Fd) ->
    if Size =< 1 -> Fd
     ; Size > 1 -> lines(Size-1, {call, ?MODULE, file_write, [Fd,bin()]})
    end.	
	

bin() ->
 non_empty(string()).

