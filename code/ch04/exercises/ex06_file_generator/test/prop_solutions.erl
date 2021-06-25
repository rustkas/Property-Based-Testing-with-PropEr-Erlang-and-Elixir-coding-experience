-module(prop_solutions).
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

prop_rand() ->
    ?FORALL(_Tmp, boolean(),
        begin
            _Rand = integer_to_list(binary:decode_unsigned(crypto:strong_rand_bytes(8)), 36),
			%io:format("~p~n",[_Rand]),
			true
        end).

prop_basedir() ->
    ?FORALL(_Tmp, boolean(),
        begin
			Prefix = "tmp",
			_TempDir = filename:basedir(user_cache, Prefix),
			%io:format("~p~n",[_TempDir]),
			true
        end).

prop_tmp_file_name() ->
    ?FORALL(_Tmp, boolean(),
        begin
            Rand = integer_to_list(binary:decode_unsigned(crypto:strong_rand_bytes(8)), 36),
			Prefix = "tmp",
			TempDir = filename:basedir(user_cache, Prefix),
			_TempFileName = filename:join(TempDir, Rand),
			%io:format("~p~n",[_TempFileName]),
			true
        end).
		
prop_tmp_ensure_dir() ->
    ?FORALL(_Tmp, boolean(),
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
    ?FORALL(_Tmp, boolean(),
        begin
            Rand = integer_to_list(binary:decode_unsigned(crypto:strong_rand_bytes(8)), 36),
			Prefix = "tmp",
			TempDir = filename:basedir(user_cache, Prefix),
			os:cmd("mkdir " ++ "\"" ++ TempDir ++ "\""),
			
			TempFilePath = filename:join(TempDir, Rand),
			{ok, IoDevice} = file:open(TempFilePath,[write,read, raw,{read_ahead,64*1024}]),
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
    ?FORALL(_Tmp, boolean(),
        begin
            Rand = integer_to_list(binary:decode_unsigned(crypto:strong_rand_bytes(8)), 36),
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
       lines(Size, {'$call', ?MODULE, file_open, [Name, [read,write,raw]]})
    ).

lines(Size, Fd) ->
    if Size =< 1 -> Fd
     ; Size > 1 -> lines(Size-1, {call, file_write, [Fd,bin()]})
    end.	
	
file1(Name) ->
     io:format("Name = ~p~n",[Name])
     %?SIZED(Size,put(file_size, Size)),
	 %?SIZED(Size,
	   %begin
	     %put(file_size, Size),
		% io:format("Size = ~p~n",[1]),
		 %Value = int(),
	%	 io:fwrite("Size = ~p~n",[Value])
	     %lines(Size, {call, file_open, [Name, [read,write,raw]]})
	   %end
     %),
	 %io:format("Size = ~p~n",[get(file_size)]),
	 %erase(file_size)
	 .

lines1(Size, Fd) ->
     %io:format("Size = ~p~n",[Size]),
     if Size =< 1 -> Fd
      ; Size > 1 -> lines(Size-1, {call, file_write, [Fd,bin()]})
     end.



bin() ->
 non_empty(string()).

