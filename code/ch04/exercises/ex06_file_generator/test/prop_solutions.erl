-module(prop_solutions).
-compile([{nowarn_unused_function, [{ file_open, 2}, {file_write, 2}]}]).
-include_lib("proper/include/proper.hrl").
-import(ex06_file_generator,[mktemp/0]).
-export[file_open/2,file_write/2].

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
     ; Size > 1 -> lines(Size-1, {'$call', ?MODULE, file_write, [Fd,bin()]})
    end.	
	
bin() ->
 non_empty(bin()).
 

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



%file1(Name) ->
%     io:format("Name = ~p~n",[Name])
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
%	 .

%lines1(Size, Fd) ->
     %io:format("Size = ~p~n",[Size]),
%     if Size =< 1 -> Fd
%      ; Size > 1 -> lines(Size-1, {call, file_write, [Fd,bin()]})
%     end.