-module(prop_solutions).

-include_lib("proper/include/proper.hrl").

-import(ex06_symbolic_call, [mktemp/0]).

-export([file_open/2, file_write/2]).

-compile([{nowarn_unused_function, [file_open/2, file_write/2]}]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_make_tmp_file() ->
    TmpFilePath = mktemp(),
    ok = file:write_file(TmpFilePath, <<>>),
    io:format("File path: ~p~n", [TmpFilePath]),
    %`Return` is return value of a proper function. You have to place it at the end of a function.
    Result =
        ?FORALL({FilePath, Fd},
                make_tmp_file(TmpFilePath),
                begin
                    %io:format("Generator work results. File path: ~p, ~p~n", [FilePath, Fd]),
                    file:datasync(Fd),
                    FileSize = filelib:file_size(FilePath),
                    io:format("File size: ~p~n", [FileSize]),
                    true
                end),
    Result.

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
file_open(Name, Opts) ->
    %io:format("File Open. Name = ~p, Opts = ~p~n", [Name, Opts]),
    {ok, Fd} = file:open(Name, Opts),
    %% ensure the file is refreshed on each test run
    ok = file:truncate(Fd),
    %io:format("File Open. File Descriptor = ~p~n", [Fd]),
    Fd.

file_write(Fd, Data) ->
   % io:format("File Write. Fd = ~p, Data = ~p~n", [Fd, Data]),
    ok = file:write(Fd, Data),
    Fd.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
make_tmp_file(TmpFilePath) ->
    %io:format("TmpFilePath = ~p~n", [TmpFilePath]),
    file(TmpFilePath).

file(Name) ->
    %io:format("Name = ~p~n", [Name]),
    ?SIZED(Size,
           begin
               %io:format("Name = ~p~n", [Name]),
               %io:format("Size = ~p~n", [Size]),
               Fd = lines(Size, {call, ?MODULE, file_open, [Name, [read, write, raw]]}),
               {Name, Fd}
           end).

lines(Size, Fd) ->
    if Size =< 1 ->
           Fd;
       Size > 1 ->
           lines(Size - 1, {call, ?MODULE, file_write, [Fd, bin()]})
    end.

bin() ->
    non_empty(binary()).
