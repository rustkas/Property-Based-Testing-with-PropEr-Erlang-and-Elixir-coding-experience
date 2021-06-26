-module(prop_exercises).

-include_lib("proper/include/proper.hrl").
-compile([{nowarn_unused_function, [file_open/2, file_write/2]}]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

file_open(Name, Opts) ->
    {ok, Fd} = file:open(Name, Opts),
    %% ensure the file is refreshed on each test run
    file:truncate(Fd),
    Fd.

file_write(Fd, Data) ->
    ok = file:write(Fd, Data),
    Fd.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
