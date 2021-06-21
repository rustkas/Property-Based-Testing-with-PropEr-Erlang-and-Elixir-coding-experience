-module(prop_generators).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_latin1_string() ->
    ?FORALL(String, latin1_string(),
        begin
            is_list(String)
        end).

prop_latin1_string2() ->
    ?FORALL(String, latin1_string2(),
            is_list(String)
        ).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
latin1_string() ->
 ?SUCHTHAT(S, string(), io_lib:printable_latin1_list(S)).

latin1_string2() ->
  ?SUCHTHAT(S, ?SIZED(Size,vector(Size,integer(33, 126))), io_lib:printable_latin1_list(S)).

unicode_string() ->
 ?SUCHTHAT(S, string(), io_lib:printable_unicode_list(S)).

