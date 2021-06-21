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

prop_unicode_string() ->
    ?FORALL(String, unicode_string(),
			begin
            io:format("~ts~n",[String]),
			is_list(String)
			end
        ).
		
prop_unicode_string2() ->
    ?FORALL(String, unicode_string2(),
			begin
            io:format("~ts~n",[String]),
			is_bitstring(String)
			end
        ).		

prop_unicode_string3() ->
    ?FORALL(String, unicode_string3(),
			begin
            io:format("~ts~n",[String]),
			is_list(String)
			end
        ).		

prop_unicode_string4() ->
    ?FORALL(String, unicode_string4(),
			begin
            io:format("~ts~n",[String]),
			is_list(String)
			end
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
 ?SUCHTHAT(S, non_empty(string()), io_lib:printable_unicode_list(S)).

unicode_string2() ->
 ?SUCHTHAT(S, non_empty(utf8()), io_lib:printable_unicode_list(unicode:characters_to_list(S))).

unicode_string3() ->
 ?SUCHTHAT(S, ?LET(S1, non_empty(utf8()), unicode:characters_to_list(S1)) , io_lib:printable_unicode_list(S)).
