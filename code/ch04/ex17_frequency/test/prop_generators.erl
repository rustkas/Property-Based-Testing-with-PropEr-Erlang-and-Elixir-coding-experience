-module(prop_generators).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_string() ->
    ?FORALL(String, string(),
        begin
            true = is_list(String),
			lists:all(fun(Elem)->is_integer(Elem) end, String)
        end).

prop_text_like() ->
    LettersIds = lists:seq(33, 126) ++ [$\s] ++ [$., $-, $!, $?, $,] ++ lists:seq($0, $9),
	True = true,
    ?FORALL(String, text_like(),
        begin
            %true = is_list(String),
			%lists:all(fun(Elem)->is_integer(Elem) end, String),
			lists:any(fun(Elem) -> 
			         %io:format("LettersIds: ~p~n",[LettersIds]),
					 %io:format("Element: ~p~n",[Elem]),
					 % find position of the letter in  letter list
					 Result = sstr:chr(LettersIds, [Elem]),
					 
					 True = is_integer(Result)
					 
					 end, String),
			True
			
        end).
		
prop_text_like2() ->
    LettersIds = lists:seq($a, $z),
	?FORALL(String, text_like2(),
        begin
            lists:all(fun(Elem) -> 
			         Result = sstr:chr(LettersIds, [Elem]),
					 %io:format("Element: ~p~n",[Elem]),
					 true = is_integer(Result)
					 end, String),
			true
			
        end).
	
%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
text_like() ->
  list(frequency([{80, range($a, $z)},  % letters
                      {10, $\s},                       % whitespace
                      {1, oneof([$., $-, $!, $?, $,])},% punctuation
                      {1, range($0, $9)}               % numbers
])).

text_like2() -> list(frequency([{100, range($a, $z)}])).