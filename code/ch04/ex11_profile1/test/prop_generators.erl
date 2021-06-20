-module(prop_generators).

-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_profile1() ->
    ?FORALL(Profile, [
			{name, resize(10, string())}, 
			{age, pos_integer()},
			{bio, resize(350, string())}
			],
            begin
			    %io:format("~p~n",[Profile]),
                NameLen = to_range(10, length(proplists:get_value(name, Profile))),
				BioLen = to_range(300, length(proplists:get_value(bio, Profile))),
                aggregate([{name, NameLen}, {bio, BioLen}], true)
            end).

prop_profile2() ->
 ?FORALL(Profile, [{name, string()},
                   {age, pos_integer()},
                   {bio, ?SIZED(Size, resize(Size*35, string()))}],
      begin
          NameLen = to_range(10,length(proplists:get_value(name, Profile))),
          BioLen = to_range(300,length(proplists:get_value(bio, Profile))),
          aggregate([{name, NameLen}, {bio, BioLen}], true)
      end).

prop_profile3() ->
 ?FORALL(Profile, [{name, string()},
                   {age, pos_integer()},
                   {bio, ?SIZED(Size,resize(min(100,Size)*35,string()))}],
      begin
          NameLen = to_range(10,length(proplists:get_value(name, Profile))),
          BioLen = to_range(300,length(proplists:get_value(bio, Profile))),
          aggregate([
		  %{name, NameLen}, 
		  {bio, BioLen}], true)
      end).

prop_profile4() ->
 ?FORALL(Profile, [{name, string()},
                   {age, pos_integer()},
                   {bio, ?SIZED(Size,resize(max(100,Size)*35,string()))}],
      begin
          NameLen = to_range(10,length(proplists:get_value(name, Profile))),
          BioLen = to_range(300,length(proplists:get_value(bio, Profile))),
          aggregate([
		  {name, NameLen}, 
		  {bio, BioLen}], true)
      end).



%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

to_range(M, N) ->
    Base = N div M,
    %io:format("~2B = ~2B div ~B | {~2B, ~2B} = {~2B * ~B, (~B + 1)*~B}~n",
    %          [Base, N, M, Base * M, (Base + 1) * M, Base, M, Base, M]),
    {Base * M, (Base + 1) * M}.
