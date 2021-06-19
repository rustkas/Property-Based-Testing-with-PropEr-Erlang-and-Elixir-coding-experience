-module(prop_generators).


-include_lib("proper/include/proper.hrl").


%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_collect1() ->
 ?FORALL(Bin, binary(), collect(byte_size(Bin), is_binary(Bin))).


%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
