-module(prop_thinking2).

-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_symmetric() ->
    ?FORALL(Data, (list({atom(), any()})),
            begin
                Encoded = encode(Data),
                is_binary(Encoded) andalso Data =:= decode(Encoded)
            end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
%% Take a shortcut by using Erlang primitives
encode(T) -> term_to_binary(T).

decode(T) -> binary_to_term(T).
