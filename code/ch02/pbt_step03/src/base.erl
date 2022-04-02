-module(base).

-export([biggest/1]).

-spec biggest([term()]) -> term().
biggest([Head | Tail]) -> biggest(Tail, Head).

-spec biggest(List, Biggest) -> Result when
    List :: [term()],
    Biggest :: term(),
    Result :: term().
biggest([], Biggest) -> Biggest;
biggest([Head | Tail], Biggest) when Head > Biggest ->
    biggest(Tail, Head);
biggest([Head | Tail], Biggest) when Head < Biggest ->
    biggest(Tail, Biggest).