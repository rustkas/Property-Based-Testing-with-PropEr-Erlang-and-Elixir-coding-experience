-module(base).

-export([biggest/1]).

biggest([Head | _Tail]) -> Head.
