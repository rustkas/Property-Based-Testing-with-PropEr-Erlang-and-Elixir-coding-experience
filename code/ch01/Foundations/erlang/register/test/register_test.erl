-module(register_test).

-import(register,[cash/3]).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

biggest_test() ->
    ?assert((5 =:= biggest([1, 2, 3, 4, 5]))),
    ?assert((8 =:= biggest([3, 8, 7, -1]))),
    ?assert((0 =:= biggest([0]))),
    ?assert((-5 =:= biggest([-10, -5, -901]))).

	%% Money in the cash register
	Register = [{20.00, 1}, {10.00, 2}, {5.00, 4}, 
				{1.00, 10}, {0.25, 10}, {0.01, 100}],
	
	%% Change 		= cash(Register, PriceToPay, MoneyPaid),
	?assert([{10.00, 1}], cash(Register, 10.00, 20.00)),
	?assert([{10.00, 1}, {0.25, 1}], cash(Register, 9.75, 20.00),
	?assert([{0.01, 18}], cash(Register, 0.82, 1.00)),
	?assert([{10.00, 1}, {5.00, 1}, {1.00, 3}, {0.25, 2}, {0.01, 13}], cash(Register, 1.37, 20.00)).

-endif.
