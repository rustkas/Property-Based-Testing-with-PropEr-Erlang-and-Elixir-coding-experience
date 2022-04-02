-module(register_example).

-import(register,[cash/3]).

-export([example01/0]).

example01() ->
	%% Money in the cash register
	Register = [{20.00, 1}, {10.00, 2}, {5.00, 4},
				{1.00, 10}, {0.25, 10}, {0.01, 100}],
	%% Change = cash(Register, PriceToPay, MoneyPaid),
	[{10.00, 1}] = cash(Register, 10.00, 20.00),
	[{10.00, 1}, {0.25, 1}] = cash(Register, 9.75, 20.00),
	[{0.01, 18}] = cash(Register, 0.82, 1.00),
	[{10.00, 1}, {5.00, 1}, {1.00, 3}, {0.25, 2}, {0.01, 13}]
 				= cash(Register, 1.37, 20.00).