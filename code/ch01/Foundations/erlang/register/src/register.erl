-module(register).

-export([cash/3]).

-spec cash(Register, PriceToPay, MoneyPaid) -> ChangeList when
	Register :: [{float(), non_neg_integer()}],
	PriceToPay :: float(),
	MoneyPaid :: float(),
	ChangeList :: [{float(), non_neg_integer()}].
cash(Register, PriceToPay, MoneyPaid) when 
	is_list(Register), is_number(PriceToPay), is_number(MoneyPaid), 
	PriceToPay > 0, MoneyPaid > 0, MoneyPaid >= PriceToPay -> 
	ChangeValue = MoneyPaid - PriceToPay,
	cash(Register, PriceToPay, MoneyPaid, ChangeAcc).


-spec cash(Register, PriceToPay, MoneyPaid, ChangeAcc) -> ChangeList when
	Register :: [{float(), non_neg_integer()}],
	PriceToPay :: float(),
	MoneyPaid :: float(),
	ChangeAcc :: [{float(), non_neg_integer()}],
	ChangeList :: ChangeAcc.
cash(Register, PriceToPay, 0.00, ChangeAcc) -> ChangeAcc.



