-module(register).

-export([cash/3]).

-spec cash(Register, PriceToPay, MoneyPaid) -> ChangeList when
	Register :: [{float(), non_neg_integer()}],
	PriceToPay :: float(),
	MoneyPaid :: float(),
	ChangeList :: [{float(), non_neg_integer()}].
cash(Register, PriceToPay, MoneyPaid) when 
		is_list(Register), 
		is_number(PriceToPay), 
		is_number(MoneyPaid), 
		PriceToPay > 0.0, 
		MoneyPaid > 0.0, 
		MoneyPaid >= PriceToPay -> 
	ChangeValueTmp = MoneyPaid - PriceToPay,
	
	ChangeValue = tune_float(ChangeValueTmp),
	
	cash_acc(Register, ChangeValue, []).


-spec cash_acc(Register, ChangeValue, ChangeAcc) -> ChangeList when
	Register :: [{float(), non_neg_integer()}],
	ChangeValue :: float(),
	ChangeAcc :: [{float(), non_neg_integer()}],
	ChangeList :: ChangeAcc.
cash_acc(_Register, 0.00, ChangeAcc) -> 
	shrink(ChangeAcc);

cash_acc([], _ChangeValue, ChangeAcc) -> 
	shrink(ChangeAcc);
	
cash_acc([{Value, Count}|Rest], ChangeValue, ChangeAcc) ->
	case Value =< ChangeValue of
		true -> 
			case Count == 0 of
				true -> 
					cash_acc(Rest, ChangeValue, ChangeAcc);
				false ->
					MaxCount = erlang:trunc(ChangeValue / Value),		
					case MaxCount =< Count of
						true ->
							cash_acc([{Value, Count - MaxCount}|Rest], tune_float(ChangeValue - (Value * MaxCount)), [{Value, MaxCount}|ChangeAcc]);
						false ->
							cash_acc([{Value, Count - 1}|Rest], tune_float(ChangeValue - Value), [{Value, 1}|ChangeAcc])		
					end
			end;
		false ->
			cash_acc(Rest,ChangeValue, ChangeAcc)
	end.	
	
%% Util

shrink(Input) ->
	%ReversedList = lists:reverse(Input),

	List = lists:foldl(fun({Key,_Value}=Elem, Accumulator) ->
		Value = proplists:get_value(Key, Accumulator),
		case Value of
			undefined ->
				[Elem|Accumulator];
			_ ->
				NewAcc = proplists:delete(Key, Accumulator),
				[{Key, Value + 1}|NewAcc]
		end		
	end, [], Input),
	List.

%% transcate float mantissa 
tune_float(Float) ->
	list_to_float(float_to_list(round(Float*100)*1.0)) / 100.
	


