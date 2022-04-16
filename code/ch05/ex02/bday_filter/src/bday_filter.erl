%% ---
%%  Excerpted from "Property-Based Testing with PropEr, Erlang, and Elixir",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/fhproper for more book information.
%%---
-module(bday_filter).
-export([birthday/2]).

-spec birthday([People], BirthDay) -> Result when
	BirthDay :: Date,
	Date :: {1..31, 1..12, non_neg_integer()},
	People :: #{string() := reference(), string() := Date},
	Result :: [People].
birthday(People, {Year, 2, 28}) ->
    case calendar:is_leap_year(Year) of
        true -> filter_dob(People, 2, 28);
        false -> filter_dob(People, 2, 28) ++ filter_dob(People, 2, 29)
    end;
birthday(People, {_Year, Month, Day}) ->
    filter_dob(People, Month, Day).

-spec filter_dob([People], Month, Day) -> Result when
	People :: #{string() := reference(), string() := Date},
	Month :: non_neg_integer(), 
	Day :: non_neg_integer(),
	Date :: {1..31, 1..12, non_neg_integer()},
	Result :: [People].
filter_dob(People, Month, Day) ->
    lists:filter(
      fun(#{"date_of_birth" := {_,M,D}}) -> {Month,Day} == {M,D} end,
      People
    ).
