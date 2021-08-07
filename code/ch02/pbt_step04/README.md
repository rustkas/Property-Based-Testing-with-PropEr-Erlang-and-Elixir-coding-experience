An OTP library

Create new OTP lib
-----
    $ rebar3 new lib pbt_step04


Get dependencies
-----
    $ rebar3 get-deps


Get PropEr help
-----
    $ rebar3 help proper


Create new propety via PropEr
-----
    $ rebar3 new proper base
	

PropEr test
-----
    $ rebar3 proper


PropEr test (make 10_000 tests)
-----	
	$ rebar3 proper -n 10000

## EUnit
-----
	$ rebar3 eunit
	$ rebar3 eunit -m base_test	