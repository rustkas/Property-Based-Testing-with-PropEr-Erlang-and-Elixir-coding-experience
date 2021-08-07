pbt_step03
=====

An OTP library

Create new OTP lib
-----
    $ rebar3 new lib pbt_step03


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
    $ rebar3 proper --long_result


PropEr test (make 10_000 tests)
-----	
	$ rebar3 proper -n 10000