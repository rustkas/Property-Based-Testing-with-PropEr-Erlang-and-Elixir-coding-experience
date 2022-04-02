An OTP library

Create new OTP lib
-----
    $ rebar3 new lib ex01_biggest && cd ex01_biggest

Add information to rebar.config
-----

Get dependencies
-----
    $ rebar3 get-deps


Get PropEr help
-----
    $ rebar3 help proper


Create new propety via PropEr
-----
    $ rebar3 new proper thinking
    $ rebar3 format	
	$ rebar3 proper -p prop_biggest
	$ rebar3 eunit

EUnit tests
-----
    $ rebar3 eunit -v

PropEr test
-----
    $ rebar3 proper


PropEr test (make 10_000 tests)
-----	
	$ rebar3 proper -n 10000