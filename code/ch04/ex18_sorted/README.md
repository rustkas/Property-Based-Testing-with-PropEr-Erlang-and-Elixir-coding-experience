An OTP library

Create new OTP lib
-----
    $ rebar3 new lib ex18_sorted && cd ex18_sorted

Add information to rebar.config


Get dependencies
-----
    $ rebar3 get-deps


Get PropEr help
-----
    $ rebar3 help proper


Create new propety via PropEr
-----
    $ rebar3 new proper generators


Format
-----
    $ rebar3 format


PropEr test
-----
    $ rebar3 proper
	$ rebar3 proper -p prop_mostly_sorted -n 3
	$ rebar3 proper -p prop_sorted_list
