An OTP library


Create new OTP lib
-----
    $ rebar3 new lib ex15_suchthat_or_let && cd ex15_suchthat_or_let

Add information to rebar.config


Get dependencies
-----
    $ rebar3 get-deps


Get PropEr help
-----
    $ rebar3 help proper


Create new propety via PropEr
-----
    $ rebar3 new proper suchthat
	$ rebar3 new proper let_macros


Format
-----
    $ rebar3 format


PropEr test
-----
    $ rebar3 proper
	$ rebar3 proper -m prop_let -p prop_even
	$ rebar3 proper -m prop_let -p prop_uneven
	$ rebar3 proper -m prop_let -p prop_stress_test1
	$ rebar3 proper -m prop_let -p prop_stress_test2
	
	$ rebar3 proper -m prop_suchthat -p prop_even
	$ rebar3 proper -m prop_suchthat -p prop_uneven
	$ rebar3 proper -m prop_suchthat -p prop_stress_test1
	$ rebar3 proper -m prop_suchthat -p prop_stress_test2
	