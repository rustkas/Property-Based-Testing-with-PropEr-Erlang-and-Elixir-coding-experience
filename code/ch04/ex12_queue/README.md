An OTP library


Create new OTP lib
-----
    $ rebar3 new lib ex12_queue && cd ex12_queue

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
	$ rebar3 proper -m prop_generators -p prop_queue_naive
	$ rebar3 proper -m prop_generators -p prop_queue_nicer
	
