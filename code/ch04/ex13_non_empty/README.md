An OTP library


Create new OTP lib
-----
    $ rebar3 new lib ex13_non_empty && ex13_non_empty

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
	$ rebar3 proper -p prop_non_empty1
	$ rebar3 proper -p prop_non_empty2
	$ rebar3 proper -p prop_non_empty3
	$ rebar3 proper -p prop_non_empty_map
	$ rebar3 proper -p prop_non_empty_list
	$ rebar3 proper -p prop_non_empty_string
	$ rebar3 proper -p prop_non_bitstring
	$ rebar3 proper -p prop_non_tuple1
	$ rebar3 proper -p prop_non_tuple2
	
