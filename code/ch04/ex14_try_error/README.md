An OTP library


Create new OTP lib
-----
    $ rebar3 new lib ex14_try_error && cd ex14_try_error

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
    $ rebar3 proper -n 50
	$ rebar3 proper -p prop_range
	$ rebar3 proper -p prop_type
	$ rebar3 proper -p prop_list_content_integer
