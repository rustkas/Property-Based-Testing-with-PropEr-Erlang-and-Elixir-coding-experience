An OTP library

Create new OTP lib
-----
    $ rebar3 new lib ex17_frequency && cd ex17_frequency

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
	$ rebar3 proper -p prop_string
	$ rebar3 proper -p prop_text_like
	$ rebar3 proper -p prop_text_like2