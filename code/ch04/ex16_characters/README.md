An OTP library


Create new OTP lib
-----
    $ rebar3 new lib ex16_characters && cd ex16_characters

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
	$ rebar3 proper -p prop_latin1_string --constraint_tries 80
	$ rebar3 proper -p prop_latin2_string2
	$ rebar3 proper -p prop_unicode_string --constraint_tries 2000
	$ rebar3 proper -p prop_unicode_string2
	$ rebar3 proper -p prop_unicode_string3