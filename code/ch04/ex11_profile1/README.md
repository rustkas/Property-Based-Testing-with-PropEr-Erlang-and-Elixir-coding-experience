An OTP library


Create new OTP lib
-----
    $ rebar3 new lib ex11_profile1 && ex11_profile1

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
	$ rebar3 proper -m prop_generators -p prop_profile1,prop_profile2
	$ rebar3 proper -p prop_profile3
	$ rebar3 proper -p prop_profile4
	$ rebar3 proper -p prop_profile5
