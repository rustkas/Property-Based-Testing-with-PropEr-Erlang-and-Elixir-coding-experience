An OTP library

Create new OTP lib
-----
    $ rebar3 new lib ex05_stamp && cd ex05_stamp

Add information to rebar.config


Get dependencies
-----
    $ rebar3 get-deps


Get PropEr help
-----
    $ rebar3 help proper


Create new propety via PropEr
-----
    $ rebar3 new proper exercises


Format
-----
    $ rebar3 format


PropEr test
-----
    $ rebar3 proper
	$ rebar3 proper -p prop_am_stamp1
    $ rebar3 proper -p prop_am_stamp2	
	$ rebar3 proper -p prop_stamps1
	$ rebar3 proper -p prop_no_standup1
	$ rebar3 proper -p prop_no_standup2
