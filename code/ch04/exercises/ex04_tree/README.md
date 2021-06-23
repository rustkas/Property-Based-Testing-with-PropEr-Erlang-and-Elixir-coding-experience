An OTP library

Create new OTP lib
-----
    $ rebar3 new lib ex04_tree && cd ex04_tree

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
    $ rebar3 proper -p prop_tree -n 1
    $ rebar3 proper -p prop_lazy_tree -n 1
	$ rebar3 proper -p prop_limited_tree -n 1