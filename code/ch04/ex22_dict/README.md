An OTP library

Create new OTP lib
-----
    $ rebar3 new lib ex22_dict && cd ex22_dict

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
    $ rebar3 proper -p prop_dict_gen
    $ rebar3 proper -p prop_dict_symb
	$ rebar3 proper -p prop_dict_autosymb
