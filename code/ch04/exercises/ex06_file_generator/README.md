An OTP library

Create new OTP lib
-----
    $ rebar3 new lib ex06_file_generator && cd ex06_file_generator

Add information to rebar.config


Get dependencies
-----
    $ rebar3 get-deps


Get PropEr help
-----
    $ rebar3 help proper


Create new propety via PropEr
-----
    $ rebar3 new proper mktemp
    $ rebar3 new proper exercises
	$ rebar3 new proper solutions


Format
-----
    $ rebar3 format


PropEr test
-----
    $ rebar3 proper -m prop_mktemp -p prop_rand
	$ rebar3 proper -m prop_mktemp -p prop_basedir -n 1
	$ rebar3 proper -m prop_mktemp -p prop_tmp_file_name
	$ rebar3 proper -m prop_mktemp -p prop_tmp_ensure_dir -n 1
	$ rebar3 proper -m prop_mktemp -p prop_tmp_write_to_file -n 1
	$ rebar3 proper -m prop_mktemp -p prop_tmp_write_to_file2 -n 1
	
	$ rebar3 proper -m prop_solutions -p prop_make_tmp_file -n 1
	$ rebar3 proper -m prop_solutions -p prop_make_tmp_file

