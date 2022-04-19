ex02
=====

Create new project
    $ rebar3 new lib ex01
	$ ​rebar3​ ​new​ ​proper_statem​ ​cache
	
-----

An OTP library

Build
-----

    $ rebar3 compile

	$ rebar3 as test shell
	1> proper_gen:sample(proper_statem:commands(prop_cache)).
	$ ​rebar3​ ​proper​ ​-n​ ​1000
	​$ rebar3 proper -p prop_test  -n 100
	​$ rebar3 proper -p prop_parallel -n 10000
	rebar3 proper -p prop_parallel -n 10000