bday
=====

Create project
------
	$ rebar3 new lib bday_csv_proplists

Get dependencies
-----
    $ rebar3 get-deps


Get PropEr help
-----
    $ rebar3 help proper


Create new propety via PropEr
-----
    $ rebar3 new proper csv
	
Compile
-----
    $ rebar3 compile
    > r3:do(compile). % recompile after source code editing 

Test generators
-----

$ rebar3 as test shell
1> proper_gen:pick(prop_csv:csv_source()).
2> proper_gen:pick(prop_csv:quotable_text()).

EUnit test
-----
rebar3 eunit

	$ rebar3 eunit -m bday_csv
	$ rebar3 eunit -m rfc_tests

Dialyzer
-----
	$ rebar3 dialyzer

PropEr test
-----
    $ rebar3 proper


PropEr test (make 10_000 tests)
-----	
	$ rebar3 proper -n 10000


