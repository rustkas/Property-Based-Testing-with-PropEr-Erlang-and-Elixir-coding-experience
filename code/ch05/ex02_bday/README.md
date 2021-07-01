An OTP application

Create new project
----	
Create ex02_bday
----	
	$ rebar3 new umbrella ex02_bday
	$ cd ex02_bday
	$ rm -R apps
	$ mkdir lib
	$ cd lib
	$ rebar3 new escript bday
	$ cd ..
	
	# all commands in one string
	$ rebar3 new umbrella ex02_bday && cd ex02_bday && rm -R apps && mkdir lib && cd lib && rebar3 new escript bday && cd ..


Proper
-----
	$ rebar3 proper -d apps/bday/test -n 1000
	$ rebar3 proper -d apps/bday/test -p prop_roundtrip  -n 1000

EUnit
-----
	$ rebar3 eunit -v --app bday -m rfc_tests
	$ rebar3 eunit -v --app bday -m rfc_tuple_tests
	$ rebar3 eunit -v -m bday_csv_tuple
	

Build
-----
	$ rebar3 compile

Run
-----
	$ rebar3 eunit -v --app bday


	
Format
-----
	$ rebar3 format

Generate documentation
-----
	$ rebar3 edoc
