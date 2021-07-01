An OTP application

Create new project
----	
Create ex01_bday
----	
	$ rebar3 ex01_bday
	$ cd ex01_bday
	$ rm -R apps
	$ mkdir lib
	$ cd lib
	$ rebar3 new escript bday
	$ cd ..
	
	# all commands in one string
	$ rebar3 new umbrella ex01_bday && cd ex01_bday && rm -R apps && mkdir lib && cd lib && rebar3 new escript bday && cd ..


Proper
-----
	$ rebar3 proper -d apps/bday/test -n 1000
	$ rebar3 proper -d apps/bday/test -p prop_roundtrip_01

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
