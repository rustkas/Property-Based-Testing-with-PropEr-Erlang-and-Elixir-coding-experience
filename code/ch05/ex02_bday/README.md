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
	
	$ rebar3 proper -d apps/bday/test -m prop_csv_tuple -n 1
	$ rebar3 proper -d apps/bday/test -m prop_csv_tuple -n 1000
	
	
	$ rebar3 proper -d apps/bday/test -m prop_csv_tuple -p prop_unquoted_text -n 1000
	$ rebar3 proper -d apps/bday/test -m prop_csv_tuple -p prop_quotable_text -n 1000
	$ rebar3 proper -d apps/bday/test -m prop_csv_tuple -p prop_field -n 1000
	$ rebar3 proper -d apps/bday/test -m prop_csv_tuple -p prop_name -n 1000
	$ rebar3 proper -d apps/bday/test -m prop_csv_tuple -p prop_header -n 1000
	$ rebar3 proper -d apps/bday/test -m prop_csv_tuple -p prop_record -n 1000
	$ rebar3 proper -d apps/bday/test -m prop_csv_tuple -p prop_entry -n 1000
	$ rebar3 proper -d apps/bday/test -m prop_csv_tuple -p prop_entry2 -n 1000
	$ rebar3 proper -d apps/bday/test -m prop_csv_tuple -p prop_entry3 -n 1000
	$ rebar3 proper -d apps/bday/test -m prop_csv_tuple -p prop_csv_source -n 1
	$ rebar3 proper -d apps/bday/test -m prop_csv_tuple -p prop_roundtrip -n 1
	
Run PropEr generators
-----
	$ rebar3 as test shell
	proper_gen:pick(prop_csv:csv_source()).
	proper_gen:pick(prop_csv_tuple:csv_source()).
	proper_gen:pick(prop_csv_tuple:entry(5, ["fiedl1","fiedl2"])).
	proper_gen:pick(prop_csv_tuple:entry(proper_types:integer(2,2), prop_csv_tuple:header(proper_types:integer(2,2)))).
	proper_gen:pick(proper_types:integer(2,2)).
	proper_gen:pick(prop_csv_tuple:header(proper_types:integer(2,2))).
	
	proper_gen:pick(prop_csv_tuple:unquoted_text()).
	proper_gen:pick(prop_csv_tuple:quotable_text()).
	proper_gen:pick(prop_csv_tuple:field()).
	proper_gen:pick(prop_csv_tuple:name()).
	
	
	
	

EUnit
-----
	$ rebar3 eunit -v -m bday_csv_tuple
	$ rebar3 eunit -v -m rfc_tuple_tests
	$ rebar3 eunit -v -m rfc_tuple_encode_tests

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
