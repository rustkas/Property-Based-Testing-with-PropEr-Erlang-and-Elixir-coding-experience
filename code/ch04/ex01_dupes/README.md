ex01_dupes
=====

An OTP library


Create new OTP lib
-----
    $ rebar3 new lib ex01_dupes && cd ex01_dupes

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

Launch test shell
-----

$ rebar3 as test shell

```
proper_gen:sample(proper_types:list({prop_generators:key(),prop_generators:val()})).

```