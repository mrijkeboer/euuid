-module(euuid_tests).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Test functions
%% ===================================================================

setup() ->
	euuid:start().


teardown(_) ->
	euuid:stop().


supervisor_test() ->
	?_assertNot(undefined == whereis(euuid_sup)).


nil_uuid_test() ->
	?_assertEqual(0, euuid:nil()).


format_test_() ->
	[
		?_assertEqual("00000000-0000-0000-0000-000000000000",
			euuid:format(0)),
		?_assertEqual("aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeff",
			euuid:format(16#aaaaaaaabbbbccccddddeeeeeeeeeeff))
	].


namespace_uuids_test_() ->
	[ ?_assertEqual("6ba7b810-9dad-11d1-80b4-00c04fd430c8",
			euuid:format(euuid:ns_dns())),
		?_assertEqual("6ba7b811-9dad-11d1-80b4-00c04fd430c8",
			euuid:format(euuid:ns_url())),
		?_assertEqual("6ba7b812-9dad-11d1-80b4-00c04fd430c8",
			euuid:format(euuid:ns_oid())),
		?_assertEqual("6ba7b814-9dad-11d1-80b4-00c04fd430c8",
			euuid:format(euuid:ns_x500()))
	].


time_mac_test_() ->
	{ setup, fun setup/0, fun teardown/1,
		[ ?_assert(is_integer(euuid:time_mac())),
			?_assert(euuid:time_mac() /= euuid:time_mac())
		]
	}.


md5_test_() ->
	{ setup, fun setup/0, fun teardown/1,
		[ ?_assert(is_integer(euuid:md5(euuid:ns_dns(), "example.com"))),
			?_assertEqual(16#9073926b929f31c2abc9fad77ae3e8eb, 
				euuid:md5(euuid:ns_dns(), "example.com")),
			?_assertEqual(16#1f986141bd0d341781253033cca442fe,
				euuid:md5(euuid:ns_dns(), "example.net"))
		]
	}.


random_test_() ->
	{ setup, fun setup/0, fun teardown/1,
		[ ?_assert(is_integer(euuid:random())),
			?_assert(euuid:random() /= euuid:random())
		]
	}.


sha1_test_() ->
	{ setup, fun setup/0, fun teardown/1,
		[ ?_assert(is_integer(euuid:sha1(euuid:ns_dns(), "example.com"))),
			?_assertEqual(16#cfbff0d193755685968c48ce8b15ae17, 
				euuid:sha1(euuid:ns_dns(), "example.com")),
			?_assertEqual(16#90d38b76e7dd573385750d06a98e8b70,
				euuid:sha1(euuid:ns_dns(), "example.net"))
		]
	}.


time_custom_test_() ->
	{ setup, fun setup/0, fun teardown/1,
		[ ?_assert(is_integer(euuid:time_custom())),
			?_assert(euuid:time_custom() < euuid:time_custom())
		]
	}.


v1_test_() ->
	{ setup, fun setup/0, fun teardown/1,
		[ ?_assert(is_integer(euuid:v1())),
			?_assert(euuid:v1() /= euuid:v1())
		]
	}.


v3_test_() ->
	{ setup, fun setup/0, fun teardown/1,
		[ ?_assert(is_integer(euuid:v3(euuid:ns_dns(), "example.com"))),
			?_assertEqual(16#9073926b929f31c2abc9fad77ae3e8eb, 
				euuid:v3(euuid:ns_dns(), "example.com")),
			?_assertEqual(16#1f986141bd0d341781253033cca442fe,
				euuid:v3(euuid:ns_dns(), "example.net"))
		]
	}.


v4_test_() ->
	{ setup, fun setup/0, fun teardown/1,
		[ ?_assert(is_integer(euuid:v4())),
			?_assert(euuid:v4() /= euuid:v4())
		]
	}.


v5_test_() ->
	{ setup, fun setup/0, fun teardown/1,
		[ ?_assert(is_integer(euuid:v5(euuid:ns_dns(), "example.com"))),
			?_assertEqual(16#cfbff0d193755685968c48ce8b15ae17, 
				euuid:v5(euuid:ns_dns(), "example.com")),
			?_assertEqual(16#90d38b76e7dd573385750d06a98e8b70,
				euuid:v5(euuid:ns_dns(), "example.net"))
		]
	}.

