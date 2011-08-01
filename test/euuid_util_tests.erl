-module(euuid_util_tests).
-include_lib("eunit/include/eunit.hrl").

get_mac_addr_test_() ->
  [
    ?_assert(0 =< euuid_util:get_mac_addr()),
    ?_assert(281474976710656 > euuid_util:get_mac_addr())
  ].


get_timestamp_test_() ->
  [
    ?_assert(is_integer(euuid_util:get_timestamp())),
    ?_assert(euuid_util:get_timestamp() < euuid_util:get_timestamp())
  ].


incr_clock_seq_test_() ->
  [
    ?_assertEqual(1, euuid_util:incr_clock_seq(0)),
    ?_assertEqual(2, euuid_util:incr_clock_seq(1)),
    ?_assertEqual(16383, euuid_util:incr_clock_seq(16382)),
    ?_assertEqual(0, euuid_util:incr_clock_seq(16383)),
    ?_assertEqual(1, euuid_util:incr_clock_seq(16384)),
    ?_assertEqual(2, euuid_util:incr_clock_seq(-1))
  ].
  

new_clock_seq_test_() ->
  [
    ?_assert(0 =< euuid_util:new_clock_seq()),
    ?_assert(16384 > euuid_util:new_clock_seq()),
    ?_assert(euuid_util:new_clock_seq() =/= euuid_util:new_clock_seq())
  ].


new_random_test_() ->
  [
    ?_assert(0 =< euuid_util:new_random(4)),
    ?_assert(16 > euuid_util:new_random(4))
  ].


pack_test_() ->
  [
    ?_assertEqual(0,
      euuid_util:pack(0, 0, 0, 0, 0, 0)),
    ?_assertEqual(79229371458603035365442846721,
      euuid_util:pack(1, 1, 1, 1, 1, 1)),
    ?_assertEqual(0,
      euuid_util:pack(4294967296, 65536, 65536, 256, 256, 281474976710656)),
    ?_assertEqual(340282366920938463463374607431768211455,
      euuid_util:pack(4294967295, 65535, 65535, 255, 255, 281474976710655))
  ].


unpack_test_() ->
  [
    ?_assertEqual([0, 0, 0, 0, 0, 0],
      euuid_util:unpack(<<0:128>>)),
    ?_assertEqual([1, 1, 1, 1, 1, 1],
      euuid_util:unpack(<<79229371458603035365442846721:128>>)),
    ?_assertEqual([4294967295, 65535, 65535, 255, 255, 281474976710655],
      euuid_util:unpack(<<340282366920938463463374607431768211455:128>>))
  ].

