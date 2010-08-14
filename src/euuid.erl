%% -------------------------------------------------------------------
%% @copyright Martijn P. Rijkeboer
%% @author Martijn P. Rijkeboer <martijn@bunix.org>
%% @version {@vsn}, {@date}, {@time}
%% @doc TODO
%% @end
%% -------------------------------------------------------------------
-module(euuid).

-compile(export_all).


%% -------------------------------------------------------------------
%% @spec nil() ->
%%				UUID
%% @doc Get the RFC4122 nil UUID.
%% @end
%% -------------------------------------------------------------------
nil() ->
	concat(0, 0, 0, 0, 0, 0).


%% -------------------------------------------------------------------
%% @spec mac() ->
%%				UUID
%% @doc Get a new time and MAC based UUID (RFC4122 Version 1).
%% @end
%% -------------------------------------------------------------------
mac() ->
	Timestamp = get_timestamp(),
	<<TH:12, TM:16, TL:32>> = <<Timestamp:60>>, 
	V = 1,
	<<THV:16>> = <<V:4, TH:12>>,
	ClockSeq = get_clock_seq(),
	<<CSH:6, CSL:8>> = <<ClockSeq:14>>,
	R = 2#10,
	<<CSHR:8>> = <<R:2, CSH:6>>,
	N = gen_mac(),
	concat(TL, TM, THV, CSHR, CSL, N).


%% -------------------------------------------------------------------
%% @spec random() ->
%%				UUID
%% @doc Get a new (pseudo) random UUID (RFC4122 Version 4).
%% @end
%% -------------------------------------------------------------------
random() ->
	_Version = 4,
	ok.


%% -------------------------------------------------------------------
%% @spec sha1() ->
%%				UUID
%% @doc Get a new SHA1 name based UUID (RFC4122 Version 5).
%% @end
%% -------------------------------------------------------------------
sha1() ->
	_Version = 5,
	ok.


%% -------------------------------------------------------------------
%% @spec v1() ->
%%				UUID
%% @doc Get a new time and MAC based UUID (RFC4122 Version 1).
%% @end
%% -------------------------------------------------------------------
v1() ->
	mac().


%% -------------------------------------------------------------------
%% @spec v4() ->
%%				UUID
%% @doc Get a new (pseudo) random UUID (RFC4122 Version 4).
%% @end
%% -------------------------------------------------------------------
v4() ->
	random().


%% -------------------------------------------------------------------
%% @spec v5() ->
%%				UUID
%% @doc Get a new SHA1 name based UUID (RFC4122 Version 5).
%% @end
%% -------------------------------------------------------------------
v5() ->
	sha1().


%% -------------------------------------------------------------------
%% @spec gen_mac() ->
%%				Mac
%% @doc Generate a new MAC-address.
%% @end
%% -------------------------------------------------------------------
gen_mac() ->
	Head = new_random(6),
	Rest = new_random(16),
	Nic = new_random(24),
	Local = 1,
	Multicast = 1,
	<<Mac:48>> = <<Head:6, Local:1, Multicast:1, Rest:16, Nic:24>>,
	Mac.


%% -------------------------------------------------------------------
%% @spec new_random(Bits) ->
%%				Number
%% @doc Generate a new random number with a maximum size of Bits bit.
%% @end
%% -------------------------------------------------------------------
new_random(4) ->
	random:uniform(16#F) -1;
new_random(6) ->
	random:uniform(16#3F) -1;
new_random(12) ->
	random:uniform(16#FFF) -1;
new_random(14) ->
	random:uniform(16#3FFF) -1;
new_random(16) ->
	random:uniform(16#FFFF) -1;
new_random(24) ->
	random:uniform(16#FFFFFF) -1;
new_random(32) ->
	random:uniform(16#FFFFFFFF) -1;
new_random(Bits) when Bits > 0 andalso is_integer(Bits) ->
	io:format("Called euuid:new_random(~w).~n", [Bits]),
	random:uniform(erlang:trunc(math:pow(2, Bits))) -1.


%% -------------------------------------------------------------------
%% @spec get_timestamp() ->
%%				Timestamp
%% @doc Get the current timestamp as defined in RFC4122.
%% @end
%% -------------------------------------------------------------------
get_timestamp() ->
	Now = {_MegaSecs,_Secs,MicroSecs} = now(),
	Utc = calendar:now_to_universal_time(Now),
	Epoch = calendar:datetime_to_gregorian_seconds({{1582,10,15}, {0,0,0}}),
	Seconds = calendar:datetime_to_gregorian_seconds(Utc) - Epoch,
	Timestamp = ((Seconds * 1000000) + MicroSecs) * 10,
	Timestamp.


%% -------------------------------------------------------------------
%% @spec get_clock_seq() ->
%%				Number
%% @doc Get the current clock sequence.
%% @end
%% -------------------------------------------------------------------
get_clock_seq() ->
	new_random(14).


%% -------------------------------------------------------------------
%% @spec concat(TL, T, M, THV, CSHR, CSL, N) ->
%%				UUID
%% @doc Concatenate the UUID parts to an UUID.
%% @end
%% -------------------------------------------------------------------
concat(TL, TM, THV, CSHR, CSL, N) ->
	Uuid = io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", [TL, TM, THV, CSHR, CSL, N]),
	lists:flatten(Uuid).

