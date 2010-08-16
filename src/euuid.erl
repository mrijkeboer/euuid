%% -------------------------------------------------------------------
%% @copyright Martijn P. Rijkeboer
%% @author Martijn P. Rijkeboer <martijn@bunix.org>
%% @version {@vsn}, {@date}, {@time}
%% @doc TODO
%% @end
%% -------------------------------------------------------------------
-module(euuid).

-export([
		format/1,
		md5/2,
		nil/0,
		ns_dns/0,
		ns_url/0,
		ns_oid/0,
		ns_x500/0,
		random/0,
		sha1/2,
		time_mac/0,
		v1/0,
		v3/2,
		v4/0,
		v5/2
	]).


%% -------------------------------------------------------------------
%% @spec time_mac() ->
%%				UUID
%% @doc Get a new time and MAC based UUID (RFC4122 Version 1).
%% @end
%% -------------------------------------------------------------------
time_mac() ->
	Timestamp = get_timestamp(),
	<<TH:12, TM:16, TL:32>> = <<Timestamp:60>>, 
	V = 1,
	<<THV:16>> = <<V:4, TH:12>>,
	ClockSeq = get_clock_seq(),
	<<CSH:6, CSL:8>> = <<ClockSeq:14>>,
	R = 2#10,
	<<CSHR:8>> = <<R:2, CSH:6>>,
	N = gen_mac(),
	pack([TL, TM, THV, CSHR, CSL, N]).


%% -------------------------------------------------------------------
%% @spec md5(NsUUID, Name) ->
%%				UUID
%% @doc Get a new MD5 name based UUID (RFC4122 Version 3).
%% @end
%% -------------------------------------------------------------------
md5(NsUUID, Name) ->
	Data = list_to_binary([<<NsUUID:128>>, Name]),
	<<MD5:128>> = crypto:md5(Data),
	<<TL:32, TM:16, _:4, TH:12, _:2, CSH:6, CSL:8, N:48>> = <<MD5:128>>,
	V = 3,
	<<THV:16>> = <<V:4, TH:12>>,
	R = 2#10,
	<<CSHR:8>> = <<R:2, CSH:6>>,
	pack([TL, TM, THV, CSHR, CSL, N]).


%% -------------------------------------------------------------------
%% @spec random() ->
%%				UUID
%% @doc Get a new (pseudo) random UUID (RFC4122 Version 4).
%% @end
%% -------------------------------------------------------------------
random() ->
	TH = new_random(12),
	TM = new_random(16),
	TL = new_random(32),
	CSH = new_random(6),
	CSL = new_random(8),
	N = new_random(48),
	V = 4,
	<<THV:16>> = <<V:4, TH:12>>,
	R = 2#10,
	<<CSHR:8>> = <<R:2, CSH:6>>,
	pack([TL, TM, THV, CSHR, CSL, N]).


%% -------------------------------------------------------------------
%% @spec sha1(NsUUID, Name) ->
%%				UUID
%% @doc Get a new SHA1 name based UUID (RFC4122 Version 5).
%% @end
%% -------------------------------------------------------------------
sha1(NsUUID, Name) ->
	Data = list_to_binary([<<NsUUID:128>>, Name]),
	<<Sha1:160>> = crypto:sha(Data),
	<<TL:32, TM:16, _:4, TH:12, _:2, CSH:6, CSL:8, N:48, _:32>> = <<Sha1:160>>,
	V = 5,
	<<THV:16>> = <<V:4, TH:12>>,
	R = 2#10,
	<<CSHR:8>> = <<R:2, CSH:6>>,
	pack([TL, TM, THV, CSHR, CSL, N]).


%% -------------------------------------------------------------------
%% @spec v1() ->
%%				UUID
%% @doc Get a new time and MAC based UUID (RFC4122 Version 1).
%% @end
%% -------------------------------------------------------------------
v1() ->
	time_mac().


%% -------------------------------------------------------------------
%% @spec v3(NsUUID, Name) ->
%%				UUID
%% @doc Get a new MD5 name based UUID (RFC4122 Version 3).
%% @end
%% -------------------------------------------------------------------
v3(NsUUID, Name) ->
	md5(NsUUID, Name).


%% -------------------------------------------------------------------
%% @spec v4() ->
%%				UUID
%% @doc Get a new (pseudo) random UUID (RFC4122 Version 4).
%% @end
%% -------------------------------------------------------------------
v4() ->
	random().


%% -------------------------------------------------------------------
%% @spec v5(NsUUID, Name) ->
%%				UUID
%% @doc Get a new SHA1 name based UUID (RFC4122 Version 5).
%% @end
%% -------------------------------------------------------------------
v5(NsUUID, Name) ->
	sha1(NsUUID, Name).


%% -------------------------------------------------------------------
%% @spec format(UUID) ->
%%				UuidStr
%% @doc Format the UUID into string representation.
%% @end
%% -------------------------------------------------------------------
format(UUID) ->
	Str = io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", unpack(<<UUID:128>>)),
	lists:flatten(Str).


%% -------------------------------------------------------------------
%% @spec nil() ->
%%				UUID
%% @doc Get the RFC4122 nil UUID.
%% @end
%% -------------------------------------------------------------------
nil() ->
	0.


%% -------------------------------------------------------------------
%% @spec ns_dns() ->
%%				UUID
%% @doc Get the RFC4122 DNS namespace UUID.
%% @end
%% -------------------------------------------------------------------
ns_dns() ->
	16#6ba7b8109dad11d180b400c04fd430c8.

%% -------------------------------------------------------------------
%% @spec ns_url() ->
%%				UUID
%% @doc Get the RFC4122 URL namespace UUID.
%% @end
%% -------------------------------------------------------------------
ns_url() ->
	16#6ba7b8119dad11d180b400c04fd430c8.


%% -------------------------------------------------------------------
%% @spec ns_oid() ->
%%				UUID
%% @doc Get the RFC4122 OID namespace UUID.
%% @end
%% -------------------------------------------------------------------
ns_oid() ->
	16#6ba7b8129dad11d180b400c04fd430c8.


%% -------------------------------------------------------------------
%% @spec ns_x500() ->
%%				UUID
%% @doc Get the RFC4122 X500 namespace UUID.
%% @end
%% -------------------------------------------------------------------
ns_x500() ->
	16#6ba7b8149dad11d180b400c04fd430c8.


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
new_random(8) ->
	random:uniform(16#FF) -1;
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
new_random(48) ->
	random:uniform(16#FFFFFFFFFFFF) -1;
new_random(64) ->
	random:uniform(16#FFFFFFFFFFFFFFFF) -1;
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
%% @spec pack(TL, TM, THV, CSHR, CSL, N) ->
%%				UUID
%% @doc Pack the parts into an UUID.
%% @end
%% -------------------------------------------------------------------
pack([TL, TM, THV, CSHR, CSL, N]) ->
	<<UUID:128>> = <<TL:32, TM:16, THV:16, CSHR:8, CSL:8, N:48>>,
	UUID.


%% -------------------------------------------------------------------
%% @spec unpack(UUID) ->
%%				[TL, TM, THV, CSHR, CSL, N]
%% @doc Unpack the UUID into it's parts.
%% @end
%% -------------------------------------------------------------------
unpack(<<TL:32, TM:16, THV:16, CSHR:8, CSL:8, N:48>>) ->
	[TL, TM, THV, CSHR, CSL, N].

