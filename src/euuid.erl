%% -------------------------------------------------------------------
%% euuid.erl - Erlang UUID API module
%%
%% @author Martijn P. Rijkeboer <martijn@bunix.org>
%% @copyright Martijn P. Rijkeboer
%% @version {@vsn}, {@date}, {@time}
%% @doc Erlang UUID API module.
%% @end
%%
%% The MIT license.
%%
%% Copyright (c) 2010 Martijn P. Rijkeboer
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to
%% deal in the Software without restriction, including without limitation the
%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%% sell copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.
%%
%% -------------------------------------------------------------------
-module(euuid).

%% API
-export([start/0, stop/0]).
-export([format/1]).
-export([md5/2, v3/2]).
-export([random/0, v4/0]).
-export([sha1/2, v5/2]).
-export([time_custom/0]).
-export([time_mac/0, v1/0]).
-export([nil/0, ns_dns/0, ns_url/0, ns_oid/0, ns_x500/0]).


%%====================================================================
%% API
%%====================================================================

%% -------------------------------------------------------------------
%% @spec start() ->
%%				ok |
%%				{error, Reason}
%% @doc Start the Erlang UUID application.
%% @end
%% -------------------------------------------------------------------
start() ->
	application:start(euuid).


%% -------------------------------------------------------------------
%% @spec stop() ->
%%				ok |
%%				{error, Reason}
%% @doc Stop the Erlang UUID application.
%% @end
%% -------------------------------------------------------------------
stop() ->
	application:stop(euuid).


%% -------------------------------------------------------------------
%% @spec time_mac() ->
%%				UUID
%% @doc Get a new time and MAC based UUID (RFC4122 Version 1).
%% @end
%% -------------------------------------------------------------------
time_mac() ->
	euuid_server:time_mac().


%% -------------------------------------------------------------------
%% @spec md5(NsUUID, Name) ->
%%				UUID
%% @doc Get a new MD5 name based UUID (RFC4122 Version 3).
%% @end
%% -------------------------------------------------------------------
md5(NsUUID, Name) ->
	euuid_server:md5(NsUUID, Name).


%% -------------------------------------------------------------------
%% @spec random() ->
%%				UUID
%% @doc Get a new (pseudo) random UUID (RFC4122 Version 4).
%% @end
%% -------------------------------------------------------------------
random() ->
	euuid_server:random().


%% -------------------------------------------------------------------
%% @spec sha1(NsUUID, Name) ->
%%				UUID
%% @doc Get a new SHA1 name based UUID (RFC4122 Version 5).
%% @end
%% -------------------------------------------------------------------
sha1(NsUUID, Name) ->
	euuid_server:sha1(NsUUID, Name).


%% -------------------------------------------------------------------
%% @spec time_custom() ->
%%				UUID
%% @doc Get a new time and MAC based UUID with modified timestamp
%%	layout to allow sorting on the UUID's date of creation.
%% @end
%% -------------------------------------------------------------------
time_custom() ->
	euuid_server:time_custom().


%% -------------------------------------------------------------------
%% @spec v1() ->
%%				UUID
%% @doc Get a new time and MAC based UUID (RFC4122 Version 1).
%% @end
%% -------------------------------------------------------------------
v1() ->
	euuid_server:time_mac().


%% -------------------------------------------------------------------
%% @spec v3(NsUUID, Name) ->
%%				UUID
%% @doc Get a new MD5 name based UUID (RFC4122 Version 3).
%% @end
%% -------------------------------------------------------------------
v3(NsUUID, Name) ->
	euuid_server:md5(NsUUID, Name).


%% -------------------------------------------------------------------
%% @spec v4() ->
%%				UUID
%% @doc Get a new (pseudo) random UUID (RFC4122 Version 4).
%% @end
%% -------------------------------------------------------------------
v4() ->
	euuid_server:random().


%% -------------------------------------------------------------------
%% @spec v5(NsUUID, Name) ->
%%				UUID
%% @doc Get a new SHA1 name based UUID (RFC4122 Version 5).
%% @end
%% -------------------------------------------------------------------
v5(NsUUID, Name) ->
	euuid_server:sha1(NsUUID, Name).


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


%%====================================================================
%% Internal functions
%%====================================================================

%% -------------------------------------------------------------------
%% @spec unpack(UUID) ->
%%				[TL, TM, THV, CSHR, CSL, N]
%% @doc Unpack the UUID into it's parts.
%% @end
%% -------------------------------------------------------------------
unpack(<<TL:32, TM:16, THV:16, CSHR:8, CSL:8, N:48>>) ->
	[TL, TM, THV, CSHR, CSL, N].

