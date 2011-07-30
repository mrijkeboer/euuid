%% -------------------------------------------------------------------
%% euuid_util.erl - Erlang UUID utilities module
%%
%% @author Martijn Rijkeboer <martijn@bunix.org>
%% @copyright 2011 Martijn Rijkeboer
%% @version {@vsn}, {@date}, {@time}
%% @doc Erlang UUID utilities module.
%% @end
%%
%% The MIT license.
%%
%% Copyright (c) 2011 Martijn Rijkeboer
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
-module(euuid_util).
-author('Martijn Rijkeboer <martijn@bunix.org>').

%% API
-export([get_mac_addr/0]).
-export([get_timestamp/0]).
-export([new_clock_seq/0]).
-export([new_random/1]).
-export([pack/6]).
-export([unpack/1]).


%%====================================================================
%% API
%%====================================================================

%% -------------------------------------------------------------------
%% @spec get_mac_addr() ->
%%        Mac
%% @doc Get the first MAC address or generate one if non available.
%% @end
%% -------------------------------------------------------------------
get_mac_addr() ->
  try
    {ok, IfAddrs} = inet:getifaddrs(),
    <<Mac:48>> = get_mac_addr(IfAddrs),
    Mac
  catch
    _:_ ->
      <<AltMac:48>> = create_mac_addr(),
      AltMac
  end.


%% -------------------------------------------------------------------
%% @spec get_timestamp() ->
%%        Timestamp
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
%% @spec new_clock_seq() ->
%%        Number
%% @doc Generate a new clock sequence.
%% @end
%% -------------------------------------------------------------------
new_clock_seq() ->
  new_random(14).


%% -------------------------------------------------------------------
%% @spec new_random(Bits) ->
%%        Number
%% @doc Generate a new random number with a maximum size of Bits bit.
%% @end
%% -------------------------------------------------------------------
new_random(4) ->
  random:uniform(16) -1;
new_random(6) ->
  random:uniform(64) -1;
new_random(8) ->
  random:uniform(256) -1;
new_random(12) ->
  random:uniform(4096) -1;
new_random(14) ->
  random:uniform(16384) -1;
new_random(16) ->
  random:uniform(65536) -1;
new_random(24) ->
  random:uniform(16777216) -1;
new_random(32) ->
  random:uniform(4294967296) -1;
new_random(48) ->
  random:uniform(281474976710656) -1;
new_random(64) ->
  random:uniform(18446744073709551616) -1;
new_random(Bits) when Bits > 0 andalso is_integer(Bits) ->
  random:uniform(erlang:trunc(math:pow(2, Bits))) -1.


%% -------------------------------------------------------------------
%% @spec pack(TL, TM, THV, CSHR, CSL, N) ->
%%        UUID
%% @doc Pack the parts into an UUID.
%% @end
%% -------------------------------------------------------------------
pack(TL, TM, THV, CSHR, CSL, N) ->
  <<UUID:128>> = <<TL:32, TM:16, THV:16, CSHR:8, CSL:8, N:48>>,
  UUID.


%% -------------------------------------------------------------------
%% @spec unpack(UUID) ->
%%        [TL, TM, THV, CSHR, CSL, N]
%% @doc Unpack the UUID into it's parts.
%% @end
%% -------------------------------------------------------------------
unpack(<<TL:32, TM:16, THV:16, CSHR:8, CSL:8, N:48>>) ->
  [TL, TM, THV, CSHR, CSL, N].


%%====================================================================
%% Internal functions
%%====================================================================

%% -------------------------------------------------------------------
%% @spec create_mac_addr() ->
%%        Mac
%% @doc Generate a new MAC address.
%% @end
%% -------------------------------------------------------------------
create_mac_addr() ->
  Head = new_random(6),
  Rest = new_random(16),
  Nic = new_random(24),
  Local = 1,
  Multicast = 1,
  <<Head:6, Local:1, Multicast:1, Rest:16, Nic:24>>.


%% -------------------------------------------------------------------
%% @spec get_mac_addr(IfAddrs) ->
%%        Mac
%% @doc Get the first MAC address or generate one if non available.
%% @end
%% -------------------------------------------------------------------
get_mac_addr([H|T]) ->
  {_Interface, Flags} = H,
  case proplists:get_value(hwaddr, Flags) of
    [0, 0, 0, 0, 0, 0] ->
      get_mac_addr(T);
    [A, B, C, D, E, F] ->
      <<A:8, B:8, C:8, D:8, E:8, F:8>>;
    _ ->
      get_mac_addr(T)
  end;

get_mac_addr([]) ->
  create_mac_addr().

