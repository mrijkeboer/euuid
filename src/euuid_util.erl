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
-export([gen_mac_addr/0]).
-export([get_timestamp/0]).
-export([incr_clock_seq/1]).
-export([new_clock_seq/0]).
-export([new_random/1]).
-export([pack/6]).
-export([unpack/1]).


%%====================================================================
%% API
%%====================================================================

%% -------------------------------------------------------------------
%% @spec gen_mac_addr() ->
%%        Mac
%% @doc Generate a MAC address as defined in RFC 4122 section 4.5.
%% @end
%% -------------------------------------------------------------------
gen_mac_addr() ->
  Head = new_random(6),
  Rest = new_random(16),
  Nic = new_random(24),
  Local = 1,
  Multicast = 1,
  <<Mac:48>> = <<Head:6, Local:1, Multicast:1, Rest:16, Nic:24>>,
  Mac.


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
%% @spec incr_clock_seq(ClockSeq) ->
%%        Number
%% @doc Increment the clock sequence.
%% @end
%% -------------------------------------------------------------------
incr_clock_seq(ClockSeq) when ClockSeq > 0 ->
  (ClockSeq + 1) rem 16384;

incr_clock_seq(ClockSeq) when ClockSeq < 0 ->
  ((ClockSeq * -1) + 1) rem 16384;

incr_clock_seq(0) ->
  1.


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
  crypto:rand_uniform(0, 16);
new_random(6) ->
  crypto:rand_uniform(0, 64);
new_random(8) ->
  crypto:rand_uniform(0, 256);
new_random(12) ->
  crypto:rand_uniform(0, 4096);
new_random(14) ->
  crypto:rand_uniform(0, 16384);
new_random(16) ->
  crypto:rand_uniform(0, 65536);
new_random(24) ->
  crypto:rand_uniform(0, 16777216);
new_random(32) ->
  crypto:rand_uniform(0, 4294967296);
new_random(48) ->
  crypto:rand_uniform(0, 28147497671065);
new_random(64) ->
  crypto:rand_uniform(0, 18446744073709551616);
new_random(Bits) when Bits > 0 andalso is_integer(Bits) ->
  crypto:rand_uniform(0, erlang:trunc(math:pow(2, Bits))).


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

