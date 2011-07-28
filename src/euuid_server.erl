%% -------------------------------------------------------------------
%% euuid_server.erl - Erlang UUID server module
%%
%% @author Martijn P. Rijkeboer <martijn@bunix.org>
%% @copyright 2010 Martijn P. Rijkeboer
%% @version {@vsn}, {@date}, {@time}
%% @doc Erlang UUID server module
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
-module(euuid_server).
-author('Martijn Rijkeboer <martijn@bunix.org>').

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([md5/2, sha1/2]).
-export([random/0]).
-export([time_custom/0, time_mac/0]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
  ]).

-define(SERVER, ?MODULE).


%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @spec start_link() ->
%%        {ok,Pid} |
%%        {error,Error} |
%%        ignore
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% -------------------------------------------------------------------
%% @spec time_mac() ->
%%        UUID
%% @doc Get a new time and MAC based UUID (RFC4122 Version 1).
%% @end
%% -------------------------------------------------------------------
time_mac() ->
  gen_server:call(?SERVER, time_mac).


%% -------------------------------------------------------------------
%% @spec md5(NsUUID, Name) ->
%%        UUID
%% @doc Get a new MD5 name based UUID (RFC4122 Version 3).
%% @end
%% -------------------------------------------------------------------
md5(NsUUID, Name) ->
  gen_server:call(?SERVER, {md5, NsUUID, Name}).


%% -------------------------------------------------------------------
%% @spec random() ->
%%        UUID
%% @doc Get a new (pseudo) random UUID (RFC4122 Version 4).
%% @end
%% -------------------------------------------------------------------
random() ->
  gen_server:call(?SERVER, random).


%% -------------------------------------------------------------------
%% @spec sha1(NsUUID, Name) ->
%%        UUID
%% @doc Get a new SHA1 name based UUID (RFC4122 Version 5).
%% @end
%% -------------------------------------------------------------------
sha1(NsUUID, Name) ->
  gen_server:call(?SERVER, {sha1, NsUUID, Name}).


%% -------------------------------------------------------------------
%% @spec time_custom() ->
%%        UUID
%% @doc Get a new time and MAC based UUID with modified timestamp
%%  layout to allow sorting on the UUID's date of creation.
%% @end
%% -------------------------------------------------------------------
time_custom() ->
  gen_server:call(?SERVER, time_custom).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) ->
%%        {ok, State} |
%%        {ok, State, Timeout} |
%%        ignore |
%%        {stop, Reason}
%% @doc Initiates the server
%% @end
%%--------------------------------------------------------------------
init([]) ->
  Dict0 = dict:new(),
  Dict1 = set_timestamp(new_timestamp(), Dict0),
  Dict2 = set_clock_seq(new_clock_seq(), Dict1),
  Dict3 = set_mac(new_mac(), Dict2),
  {ok, Dict3}.


%%--------------------------------------------------------------------
%% @spec handle_call(Request, From, State) ->
%%        {reply, Reply, State} |
%%        {reply, Reply, State, Timeout} |
%%        {noreply, State} |
%%        {noreply, State, Timeout} |
%%        {stop, Reason, Reply, State} |
%%        {stop, Reason, State}
%% @doc Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call(time_mac, _From, State) ->
  {data, Timestamp, ClockSeq, Mac, State1} = get_data(State),
  <<TH:12, TM:16, TL:32>> = <<Timestamp:60>>,
  V = 1,
  <<THV:16>> = <<V:4, TH:12>>,
  <<CSH:6, CSL:8>> = <<ClockSeq:14>>,
  R = 2#10,
  <<CSHR:8>> = <<R:2, CSH:6>>,
  UUID = pack(TL, TM, THV, CSHR, CSL, Mac),
  {reply, UUID, State1};

handle_call({md5, NsUUID, Name}, _From, State) ->
  Data = list_to_binary([<<NsUUID:128>>, Name]),
  <<MD5:128>> = crypto:md5(Data),
  <<TL:32, TM:16, _:4, TH:12, _:2, CSH:6, CSL:8, N:48>> = <<MD5:128>>,
  V = 3,
  <<THV:16>> = <<V:4, TH:12>>,
  R = 2#10,
  <<CSHR:8>> = <<R:2, CSH:6>>,
  UUID = pack(TL, TM, THV, CSHR, CSL, N),
  {reply, UUID, State};

handle_call(random, _From, State) ->
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
  UUID = pack(TL, TM, THV, CSHR, CSL, N),
  {reply, UUID, State};

handle_call({sha1, NsUUID, Name}, _From, State) ->
  Data = list_to_binary([<<NsUUID:128>>, Name]),
  <<Sha1:160>> = crypto:sha(Data),
  <<TL:32, TM:16, _:4, TH:12, _:2, CSH:6, CSL:8, N:48, _:32>> = <<Sha1:160>>,
  V = 5,
  <<THV:16>> = <<V:4, TH:12>>,
  R = 2#10,
  <<CSHR:8>> = <<R:2, CSH:6>>,
  UUID = pack(TL, TM, THV, CSHR, CSL, N),
  {reply, UUID, State};

handle_call(time_custom, _From, State) ->
  {data, Timestamp, ClockSeq, Mac, State1} = get_data(State),
  <<TL:32, TM:16, TH:12>> = <<Timestamp:60>>,
  V = 15,
  <<THV:16>> = <<V:4, TH:12>>,
  <<CSH:6, CSL:8>> = <<ClockSeq:14>>,
  R = 2#10,
  <<CSHR:8>> = <<R:2, CSH:6>>,
  UUID = pack(TL, TM, THV, CSHR, CSL, Mac),
  {reply, UUID, State1};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.


%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) ->
%%        {noreply, State} |
%%        {noreply, State, Timeout} |
%%        {stop, Reason, State}
%% @doc Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.


%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) ->
%%        {noreply, State} |
%%        {noreply, State, Timeout} |
%%        {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.


%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) ->
%%        void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _state) ->
  ok.


%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) ->
%%        {ok, NewState}
%% @doc Convert process state when code is changed.
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

%% -------------------------------------------------------------------
%% @spec get_data(State) ->
%%        {data, Timestamp, ClockSeq, Mac, State}
%% @doc get the persistent data.
%% @end
%% -------------------------------------------------------------------
get_data(State) ->
  NewTimestamp = new_timestamp(),
  case get_timestamp(State) of
    {ok, Timestamp} when Timestamp > NewTimestamp ->
      NewClockSeq = new_clock_seq();
    {ok, _Timestamp} ->
      case get_clock_seq(State) of
        {ok, ClockSeq} ->
          NewClockSeq = ClockSeq;
        error ->
          NewClockSeq = new_clock_seq()
      end;
    error ->
      NewClockSeq = new_clock_seq()
  end,

  case get_mac(State) of
    {ok, Mac} ->
      NewMac = Mac;
    error ->
      NewMac = new_mac()
  end,

  State1 = set_timestamp(NewTimestamp, State),
  State2 = set_clock_seq(NewClockSeq, State1),
  State3 = set_mac(NewMac, State2),
  {data, NewTimestamp, NewClockSeq, NewMac, State3}.


%% -------------------------------------------------------------------
%% @spec get_mac(State) ->
%%        {ok, Mac} |
%%        error
%% @doc get the MAC-address.
%% @end
%% -------------------------------------------------------------------
get_mac(State) ->
  dict:find(mac, State).


%% -------------------------------------------------------------------
%% @spec new_mac() ->
%%        Mac
%% @doc Generate a new MAC-address.
%% @end
%% -------------------------------------------------------------------
new_mac() ->
  Head = new_random(6),
  Rest = new_random(16),
  Nic = new_random(24),
  Local = 1,
  Multicast = 1,
  <<Mac:48>> = <<Head:6, Local:1, Multicast:1, Rest:16, Nic:24>>,
  Mac.


%% -------------------------------------------------------------------
%% @spec set_mac(Mac, State) ->
%%        State
%% @doc Set the MAC-address.
%% @end
%% -------------------------------------------------------------------
set_mac(Mac, State) ->
  dict:store(mac, Mac, State).


%% -------------------------------------------------------------------
%% @spec new_random(Bits) ->
%%        Number
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
%% @spec get_timestamp(State) ->
%%        {ok, Timestamp} |
%%        error
%% @doc Get the last timestamp as defined in RFC4122.
%% @end
%% -------------------------------------------------------------------
get_timestamp(State) ->
  dict:find(timestamp, State).


%% -------------------------------------------------------------------
%% @spec new_timestamp() ->
%%        Timestamp
%% @doc Get the current timestamp as defined in RFC4122.
%% @end
%% -------------------------------------------------------------------
new_timestamp() ->
  Now = {_MegaSecs,_Secs,MicroSecs} = now(),
  Utc = calendar:now_to_universal_time(Now),
  Epoch = calendar:datetime_to_gregorian_seconds({{1582,10,15}, {0,0,0}}),
  Seconds = calendar:datetime_to_gregorian_seconds(Utc) - Epoch,
  Timestamp = ((Seconds * 1000000) + MicroSecs) * 10,
  Timestamp.


%% -------------------------------------------------------------------
%% @spec set_timestamp(Timestamp, State) ->
%%        State
%% @doc Set the clock sequence.
%% @end
%% -------------------------------------------------------------------
set_timestamp(Timestamp, State) ->
  dict:store(timestamp, Timestamp, State).


%% -------------------------------------------------------------------
%% @spec get_clock_seq(State) ->
%%        {ok, ClockSeq} |
%%        error
%% @doc Get the clock sequence.
%% @end
%% -------------------------------------------------------------------
get_clock_seq(State) ->
  dict:find(clock_seq, State).


%% -------------------------------------------------------------------
%% @spec new_clock_seq() ->
%%        Number
%% @doc Generate a new clock sequence.
%% @end
%% -------------------------------------------------------------------
new_clock_seq() ->
  new_random(14).


%% -------------------------------------------------------------------
%% @spec set_clock_seq(ClockSeq, State) ->
%%        State
%% @doc Set the clock sequence.
%% @end
%% -------------------------------------------------------------------
set_clock_seq(ClockSeq, State) ->
  dict:store(clock_seq, ClockSeq, State).


%% -------------------------------------------------------------------
%% @spec pack(TL, TM, THV, CSHR, CSL, N) ->
%%        UUID
%% @doc Pack the parts into an UUID.
%% @end
%% -------------------------------------------------------------------
pack(TL, TM, THV, CSHR, CSL, N) ->
  <<UUID:128>> = <<TL:32, TM:16, THV:16, CSHR:8, CSL:8, N:48>>,
  UUID.

