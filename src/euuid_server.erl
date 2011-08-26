%% -------------------------------------------------------------------
%% euuid_server.erl - Erlang UUID server module
%%
%% @author Martijn Rijkeboer <martijn@bunix.org>
%% @copyright 2010 Martijn Rijkeboer
%% @version {@vsn}, {@date}, {@time}
%% @doc Erlang UUID server module
%% @end
%%
%% The MIT license.
%%
%% Copyright (c) 2010 Martijn Rijkeboer
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

%% State record.
-record(state, {mac, timestamp, clock_seq}).


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
  Mac = euuid_util:gen_mac_addr(),
  Timestamp = euuid_util:get_timestamp(),
  ClockSeq = euuid_util:new_clock_seq(),
  {ok, #state{mac = Mac, timestamp = Timestamp, clock_seq = ClockSeq}}.


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
  NewState = update_state(State),
  Mac = NewState#state.mac,
  Timestamp = NewState#state.timestamp,
  ClockSeq = NewState#state.clock_seq,

  <<TH:12, TM:16, TL:32>> = <<Timestamp:60>>,
  V = 1,
  <<THV:16>> = <<V:4, TH:12>>,
  <<CSH:6, CSL:8>> = <<ClockSeq:14>>,
  R = 2#10,
  <<CSHR:8>> = <<R:2, CSH:6>>,
  UUID = euuid_util:pack(TL, TM, THV, CSHR, CSL, Mac),
  {reply, UUID, NewState};

handle_call({md5, NsUUID, Name}, _From, State) ->
  Data = list_to_binary([<<NsUUID:128>>, Name]),
  <<MD5:128>> = crypto:md5(Data),
  <<TL:32, TM:16, _:4, TH:12, _:2, CSH:6, CSL:8, N:48>> = <<MD5:128>>,
  V = 3,
  <<THV:16>> = <<V:4, TH:12>>,
  R = 2#10,
  <<CSHR:8>> = <<R:2, CSH:6>>,
  UUID = euuid_util:pack(TL, TM, THV, CSHR, CSL, N),
  {reply, UUID, State};

handle_call(random, _From, State) ->
  <<TH:12, TM:16, TL:32, CSH:6, CSL:8, N:48, _:6>> = crypto:rand_bytes(16),
  V = 4,
  <<THV:16>> = <<V:4, TH:12>>,
  R = 2#10,
  <<CSHR:8>> = <<R:2, CSH:6>>,
  UUID = euuid_util:pack(TL, TM, THV, CSHR, CSL, N),
  {reply, UUID, State};

handle_call({sha1, NsUUID, Name}, _From, State) ->
  Data = list_to_binary([<<NsUUID:128>>, Name]),
  <<Sha1:160>> = crypto:sha(Data),
  <<TL:32, TM:16, _:4, TH:12, _:2, CSH:6, CSL:8, N:48, _:32>> = <<Sha1:160>>,
  V = 5,
  <<THV:16>> = <<V:4, TH:12>>,
  R = 2#10,
  <<CSHR:8>> = <<R:2, CSH:6>>,
  UUID = euuid_util:pack(TL, TM, THV, CSHR, CSL, N),
  {reply, UUID, State};

handle_call(time_custom, _From, State) ->
  NewState = update_state(State),
  Mac = NewState#state.mac,
  Timestamp = NewState#state.timestamp,
  ClockSeq = NewState#state.clock_seq,

  <<TL:32, TM:16, TH:12>> = <<Timestamp:60>>,
  V = 15,
  <<THV:16>> = <<V:4, TH:12>>,
  <<CSH:6, CSL:8>> = <<ClockSeq:14>>,
  R = 2#10,
  <<CSHR:8>> = <<R:2, CSH:6>>,
  UUID = euuid_util:pack(TL, TM, THV, CSHR, CSL, Mac),
  {reply, UUID, NewState};

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
%% @spec update_state(State) ->
%%        NewState
%% @doc Update the state with new values.
%% @end
%% -------------------------------------------------------------------
update_state(State) ->
  Mac = State#state.mac,
  PrevTimestamp = State#state.timestamp,
  CurrTimestamp = euuid_util:get_timestamp(),
  ClockSeq = case PrevTimestamp < CurrTimestamp of
    true -> State#state.clock_seq;
    false -> euuid_util:incr_clock_seq(State#state.clock_seq)
  end,
  #state{mac = Mac, timestamp = CurrTimestamp, clock_seq = ClockSeq}.
  
