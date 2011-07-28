%% -------------------------------------------------------------------
%% euuid_sup.erl - Erlang UUID supervisor module
%%
%% @author Martijn Rijkeboer <martijn@bunix.org>
%% @copyright 2010 Martijn Rijkeboer
%% @version {@vsn}, {@date}, {@time}
%% @doc Erlang UUID supervisor module
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
-module(euuid_sup).
-author('Martijn Rijkeboer <martijn@bunix.org>').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

%% -------------------------------------------------------------------
%% @spec start_link() ->
%%        {ok, Pid} |
%%        ignore |
%%        {error, Error}
%% @doc Starts the supervisor
%% @end
%% -------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% -------------------------------------------------------------------
%% @spec init(Args) ->
%%        {ok, {SupFlags, [ChildSpec]}} |
%%        ignore |
%%        {error, Error}
%% @doc Get the restart strategy for the children of the supervisor
%% @end
%% -------------------------------------------------------------------
init([]) ->
  Server = ?CHILD(euuid_server, worker),
  {ok, { {one_for_one, 5, 10}, [Server]} }.

