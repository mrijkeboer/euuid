%% -------------------------------------------------------------------
%% @copyright Martijn P. Rijkeboer
%% @author Martijn P. Rijkeboer <martijn@bunix.org>
%% @version {@vsn}, {@date}, {@time}
%% @doc Universally Unique IDentifier supervisor
%% @end
%% -------------------------------------------------------------------
-module(euuid_sup).

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
%%				{ok, Pid} |
%%				ignore |
%%				{error, Error}
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
%%				{ok, {SupFlags, [ChildSpec]}} | 
%%				ignore |
%%				{error, Error}
%% @doc Get the restart strategy for the children of the supervisor
%% @end
%% -------------------------------------------------------------------
init([]) ->
	{ok, { {one_for_one, 5, 10}, []} }.

