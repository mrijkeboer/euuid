%% -------------------------------------------------------------------
%% @copyright Martijn P. Rijkeboer
%% @author Martijn P. Rijkeboer <martijn@bunix.org>
%% @version {@vsn}, {@date}, {@time}
%% @doc Universally Unique IDentifier application module
%% @end
%% -------------------------------------------------------------------
-module(euuid_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @spec start(Type, StartArgs) ->
%%				{ok, Pid} |
%%				{ok, Pid, State} |
%%				{error, Reason}
%% @doc This function is called when an application is started using
%% application:start/1,2, and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
	euuid_sup:start_link().


%%--------------------------------------------------------------------
%% @spec stop(State) -> void()
%% @doc This function is called whenever an application has stopped.
%% It is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored. 
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
	ok.
