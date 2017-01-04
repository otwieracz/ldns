%%%-------------------------------------------------------------------
%% @doc ldns top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ldns_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ldns_log:info(?MODULE, "booting"),
    SupFlags = {one_for_one, 1, 5},
    ChildSpecs = [#{id => ldns_ns,
                    start => {ldns_ns, start_link, []}},
                  #{id => ldns_network,
                    start => {ldns_network, start_link, []}},
                  #{id => ldns_localhost,
                    start => {ldns_localhost, start_link, []}}
                 ],
    {ok, {SupFlags, ChildSpecs} }.

%%====================================================================
%% Internal functions
%%====================================================================
