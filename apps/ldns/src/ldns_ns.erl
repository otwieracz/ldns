%%%-------------------------------------------------------------------
%%% @author Slawomir Gonet <slawek@otwiera.cz>
%%% @copyright (C) 2016, 
%%% @doc ldns nameserver DB
%%%
%%% Provides database:
%%% - addrs: keeps all addrs hosts, even unreachable
%%%   [{fqdn, [addr1, addr2, addr3]}]
%%%
%%% It's just a simple interface to erlang dictionary
%%%
%%% @end
%%% Created : 08 Dec 2016 by Slawomir Gonet <slawek@otwiera.cz>
%%%-------------------------------------------------------------------

-module(ldns_ns).

-behaviour(gen_server).

%% addr record
-include("ldns_ns.hrl").

%% api
-export([start_link/0, stop_link/0, ping/0, fqdns/0, foreigns/0, addrs/1, store/4, update/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {addrs, ping}).

%%% api

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop_link() ->
    gen_server:stop(?SERVER).

ping() ->
    gen_server:call(?SERVER, {ping}).

%% fqdns/0
%%  get list of all FQDNs stored in database
fqdns() ->
    gen_server:call(?SERVER, {fqdns}).

%% foreigns/0
%%  get list of all foreign FQDNs stored in database
foreigns() ->
    gen_server:call(?SERVER, {foreigns}).

%% addrs/0
%%  get list of all ADDRs stored in database for FQDN `Fqdn'
addrs(Fqdn) ->
    gen_server:call(?SERVER, {addrs, Fqdn}).

%% store_addr/3
%%  Store address `Address'/`Netmask' of family `Family' fot FQDN `Fqdn'
store(Fqdn, Family, Address, Netmask) ->
    Addr = #addr{family = Family, addr = Address, netmask = Netmask, timestamp = calendar:universal_time()},
    gen_server:call(?SERVER, {store, Fqdn, Addr}).

%% update_addrs/2
%%  Update (replace) addresses for FQDN `Fqdn' with `Addresses' (a list of #addr records)
update(Fqdn, Addresses) when is_list(Addresses) ->
    gen_server:call(?SERVER, {update, Fqdn, Addresses}).

%%% gen_server callbacks

init([]) ->
    ldns_log:info(?MODULE, "booting"),
    process_flag(trap_exit, true),
    Addrs = dict:new(),
    {ok, #state{addrs=Addrs,
                ping = 0}}.

handle_call({ping}, From, State) ->
    NewState = #state{addrs = State#state.addrs,
                      ping = State#state.ping+1},
    Reply = {pong, NewState#state.ping, From},
    {reply, Reply, NewState};
handle_call({fqdns}, _From, State) ->
    Reply = dict:fetch_keys(State#state.addrs),
    {reply, Reply, State};
handle_call({foreigns}, _From, State) ->
    Reply = exclude_own(dict:fetch_keys(State#state.addrs)),
    {reply, Reply, State};
handle_call({addrs, Fqdn}, _From, State) ->
    FqdnAddrs = find_addr(Fqdn, State#state.addrs),
    {reply, {Fqdn, FqdnAddrs}, State};
handle_call({store, Fqdn, Addr = #addr{}}, _From, State) ->
    NewAddrs = store_addr(Fqdn, Addr, State#state.addrs),
    {reply, {ok, Fqdn}, update_state_addrs(NewAddrs, State)};
handle_call({update, Fqdn, Addrs}, _From, State) ->
    {reply, {ok, Fqdn}, update_state_addrs(Fqdn, Addrs, State)};
handle_call(_Request, _From, State) ->
    Reply = invalid_call,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% utils

%% pushnew_addrs
%%  Push `NewAddr' to `OldAddrs' (if does not exists). Sounds like
%%  overkill, but might be useful if record `addr' gets extended
pushnew_addrs(OldAddrs, NewAddr = #addr{}) ->
    Pred = fun(Addr = #addr{}) ->
                   not ((Addr#addr.family == NewAddr#addr.family) and (Addr#addr.addr == NewAddr#addr.addr))
           end,
    Filtered = lists:filter(Pred, OldAddrs),
    Filtered ++ [NewAddr].

%% store_addr
%%  Store address `Addr' in dict `Dict' with `Fqdn' as a key
% look for entry in dict
store_addr(Fqdn, Addr = #addr{}, Dict) ->
    store_addr(Fqdn, Addr, Dict, dict:find(Fqdn, Dict)).
% handle case when there are already addresses for `Fqdn'
store_addr(Fqdn, Addr = #addr{}, Dict, {ok, OldAddrs}) ->
    NewAddrs = pushnew_addrs(OldAddrs, Addr),
    update_addrs(Fqdn, NewAddrs, Dict);
% new fqdn (dict:find returned `error')
store_addr(Fqdn, Addr = #addr{}, Dict, error) ->
    update_addrs(Fqdn, [Addr], Dict).

%% update_addrs
%%  Replace addrs for `Fqdn' in dictionary `Dict' with `NewAddrs'
update_addrs(Fqdn, NewAddrs, Dict) ->
    dict:store(Fqdn, NewAddrs, Dict).

%% update_state_addrs
%%  Replace `addrs' field of state `State' with `NewAddrs'
update_state_addrs(NewAddrs, State) ->
    State#state{addrs = NewAddrs}.
update_state_addrs(Fqdn, Addrs, State) ->
    NewAddrs = update_addrs(Fqdn, Addrs, State#state.addrs),
    update_state_addrs(NewAddrs, State).

%% find_addr
%%  find all addresses saved for fqdn `Fqdn' in dict `Dict'
find_addr(Fqdn, Dict) ->
    find_addr(dict:find(Fqdn, Dict)).
find_addr({ok, Addrs}) ->
    Addrs;
find_addr(error) ->
    [].

%%% exclude_own/1
%%%  exclude own addresses from list of all fqdns
exclude_own(FqdnsList) when is_list(FqdnsList) ->
    Pred = fun(Fqdn) ->
                   Fqdn /= net_adm:localhost()
           end,
    lists:filter(Pred, FqdnsList).


