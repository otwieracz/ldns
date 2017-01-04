%%%-------------------------------------------------------------------
%%% @author otwieracz
%%% @copyright (C) 2016, otwieracz
%%% @doc
%%%
%%% @end
%%% Created : 2016-12-28 23:44:40.237196
%%%-------------------------------------------------------------------
-module(ldns_discovery).
-compile(export_all).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

yell(IpAddr) ->
    ldns_log:info("yelling at ", [IpAddr]),
    UdpPort = application:get_env(ldns, udp_port, ?UDP_PORT),
    DiscoveryPorts = application:get_env(ldns, discovery_ports, ?DISCOVERY_PORTS) ++ UdpPort,
    [ yell(IpAddr, Port) || Port <- DiscoveryPorts ].

yell(IpAddr, Port) ->
    ldns_udp:send(IpAddr, Port, {are_you_there_ldns}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================




-include("ldns_ns.hrl").
-include("ldns_udp.hrl").

-define(DISCOVERY_PORTS, [9091]).

%%% utils
localhost_addrs() ->
    {_Fqdn, Addrs} = ldns_ns:addrs(net_adm:localhost()),
    Addrs.

%%% IP calculations
ip_to_binary(Ip) when is_tuple(Ip) ->
    binary:decode_unsigned(list_to_binary(tuple_to_list(Ip))).

binary_to_ip(Binary) when is_integer(Binary) ->
    list_to_tuple(binary_to_list(binary:encode_unsigned(Binary))).

wildcard(Netmask) ->
    binary_to_ip(ip_to_binary(Netmask) bxor ip_to_binary(list_to_tuple(lists:duplicate(size(Netmask), 255)))).

network(Ip, Netmask) ->
    IpB = ip_to_binary(Ip),
    NetmaskB = ip_to_binary(Netmask),
    Network = IpB band NetmaskB,
    binary_to_ip(Network).
    

%%% neighbours/0,1,2
%%%  Get all neighbours of `Ip', `Netmask'
neighbours(Ip, Netmask) ->
    Network = network(Ip, Netmask),
    Wildcard = wildcard(Netmask),
    [ binary_to_ip(ip_to_binary(Network)+X) || X <- lists:seq(1,ip_to_binary(Wildcard)) ].
neighbours(Addrs) when is_list(Addrs) ->
    [ neighbours(Ip, Netmask) || {Ip,Netmask} <- Addrs ].
neighbours() ->
    Addrs = [ {Addr#addr.addr, Addr#addr.netmask} || Addr <- localhost_addrs() ],
    neighbours(Addrs).


