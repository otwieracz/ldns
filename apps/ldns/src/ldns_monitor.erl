%%%-------------------------------------------------------------------
%%% @author otwieracz
%%% @copyright (C) 2016, otwieracz
%%% @doc
%%%
%%% @end
%%% Created : 2016-12-22 22:47:01.104555
%%%-------------------------------------------------------------------
-module(ldns_monitor).
-export([update_fqdn/1, update/0, start_monitor/0, start_link/0]).

%%% include `#addr' record
-include("ldns_ns.hrl").

-record(ping_response, {host,addr,elapsed}).

-define(MONITOR_INTERVAL, 180).

%%% From gen_icmp:
%%%
%%% Responses = [ Response ]
%%% Response = {ok, Host, Address, ReplyAddr, Details, Payload}
%%% | {error, ICMPError, Host, Address, ReplyAddr, Details, Payload}
%%% | {error, Error, Host, Address}
%%% Details = {Id, Sequence, TTL, Elapsed}
%%% Elapsed = int() | undefined
parse_ping_response(Responses) when is_list(Responses) ->
    [ parse_ping_response(Response) || Response <- Responses ];
parse_ping_response({ok, Host, Address, _ReplyAddr, Details, _Payload}) ->
    parse_ping_response({details, Host, Address, Details});
parse_ping_response({details, Host, Address, Details}) ->
    {_Id, _Sequence, _TTL, Elapsed} = Details,
    #ping_response{host=Host, addr=Address, elapsed=Elapsed};
parse_ping_response(_) ->
    unreachable.

%%% ping_addr/1
%%%  ping `#addr' and return `#ping_response' or `unreachable'
ping_addr(#addr{} = Addr) ->
    Responses = gen_icmp:ping(Addr#addr.addr),
    parse_ping_response(Responses).

%%% distance/1
%%%  get host distance from `#ping_response'
distance(#ping_response{} = PingResponse) ->
    PingResponse#ping_response.elapsed;
distance(unreachable) ->
    unreachable.

%%% update_addr/1
%%%  update `distance' field in `#addr'
update_addr(#addr{} = Addr) ->
    [PingResponse|_] = ping_addr(Addr), %% ignore everything but first response
    Distance = distance(PingResponse),
    NewAddr = Addr#addr{distance = Distance},
    NewAddr.

%%% update addrs of `Fqdn' about their distance
update_fqdn(Fqdn) ->
    %% ldns_log:info(?MODULE, "fqdn[~s]: updating", [Fqdn]),
    {Fqdn, Addrs} = ldns_ns:addrs(Fqdn),
    NewAddrs = [ update_addr(Addr) || Addr <- Addrs ],
    %% ldns_log:info(?MODULE, "fqdn[~s]: finished", [Fqdn]),
    ldns_ns:update(Fqdn, NewAddrs).

%%% get_fqdns/0
%%%  get all fqdns from NS
get_fqdns() ->
    try
        ldns_ns:fqdns()
    catch
        _Exception:_Reason -> []
    end.

%%% update/0
%%%  Update `distance' of all addresses from NS
update() ->
    ldns_log:info(?MODULE, "updating addrs"),
    Fqdns = get_fqdns(),
    Response = [ update_fqdn(Fqdn) || Fqdn <- Fqdns ],
    ldns_hostsfile:update(),
    ldns_log:info(?MODULE, "updating finished"),
    Response.

%%% start_monitor/0
%%%  start monitor loop
start_monitor() ->
    Interval = application:get_env(ldns, monitor_interval, ?MONITOR_INTERVAL)*1000,
    update(),
    timer:sleep(Interval),
    start_monitor().

%%% start_link/0
%%%  start monitor and link
start_link() ->
    ldns_log:info(?MODULE, "booting"),
    Pid = spawn_link(?MODULE, start_monitor, []),
    try
        register(?MODULE, Pid),
        {ok, Pid}
    catch
        _Exception:_Reason -> error
    end.

