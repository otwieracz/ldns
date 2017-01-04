-module(ldns_localhost).
-export([store/0, start_localhost/0, start_link/0]).

-record(addr, {ifname, family, addr, netmask}).

-define(LOCALHOST_INTERVAL, 360).

%%% store/0
%%%  store localhost addresses to LDNS_NS
store() ->
    Addrs = get_addrs(),
    [ store_addr(Addr) || Addr <- Addrs ].

%% store_addr/3
%%  store address 
store_addr(#addr{} = Addr) ->
    ldns_ns:store(net_adm:localhost(), Addr#addr.family, Addr#addr.addr, Addr#addr.netmask).

%%% addr_family/1
%%%  guess address family
addr_family(4) ->
    ipv4;
addr_family(8) ->
    ipv6;
addr_family(Addr) ->
    addr_family(size(Addr)).

%%% extract_addrs_from_props/2
%%%  extract addrs (Addr, Netmask) from Props
extract_addr_from_props(Ifname, Props) ->
    Addrs = [ Addr || {addr, Addr} <- Props ],
    Families = [ addr_family(Addr) || {addr, Addr} <- Props ],
    Netmasks = [ Netmask || {netmask, Netmask} <- Props ],
    Zipped = lists:zip3(Addrs, Families, Netmasks),
    [ #addr{ifname = Ifname, addr = Addr, family = Family, netmask = Netmask} || {Addr, Family, Netmask} <- Zipped ].

%% get_addr/1
%%  Extract addresses from [{ifname, props}, ...] structure and ignore addresses for lo
get_addr({"lo", _Props}) -> [];
get_addr({Ifname, Props}) ->
    IfAddrs = extract_addr_from_props(Ifname, Props),
    IfAddrs.

%% get_addrs/0
%%  get all addresses for current host in format {family, interface, {IP,ADDRESS,...}
get_addrs() ->
    {ok, Addrs} = inet:getifaddrs(),
    lists:flatten([ get_addr(A) || A <- Addrs ]).

%%% start_locahost/0
%%%  start localhost address scanner
start_localhost() ->
    Interval = application:get_env(ldns, localhost_interval, ?LOCALHOST_INTERVAL)*1000,
    store(),
    timer:sleep(Interval),
    start_localhost().

%%% start_link/0
%%%  start monitor and link
start_link() ->
    ldns_log:info(?MODULE, "booting"),
    Pid = spawn_link(?MODULE, start_localhost, []),
    try
        register(?MODULE, Pid),
        {ok, Pid}
    catch
        _Exception:_Reason -> error
    end.

