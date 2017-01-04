-module(ldns_hostsfile).
-compile(export_all).

-include("ldns_ns.hrl").

-define(HOSTS_FILE, "/etc/hosts").
-define(HOSTS_TAG, "# ldns").

get_hosts_file_path() ->
    application:get_env(ldns, hosts_file, ?HOSTS_FILE).
get_tag() ->
    application:get_env(ldns, hosts_tag, ?HOSTS_TAG).

sort_addrs(Fqdn) ->
    {Fqdn, Addrs} = ldns_ns:addrs(Fqdn),
    Pred = fun(A, B) ->
                   DistanceA = A#addr.distance,
                   DistanceB = B#addr.distance,
                   DistanceA /= unreachable andalso ((DistanceB == unreachable) orelse
                                                     (DistanceA =< DistanceB))
           end,
    lists:sort(Pred, Addrs).

fqdn_addr(Fqdn) ->
    Sorted = sort_addrs(Fqdn),
    try
        [Best|_] = Sorted,
        Best
    catch
        _Exception:_Reason -> unknown
    end.

known_addrs() ->
    Fqdns = ldns_ns:fqdns(),
    Known = [ {Fqdn, fqdn_addr(Fqdn)} || Fqdn <- Fqdns ],
    Known.

read_hosts_file() ->
    HostsFilePath = get_hosts_file_path(),
    read_hosts_file(file:read_file(HostsFilePath)).

read_hosts_file({ok, Data}) ->
    Binary = binary:split(Data, [<<"\n">>], [global]),
    Strings = [ binary:bin_to_list(Bin) || Bin <- Binary ],
    Strings.

%%% grep_hosts_file/0
%%%  Remove all lines matching `TAG' from 
grep_hosts_file() ->
    HostsFile = read_hosts_file(),
    Tag = get_tag(),
    %% remove all lines matching `Tag'
    Pred = fun(Elem) ->
                   case re:run(Elem, Tag, []) of
                       {match, _Captured} ->
                           false;
                       nomatch ->
                           true
                   end
           end,
    lists:filter(Pred, HostsFile).

%%% prepare_hosts_entries/2
%%%  generate hosts entries from `#addr's
prepare_hosts_entries(Fqdn, #addr{} = Addr) ->
    inet:ntoa(Addr#addr.addr)++" "++Fqdn++" "++get_tag().

%%% prepare_hosts_entries/0
%%%  generate hosts entries from all known entries
prepare_hosts_entries() ->
    KnownAddrs = known_addrs(),
    KnownEntries = [ prepare_hosts_entries(Fqdn, Addr) || {Fqdn, Addr} <- KnownAddrs ],
    [get_tag()++" begin"] ++ KnownEntries ++ [get_tag()++" end"].


write_hosts_file(Lines) when is_list(Lines) ->
    HostsFilePath = get_hosts_file_path(),
    LineSep = io_lib:nl(),
    Data = string:join(Lines, LineSep),
    file:write_file(HostsFilePath, Data).

update() ->
    NewContents = grep_hosts_file() ++ prepare_hosts_entries(),
    write_hosts_file(NewContents).

%%% To implement
%%% - convert addr to text format (both ipv4 and ipv6) inet:ntoa, inet_parse:address
%%% - grep -v /etc/hosts of some identifier (-define and env value)
%%% - update_hosts/0
%%% - format_addr/1 -> return string "IP hostname"
%%%
