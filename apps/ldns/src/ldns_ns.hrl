% ldns_ns records
%
-record(addr, {family, addr, netmask, distance = unreachable, timestamp}).


