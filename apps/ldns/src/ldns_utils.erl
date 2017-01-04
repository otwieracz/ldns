-module(ldns_utils).
-export([format_addr/2, int_to_hex/1]).

%% format_addr/2
%%  format address into more readable form
format_addr(ipv4, {A,B,C,D}) ->
    lists:concat([A,".",B,".",C,".",D]).

%% http://sacharya.com/tag/integer-to-hex-in-erlang/
int_to_hex(N) when N < 256 ->
       [hex(N div 16), hex(N rem 16)].
hex(N) when N < 10 ->
       $0+N;
hex(N) when N >= 10, N < 16 ->
       $a + (N-10).
