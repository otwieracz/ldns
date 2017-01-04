-module(ldns_listener).

-export([handle_message/4]).

handle_message({ping}, SenderIp, SenderPort, LocalSocket) ->
    io:format("ping\n"),
    ldns_udp:send_term(LocalSocket, SenderIp, SenderPort, {pong});
handle_message({pong}, _SenderIp, _SenderPort, _LocalSocket) ->
    io:format("pong\n"),
    pong;
handle_message(_Message, _SenderIp, _SenderPort, _LocalSocket) ->
    not_supported.
