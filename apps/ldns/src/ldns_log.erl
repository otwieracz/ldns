-module(ldns_log).
-export([info/3, info/2]).

info(Module, Format, Arguments) ->
    FormatString = "~p ldns/~p: " ++ Format ++ "\n",
    FormatArguments = [calendar:universal_time(), Module] ++ Arguments,
    io:format(FormatString, FormatArguments).
info(Module, Format) ->
    info(Module, Format, []).

