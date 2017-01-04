%%%-------------------------------------------------------------------
%%% @author Slawomir Gonet <slawek@otwiera.cz>
%%% @copyright (C) 2016, 
%%% @doc
%%%
%%% @end
%%% Created : 14 Dec 2016 by Slawomir Gonet <slawek@otwiera.cz>
%%%-------------------------------------------------------------------
-module(ldns_udp).

-behaviour(gen_server).

-include("ldns_udp.hrl").

%% API
-export([start_link/0, stop_link/0, send/3, yell/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(HOST, {127,0,0,1}).

-record(state, {socket}).

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

stop_link() ->
    gen_server:stop(?SERVER).

send(Ip, Port, Term) ->
    {ok, Socket} = gen_udp:open(0, [binary, {active, once}]),
    gen_server:cast(?SERVER, {send, Socket, Ip, Port,Term}),
    gen_udp:close(Socket).

yell(Ip, Port) ->
    ldns_udp:send(Ip, Port, {are_you_there_ldns}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%-------------------------------------------------------------------- %% @private
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
    ldns_log:info(?MODULE, "booting"),
    process_flag(trap_exit, true),
    Port = application:get_env(ldns, udp_port, ?UDP_PORT),
    {ok, Socket} = gen_udp:open(Port, [binary, {active, once}]),
    {ok, #state{socket = Socket}}.

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
    Reply = not_supported,
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
handle_cast({send, Socket, Ip, Port, Message}, State) ->
    send_term(Socket, Ip, Port, Message),
    {noreply, State};
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
handle_info(Info, State) ->
    {udp, Socket, SenderIp, SenderPort, Message} = Info,
    Term = binary_to_term(Message),
    handle_term(Socket, SenderIp, SenderPort, Term),
    % set active again to parse another message
    inet:setopts(State#state.socket, [binary, {active, once}]),
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
terminate(_Reason, State) ->
    gen_udp:close(State#state.socket),
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

%%% Handle received term
handle_term(LocalSocket, SenderIp, SenderPort, {Message, Signature}) ->
    SignatureOk = ldns_auth:verify_message(Message, Signature),
    if SignatureOk ->
            ldns_listener:handle_message(Message, SenderIp, SenderPort, LocalSocket)
    end;
handle_term(_LocalSocket, _SenderIp, _SenderPort, _Term) ->
    not_supported.

%%% Send term to IP and port
send_term(Socket, Ip, Port, Message) ->
    Signature = ldns_auth:sign_message(Message),
    BinaryToSend = term_to_binary({Message, Signature}),
    gen_udp:send(Socket, Ip, Port, BinaryToSend).

