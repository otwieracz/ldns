%%% ldns authentication module

-module(ldns_auth).

%%% defaults, used while not in application
-define(PSK, "my-secret-pre-shared-key").
-define(HASH, sha512).
-define(MAX_CLOCK_SKEW, 30).
-define(AUTH_DISABLED, true).

-export([sign_message/1, verify_message/2]).

%%%%%%%%%
%%% utils

%%% convert any erlang term to list of integers
term_to_list(Term) ->
    binary:bin_to_list(term_to_binary(Term)).

%%% generate signature for message `Message' at specific time.
generate_signature(Message, {Date, {Hour, Min, Sec}}) ->
    HashFun = application:get_env(ldns, auth_hashfun, ?HASH),

    PskPart = application:get_env(ldns, auth_psk, ?PSK),
    TimePart = term_to_list({Date, {Hour,Min,Sec}}),
    MessagePart = term_to_list(Message),
    
    SignatureInput = PskPart++TimePart++MessagePart,

    binary:bin_to_list(base64:encode(crypto:hash(HashFun, SignatureInput))).

%%% return list of valid timestamps, taking clock skew into account (in secnods)
clock_skewed_timestamps(BaseTimestamp) ->
    MaxSkew = application:get_env(ldns, auth_maxclockskew, ?MAX_CLOCK_SKEW),
    GregorianTime = calendar:datetime_to_gregorian_seconds(BaseTimestamp),
    DiffSeconds = lists:seq(-MaxSkew,MaxSkew),
    GregorianTimestamps = [ GregorianTime+Skew || Skew <- DiffSeconds ],
    [ calendar:gregorian_seconds_to_datetime(GregorianSeconds) || GregorianSeconds <- GregorianTimestamps ].

%%%%%%%
%%% api

%%% sign message `Message'
sign_message(Message) ->
    %% check if built-in PSK is used
    PskPart = application:get_env(ldns, auth_psk, ?PSK),
    if
        PskPart == ?PSK ->
            io:format("WARNING: default pre-shared-key is used!\n")
    end,

    Signature = generate_signature(Message, calendar:universal_time()),
    {Message, Signature}.

%%% Verify if signature `Signature' is valid for message `Message' taking maximum clock skew into account.
verify_message(Message, Signature) ->
    %% Eventual check override
    Disabled = application:get_env(ldns, auth_disabled, ?AUTH_DISABLED),
    if Disabled ->
            io:format("WARNING: Message signature check is supressed!\n")
    end,

    %% Generate signature for each valid time, taking possible clock skew into account
    ValidSignatures = [ generate_signature(Message, Time) || Time <- clock_skewed_timestamps(calendar:universal_time())],
    
    %% Find if `Signature' is equal to any of `ValidSignatures' 
    Pred = fun(ValidSignature) ->
                   Signature == ValidSignature
           end,

    Disabled or lists:any(Pred, ValidSignatures).
