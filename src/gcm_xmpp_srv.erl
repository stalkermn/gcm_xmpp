%%%-------------------------------------------------------------------
%%% @author protoj
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Авг. 2015 11:23
%%%-------------------------------------------------------------------
-module(gcm_xmpp_srv).
-author("protoj").

-behaviour(gen_server).

-include("gcm_xmpp.hrl").
-include_lib("exmpp/include/exmpp.hrl").

%% API
-export([start_link/4]).
-export([stop/1]).
%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(gcm_error, {}).
-record(gcm_nack, {}).
-record(gcm_ack, {}).
-record(gcm_ok, {}).

-record(wait_deliv, {
    message_id :: list() | binary(),
    from :: {pid(), Tag :: term()}
}).
-record(state, {
    session :: pid(),
    wait_deliv_status :: ets:tab(),
    connection_cfg :: list( string() | integer() )
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(GCMSenderId, ApiKey, GcmEndpoint, GcmEndpointPort) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()} when

    GCMSenderId :: string(),
    ApiKey :: string(),
    GcmEndpoint :: string(),
    GcmEndpointPort :: integer().
start_link(GCMSenderId, ApiKey, GcmEndpoint, GcmEndpointPort) ->
    gen_server:start_link(?MODULE, [GCMSenderId, ApiKey, GcmEndpoint, GcmEndpointPort], []).

stop(Session)->
    gen_server:call(Session, stop).

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
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([GCMSenderId, ApiKey, GcmEndpoint, GcmEndpointPort]) ->
    case establish_xmpp_connection(GCMSenderId, ApiKey, GcmEndpoint, GcmEndpointPort) of
        {ok, Session} ->
            Tid = ets:new(gcm_worker_cache, [public, set, {keypos, #wait_deliv.message_id}]),
            {ok, #state{session = Session, wait_deliv_status = Tid}};
        {error, ErrorReason} ->
            lager:error("[gcm_xmpp_srv] error establishing connection: ~p", [ErrorReason]),
            {stop, {shutdown, {connection_error, ErrorReason}}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request, From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply, NewState :: #state{}} |
    {noreply, NewState :: #state{}} |
    {stop, normal, ok, #state{}} |
    {stop, {shutdown, {unexpected_call, UnexpecterCall :: term()}}, #state{}} when

    Request :: #gcm_message{},
    Reply :: #gcm_ok{} | #gcm_ack{} | #gcm_nack{} | #gcm_error{}.

handle_call(#gcm_message{} = GcmMessage, _From, #state{session = undefined} = State) ->

    #state{
        session = undefined,
        connection_cfg = [GCMSenderId, ApiKey, GcmEndpoint, GcmEndpointPort]
    } = State,

    case establish_xmpp_connection(GCMSenderId, ApiKey, GcmEndpoint, GcmEndpointPort) of
        {ok, Session} ->
            handle_call(GcmMessage, _From, State#state{session = Session});
        {error, ErrorReason} ->
%%             TODO: change to #gcm_error{}
            {stop, {shutdown, {connection_error, ErrorReason}}, {error, ErrorReason}, State}
    end;

handle_call(#gcm_message{} = GcmMessage, From, State) ->
    case is_process_alive(State#state.session) of
        true ->
            send_gcm_push(GcmMessage, From, State);
        _ ->
            handle_call(GcmMessage, From, State#state{session = undefined})
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(UnexpecterCall, _From, State) ->
    lager:error("[~p] unexpected call in ~p: ~p", [?MODULE, self(), UnexpecterCall]),
    {stop, {shutdown, {unexpected_call, UnexpecterCall}}, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {stop, {shutdown, {unexpected_cast, term()}}, #state{}}).
handle_cast(Request, State) ->
    lager:error("[~p] unexpected cast in ~p: ~p", [?MODULE, self(), Request]),
    {stop, {shutdown, {unexpected_cast, Request}} , State}.

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
-spec(handle_info(Info :: term(), State :: #state{}) ->
    {noreply, #state{}} |
    {stop, {shutdown, {unexpected_cast, term()}}, #state{}}).
handle_info(Info, State) ->
    lager:info("[~p] info in ~p: ~p", [?MODULE, self(), Info]),
    {noreply, State}.
%% handle_info(Info, State) ->
%%     lager:error("[~p] unexpected info in ~p: ~p", [?MODULE, self(), Info]),
%%     {stop, {shutdown, {unexpected_info, Info}} , State}.

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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, State) ->
    catch exmpp_session:stop(State#state.session),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

establish_xmpp_connection(GCMSenderId, ApiKey, GcmEndpoint, GcmEndpointPort) ->
    BasicSession = exmpp_session:start(),
    JID = exmpp_jid:make(GCMSenderId, GcmEndpoint, undefined),
    case gcm_xmpp_authorization:authorize(BasicSession, JID, ApiKey, GcmEndpoint, GcmEndpointPort) of
        {ok, Session} ->
            {ok, Session};
        {error, Reason} ->
            {error, Reason}
    end.

send_gcm_push(GcmMessage, From, State) ->
    case send_push(State#state.session, GcmMessage) of
        {ok, Id} ->
            ets:insert(State#state.wait_deliv_status, #wait_deliv{message_id = Id, from = From}),
            {noreply, State};
        {error, Reason} ->
%%             TODO: change to #gcm_error{}
            {reply, {error, Reason}, State}
    end.

send_push(Session, GcmMessage) when is_pid(Session) andalso is_record(GcmMessage, gcm_message)->
    try
        AssembledJSONPacket = gcm_xmpp_assembler:assemble(GcmMessage),
        XmppPacket =
            #xmlel{name = message,
                children = #xmlel{
                    name = gcm,
                    ns= 'google:mobile:data',
                    children = #xmlcdata{cdata = AssembledJSONPacket}
                }
            },
        XmppMessageId = send_packet(Session, XmppPacket),
        {ok, XmppMessageId}
    catch
        throw:{package_validation, Reason} ->
            {error, {wrong_gcm_package, Reason}};
        throw:{package_assembling, Reason} ->
            {error, {wrong_gcm_package, Reason}};
        throw:ErrorReason ->
            {error, {packet_sending_error, ErrorReason}};
        Level:ErrorReason ->
            lager:error("unknown error: ~p~nStackTrace: ~p~n", [{Level,ErrorReason}, erlang:get_stacktrace()]),
            {error, {unknown_error, {Level, ErrorReason}}}
    end.


send_packet(Session, Packet) when is_tuple(Packet) ->
    exmpp_session:send_packet(Session, Packet).