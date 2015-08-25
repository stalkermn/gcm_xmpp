%%%-------------------------------------------------------------------
%%% @author protoj
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Авг. 2015 10:01
%%%-------------------------------------------------------------------
-module(gcm_xmpp_assembler).
-author("protoj").

-include("gcm_xmpp.hrl").
%% API
-export([assemble/1]).

-spec assemble(GcmMessage) -> jsx:json_text() when
    GcmMessage :: #gcm_message{}.
assemble(GcmMessage)->

    ok = gcm_xmpp_validator:gcm_message_validate(GcmMessage),

    #gcm_message{
        to = To,
        notification = Notification,
        data = Data,
        ttl = TTL,
        delay_while_idle = IdleDelayFlag,
        delivery_receipt_requested = DeliveryRecieptFlag
    } = GcmMessage,

    jsx:encode([
        {<<"to">>, To},
        assemble_notification_payload(Notification, Data) |
        assemble_additional_params([
            {<<"time_to_live">>, TTL},
            {<<"delay_while_idle">>, IdleDelayFlag},
            {<<"delivery_receipt_requested">>, DeliveryRecieptFlag}
        ])
    ]).

assemble_additional_params(List) ->
    lists:filter(
        fun
            ({_, undefined}) -> false;
            ({<<"delay_while_idle">>, Value}) when is_boolean(Value) ->
                true;
            ({<<"delivery_receipt_requested">>, Value}) when is_boolean(Value) ->
                true
        end,
        List).

assemble_notification_payload(#notification{title = Title, body = Text}, undefined) ->
    {<<"notification">>, [
        {<<"title">>, Title},
        {<<"text">>, Text}
    ]};
assemble_notification_payload(undefined, Data) ->
    {<<"data">>, Data};

assemble_notification_payload(_, _) ->
    erlang:throw({package_assembling, <<"wrong 'data' or 'notification'">>}) .


%% assemble(_To, Notification, Data, TTL, IdleDelayFlag, DeliveryRecieptFlag) ->