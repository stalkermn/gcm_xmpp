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
-include_lib("exmpp/include/exmpp_xml.hrl").
%% API
-export([assemble/2]).
-export([disassemble_data/1]).
-export([assemble_xmpp_packet/1]).

-spec assemble(GcmMessage, MessageId :: binary()) -> jsx:json_text() when
    GcmMessage :: #gcm_message{}.
assemble(GcmMessage, MessageId)->
    ok = gcm_xmpp_validator:gcm_message_validate(GcmMessage),
    #gcm_message{
        to = To,
        payload = Payload,
        ttl = TTL,
        delay_while_idle = IdleDelayFlag,
        delivery_receipt_requested = DeliveryRecieptFlag,
        collapse_key = CollapseKey,
        priority = Priority,
        content_available = ContentAvailable,
        dry_run = DryRun
    } = GcmMessage,

    jsx:encode([
        {<<"to">>, To},
        {<<"message_id">>, MessageId} |
        assemble_additional_params([
            assemble_notification_payload(Payload),
            {<<"time_to_live">>, TTL},
            {<<"delay_while_idle">>, IdleDelayFlag},
            {<<"delivery_receipt_requested">>, DeliveryRecieptFlag},
            {<<"collapse_key">>, CollapseKey},
            {<<"priority">>, Priority},
            {<<"content_available">>, ContentAvailable},
            {<<"dry_run">>, DryRun}
        ])
    ]).

assemble_additional_params(List) ->
    lists:filter(
        fun
            ({_, undefined}) -> false;
            ({_, _})  -> true
        end, List).

assemble_notification_payload(#notification{} = Notification) ->
    Fields = record_info(fields, notification),
    NotificationPayloadRaw = lists:zip(Fields, tl(tuple_to_list(Notification))),
    NotificationPayloadJsonTerm =
        lists:filtermap(
            fun
                ({_, undefined}) ->
                    false;
                ({Key, Value}) ->
                    {true, {atom_to_binary(Key, utf8), Value}}

            end, NotificationPayloadRaw),
    {<<"notification">>, NotificationPayloadJsonTerm};

assemble_notification_payload(Data) when is_list(Data) ->
    {<<"data">>, Data};

assemble_notification_payload(undefined) ->
    {<<"">>,undefined}.


-spec disassemble_data(jsx:json_term()) ->
    gcm_reply().
disassemble_data(Data) ->
    JSONData = jsx:decode(exmpp_xml:get_cdata(Data)),
    {_, MessageId} = lists:keyfind(<<"message_id">>, 1, JSONData),
    {_, MessageType} = lists:keyfind(<<"message_type">>, 1, JSONData),
    {MessageId, do_disassemble(MessageId, MessageType, JSONData)}.

do_disassemble(MessageId, <<"ack">>, _Data) ->
    #gcm_ack{ message_id = MessageId };
do_disassemble(MessageId, <<"receipt">>, Data) ->
    ReceiptData = proplists:get_value(<<"data">>, Data),
    #gcm_receipt{
        message_id = MessageId,
        message_status = proplists:get_value(<<"message_status">>, ReceiptData),
        category = proplists:get_value(<<"category">>, ReceiptData)

    };
do_disassemble(MessageId, <<"nack">>, Data) ->
    #gcm_nack{
        message_id = MessageId,
        error = proplists:get_value(<<"error">>, Data),
        error_description = proplists:get_value(<<"error_description">>, Data)
    };
do_disassemble(_, <<"control">>, Data) ->
    #gcm_control{
        control_type = proplists:get_value(<<"control_type">>, Data)
    }.

-spec assemble_xmpp_packet(#gcm_message{})-> xmlel().
assemble_xmpp_packet(GcmMessage) ->
    MessageId = list_to_binary(exmpp_utils:random_id("xmpp-gcm")),
    AssembledJSONPacket = gcm_xmpp_assembler:assemble(GcmMessage, MessageId),
    #xmlel{name = message,
        attrs = [
            #xmlattr{
                name = <<"id">>,
                value = MessageId
            }
        ],
        children = #xmlel{
            name = gcm,
            ns= 'google:mobile:data',
            children = [ #xmlcdata{cdata = AssembledJSONPacket} ]
        }
    }.