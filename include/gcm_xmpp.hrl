%%%-------------------------------------------------------------------
%%% @author valeriy.vasilkov <valeriy.vasilkov@gmail.com>
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Авг. 2015 09:45
%%%-------------------------------------------------------------------
-author("valeriy.vasilkov <valeriy.vasilkov@gmail.com>").




-type registration_id() :: binary().

-record(notification, {
    title :: binary(),
    body :: binary(),
    icon :: binary(),
    sound :: binary(),
    badge :: binary(),
    tag :: binary(),
    color :: binary(),
    click_action :: binary()
}).

-record(gcm_message, {
    to :: registration_id(),
    payload :: #notification{} | jsx:json_term() | undefined,

    ttl :: binary(),
    collapse_key :: binary(),
    priority :: binary(),
    content_available :: boolean(),
    dry_run :: boolean(),
    delay_while_idle :: boolean() | undefined,
    delivery_receipt_requested :: boolean() | undefined
}).

-record(gcm_error, {message_id}).
-record(gcm_nack, {message_id, error, error_description}).
-record(gcm_ack, {message_id}).
-record(gcm_control, {control_type}).