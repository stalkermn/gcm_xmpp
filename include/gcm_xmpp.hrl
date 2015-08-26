%%%-------------------------------------------------------------------
%%% @author valeriy.vasilkov <valeriy.vasilkov@gmail.com>
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Авг. 2015 09:45
%%%-------------------------------------------------------------------
-author("valeriy.vasilkov <valeriy.vasilkov@gmail.com>").


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

    delay_while_idle :: boolean() | undefined,
    delivery_receipt_requested :: boolean() | undefined,
    ttl :: binary(),
    collapse_key :: binary(),
    priority :: binary(),
    content_available :: boolean(),
    dry_run :: boolean()

}).

-record(gcm_error, {
    message_id :: message_id()
}).
-record(gcm_nack, {
    message_id :: message_id(),
    error :: binary(),
    error_description :: binary()
}).
-record(gcm_ack, {
    message_id :: message_id()
}).
-record(gcm_control, {
    control_type :: binary()
}).
-record(gcm_receipt, {
    message_id :: message_id(),
    message_status :: binary(),
    category :: binary()
}).

-type registration_id() :: binary().
-type message_id() :: binary().
-type gcm_reply() ::  #gcm_nack{} | #gcm_ack{} | #gcm_receipt{} | {error, term()}.
-type gcm_payload() :: #notification{} | jsx:json_term().
