gcm_xmpp
=======
This software provides an Erlang client for [`Google Cloud Messaging`](http://developer.android.com/google/gcm/index.html "Google Cloud Messaging") using XMPP connection.
### How it works:
> gcm_xmpp use  CCS (Cloud Connection Server). It's an XMPP endpoint that provides a persistent, asynchronous, bidirectional connection to Google servers.

Some of the benefits of CCS:
 - The asynchronous nature of XMPP allows you to send more messages with fewer resources.
 - Communication is bidirectional—not only can your server send messages to the device, but the device can send messages back to your server.
 - The device can send messages back using the same connection used for receiving, thereby improving battery life.

More information about CSS you can find there:  [Cloud Connection Server](https://developers.google.com/cloud-messaging/ccs)

Getting Started
=======
For starting to use gcm_xmpp
### How to compile
```sh
git clone https://github.com/stalkermn/gcm_xmpp.git 
cd gcm_xmpp
make
```
gcm_xmpp use exmpp by process-one. exmpp works as xmpp client to `CCS`.
### Running application
gcm_xmpp is not standalone application. It's only OTP library with couple of fucntions for use. 
Working with gcm_xmpp requeire to run some depends applications

```erlang    
lager:start(),
exmpp:start(),
ssl:start(),
application:start(gcm_xmpp)
``` 
### OR
```erlang    
gcm_xmpp:start().
``` 
### Starting new worker
CCS has few endpoints with different ports. Thats why we can manipulate, while starting a new worker.
```erlang    
gcm_xmpp:start_link(GcmJid, APIKey, GcmEndpoint, GcmEndpointPort).
``` 
    

| Attribute | Type | Description | Example | Default
| :----- | :--- | :------------- | :---------- |:---------- |
| `GcmJid` | `string` | [JID](http://xmpp.org/extensions/xep-0029.html). | "<Project Number>@gcm.googleapis.com" |
| `APIKey`    | `string` | Credentials to access your enabled APIs. For [more...](https://developers.google.com/console/help/new/?hl=en_US#credentials-access-security-and-identity) |
| `GcmEndpoint`    | `string` | The CCS XMPP endpoint. | "gcm-xmpp.googleapis.com" | "gcm-xmpp.googleapis.com" |
| `GcmEndpointPort`    | `integer` | CSS XMPP endpoint port. | 5235 | 5235 |

### Alternative worker starting
```erlang    
gcm_xmpp:start_link(GcmJid, APIKey, GcmEndpoint).
``` 
    
#### OR
```erlang    
gcm_xmpp:start_link(GcmJid, APIKey).
``` 
    
    
[GcmEndpointPort] AND [GcmEndpoint]  can be taken from enviroment configuration or will be taken by default:
"gcm.googleapis.com" AND 5235 respectively
#### OR
```erlang
gcm_xmpp:start_link().
``` 
All configuration params which are don't present visibly in start_link call will be taken from Enviroment. For example, your enviroment must be configured such as:
```erlang    
{gcm_xmpp, [
    {gcm_jid, "<Project Number>@gcm.googleapis.com"},
    {gcm_api_key, "<You APIKey>"},
    {gcm_xmpp_endpoint, "gcm.googleapis.com"},
    {gcm_xmpp_port, 5235}
]}
```    
### How to send notification
##### Send-to-Sync notification
```erlang
{ok, Pid} = gcm_xmpp:start_link(),
Token = <Subscription Token>,
gcm_xmpp:send_to_sync(Pid, Token).
```
##### Notifications with payload
Basic Notification with payload is
```erlang
-include_lib("gcm_xmpp/include/gcm_xmpp.hrl).
....
gcm_xmpp:send_push(Session, #gcm_message{}).
```
###### Alternatives for sending Payload is
```erlang    
gcm_xmpp:send_push(Session, To, Title, Text, Icon).
```
###### OR
```erlang    
-include_lib("gcm_xmpp/include/gcm_xmpp.hrl).
....
gcm_xmpp:send_push(Session, To, #notification{}).
```
###### OR
```erlang
gcm_xmpp:send_push(Session, To, Data)
```

### Send Notification Response
```erlang
-type gcm_reply() :: #gcm_nack{} | #gcm_ack{} | #gcm_receipt{} | {error, term()}.
```
For more information about CCS response information, [Google Cloud Messaging CSS: Response Format](https://developers.google.com/cloud-messaging/ccs)

License
----
MIT
