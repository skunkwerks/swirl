%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.

%% @doc Library for PPSPP over UDP, aka Swift protocol
%%
%% This module implements a library of functions necessary to
%% handle the wire-protocol of PPSPP over UDP, including
%% functions for encoding and decoding messages.
%% @end

-module(ppspp_message).
-include("swirl.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% api
-export([unpack/2,
         pack/1,
         get_message_type/1,
         get_messages/1,
         handle/3]).

-opaque messages() :: {messages, list(message())}.
-opaque message() :: {message_type(), any()} |
ppspp_handshake:handshake().
-opaque message_type() :: handshake
| data
| ack
| have
| integrity
| pex_resv4
| pex_req
| signed_integrity
| request
| cancel
| choke
| unchoke
| pex_resv6
| pex_rescert.

-export_type([messages/0,
              message/0,
              message_type/0]).

%% @doc unpack binary into a PPSPP message using erlang term format.
%% Deconstruct PPSPP UDP datagram into multiple erlang terms, including
%% parsing any additional data within the same segment. Any parsing failure
%% is fatal and will propagate back to the attempted datagram unpacking.
%% @end
-spec unpack(binary(), ppspp_options:options()) -> message() | messages().
unpack(Maybe_Messages, Swarm_Options) when is_binary(Maybe_Messages) ->
    {messages, unpack3(Maybe_Messages, [], Swarm_Options)}.

%% @doc convert a list of messages into binary iolist for transmission.
%% @end
-spec pack(messages()) -> iolist().
pack({messages, Messages}) -> pack(Messages, []).

%% private
-spec pack(list(message()), iolist()) -> iolist().
%% Try to pack another valid message, peeling off and parsing
%% recursively the remainder, accumulating valid (packed) messages.
%% A failure anywhere in a message ultimately causes the entire iolist
%% to be rejected.
pack([Message, Rest], Messages) ->
    pack(Rest, [pack_message(Message) | Messages]);
%% if head binary is empty, all messages were packed successfully
pack([], Messages) -> lists:reverse(Messages).


%% @doc dispatches messages to module that knows how to pack that format
%% TODO
%% @end
-spec pack_message(message()) -> binary().
pack_message(_Message) -> <<>>.

%% if the binary is empty, all messages were parsed successfully
-spec unpack3(binary(), message() | list(message()), ppspp_options:options()) ->
    list(message()) | { message(), binary()}.
unpack3( <<>>, Parsed_Messages, _) ->
    lists:reverse(Parsed_Messages);
%% otherwise try to unpack another valid message, peeling off and parsing
%% recursively the remainder, accumulating valid (parsed) messages.
%% A failure anywhere in a message ultimately causes the entire datagram
%% to be rejected.
unpack3(<<Maybe_Message_Type:?PPSPP_MESSAGE_SIZE, Rest/binary>>,
        Parsed_Messages, Swarm_Options) ->
    Type = get_message_type(Maybe_Message_Type),
    {Parsed_Message, Maybe_More_Messages} = parse_message(Type, Rest, Swarm_Options),
    unpack3(Maybe_More_Messages,
            [Parsed_Message | Parsed_Messages],
            Swarm_Options).

%% @doc dispatch to correct message parser along with swarm options
%% @end
-spec parse_message(message_type(), binary(), ppspp_options:options()) ->
    {message(), binary()} | {error, atom()}.
parse_message(handshake, Binary, _) ->
    ppspp_handshake:unpack(Binary);
parse_message(have, Binary, Swarm_Options) ->
    Chunk_Method = ppspp_options:get_chunk_addressing_method(Swarm_Options),
    ppspp_have:unpack(Chunk_Method, Binary);
parse_message(Message_Type, Binary, Options) ->
    {error, {ppspp_unsupported_message_type,
             [Message_Type, Binary, Options]}}.

%% @doc retrieve message type based on IETF spec
%% @end
-spec get_message_type(non_neg_integer()) -> message_type().
get_message_type(Maybe_Message_Type)
  when is_integer(Maybe_Message_Type),
       Maybe_Message_Type < ?PPSPP_MAXIMUM_MESSAGE_TYPE ->
    %% message types in the current spec version
    Message_Type = case <<Maybe_Message_Type:?PPSPP_MESSAGE_SIZE>> of
                       ?HANDSHAKE -> handshake;
                       ?DATA -> data;
                       ?ACK -> ack;
                       ?HAVE -> have;
                       ?INTEGRITY -> integrity;
                       ?PEX_RESv4 -> pex_resv4;
                       ?PEX_REQ -> pex_req;
                       ?SIGNED_INTEGRITY -> signed_integrity;
                       ?REQUEST -> request;
                       ?CANCEL -> cancel;
                       ?CHOKE -> choke;
                       ?UNCHOKE -> unchoke;
                       ?PEX_RESv6 -> pex_resv6;
                       ?PEX_REScert -> pex_rescert
                   end,
    ?DEBUG("message: parser got valid message type ~p~n", [Message_Type]),
    Message_Type.

%% @doc helper unwrapper to pull out messages from a datagram orddict
%% @end
-spec get_messages([any()]) -> any().
get_messages(Messages) -> orddict:fetch(messages, Messages).

%% @doc dispatch messages to correct handler in received order
%% @end
-spec handle(messages() | list(message()),
             ppspp_options:options(),
             ppspp_datagram:endpoint()) -> ok.
handle({messages, Messages}, Swarm_Options, Endpoint) ->
    handle(Messages, Swarm_Options, Endpoint);
handle([], _, _ ) -> ok;
handle([Message | Messages], Swarm_Options, Endpoint) ->
    ok  = handle_message(Message, Swarm_Options, Endpoint),
    handle(Messages, Swarm_Options, Endpoint).

%% @doc does the dirty work of routing each message.
%% Options and Endpoint are required state for handling many message types.
%% @end
-spec handle_message(ppspp_handshake:handshake(),
                     ppspp_options:options(),
                     ppspp_datagram:endpoint()) -> ok | {error, any()}.
handle_message(Message = {handshake, _Body}, _Swarm_Options, Endpoint) ->
    ppspp_handshake:handle(Message, Endpoint);

handle_message(Message, Swarm_Options, Endpoint) ->
    ?DEBUG("~p: handler not yet implemented ~n~p~n~p~n~p~n",
           [?MODULE, Message, Swarm_Options, Endpoint]),
    {error, ppspp_message_handler_not_yet_implemented}.
