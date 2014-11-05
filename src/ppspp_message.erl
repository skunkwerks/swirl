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
%% <p>This module implements a library of functions necessary to
%% handle the wire-protocol of PPSPP over UDP, including
%% functions for encoding and decoding messages.</p>
%% @end

-module(ppspp_message).
-include("swirl.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> term().
-endif.

%% api
-export([unpack/2,
         pack/1,
         get_message_type/1,
         handle/1]).

-opaque messages() :: list(message()).
-opaque message() :: {message_type(), any()}.
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc unpack a datagram segment into a PPSPP message using erlang term format
%% <p>  Deconstruct PPSPP UDP datagram into multiple erlang terms, including
%% parsing any additional data within the same segment. Any parsing failure
%% is fatal & will propagate back to the attempted datagram unpacking.
%% <ul>
%% <li>Message type</li>
%% <li>orddict for Options</li>
%% <ul>
%% </p>
%% @end

-spec unpack(binary(), ppspp_options:options()) -> message() | messages().

unpack(Maybe_Messages, Swarm_Options) when is_binary(Maybe_Messages) ->
    unpack3(Maybe_Messages, [], Swarm_Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec pack(messages()) -> {ok, iolist()}.
pack(Messages) -> {ok, pack(Messages, [])}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% private

-spec pack(messages(), iolist()) -> iolist().
%% Try to pack another valid message, peeling off and parsing
%% recursively the remainder, accumulating valid (packed) messages.
%% A failure anywhere in a message ultimately causes the entire iolist
%% to be rejected.
pack([Message, Rest], Messages_as_iolist) ->
    pack(Rest, [pack_message(Message) | Messages_as_iolist]);
%% if head binary is empty, all messages were packed successfully
pack([], Messages_as_iolist) -> lists:reverse(Messages_as_iolist).

-spec pack_message(message()) -> binary().
pack_message(_Message) -> <<>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %% if the binary is empty, all messages were parsed successfully
-spec unpack3(binary(), message() | messages(), ppspp_options:options()) ->
    messages() | { message(), binary()}.
unpack3( <<>>, Parsed_Messages, _) ->
    lists:reverse(Parsed_Messages);
%% otherwise try to unpack another valid message, peeling off and parsing
%% recursively the remainder, accumulating valid (parsed) messages.
%% A failure anywhere in a message ultimately causes the entire datagram
%% to be rejected.
unpack3(<<Maybe_Message_Type:?PPSPP_MESSAGE_SIZE, Rest/binary>>,
        Parsed_Messages, Swarm_Options) ->
    Type = get_message_type(Maybe_Message_Type),
    {Parsed_Message, Maybe_More_Messages} = route_to(Type, Rest, Swarm_Options),
    unpack3(Maybe_More_Messages,
            [Parsed_Message | Parsed_Messages],
            Swarm_Options).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% route to specific parser per message type, with swarm options if required
-spec route_to(message_type(), binary(), ppspp_options:options()) -> 
    {message(), binary()} | {error, atom()}.
route_to(handshake, Binary, _) ->
    {Handshake, Maybe_Messages} =  ppspp_handshake:unpack(Binary),
    {Handshake, Maybe_Messages};
route_to(have, Binary, Swarm_Options) ->
    Chunk_Method = ppspp_options:get_chunk_addressing_method(Swarm_Options),
    {Have, Maybe_Messages} =  ppspp_have:unpack(Chunk_Method, Binary),
    {Have, Maybe_Messages};
route_to(_, _, _) ->
    {error, unsupported_ppspp_message_type}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%-spec ... handle takes a tuple of {type, message_body} where body is a
%%    parsed orddict message and returns either
%%    {error, something} or tagged tuple for the unpacked message
%%    {ok, reply} where reply is probably an orddict to be sent to the
%%    alternate peer.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The payload of the HANDSHAKE message is a channel ID (see
%  Section 3.11) and a sequence of protocol options.  Example options
%  are the content integrity protection scheme used and an option to
%  specify the swarm identifier.  The complete set of protocol options
%  are specified in Section 7.
-spec handle(message()) -> {ok, any()}.

handle({handshake, Body}) -> ppspp_handshake:handle(Body);

handle(Message) ->
    ?DEBUG("message: handler not yet implemented ~p~n", [Message]),
    {ok, ppspp_message_handler_not_yet_implemented}.
