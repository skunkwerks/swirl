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
%% functions for encoding and decoding datagrams.
%% @end

-module(ppspp_datagram).
-include("swirl.hrl").

%% api
-export([handle_packet/1,
         handle_datagram/2,
         get_peer_uri/1,
         get_endpoint/1,
         get_messages/1,
         get_peer_channel/1,
         get_swarm_options/1,
         unpack/3,
         pack/1]).

-opaque endpoint() :: {endpoint, list(endpoint_option())}.
-opaque uri() :: {uri, string()}.
-opaque endpoint_option() :: {ip, inet:ip_address()}
| {socket, inet:socket()}
| {port, inet:port_number()}
| {transport, udp}
| uri()
| ppspp_channel:channel().
-opaque datagram() :: {datagram, list(endpoint() | ppspp_message:messages())}.
-export_type([endpoint/0,
              endpoint_option/0,
              uri/0,
              datagram/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc  helper functions
%% @end
-spec peer_to_string(inet:ip_address(), inet:port_number()) -> string().

peer_to_string(Peer, Port) ->
    lists:flatten([[inet_parse:ntoa(Peer)], $:, integer_to_list(Port)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc translates raw udp packet into a tidy structure for later use
%% @end

-spec build_endpoint(udp, inet:socket(), inet:ip_address(), inet:port_number(),
                     ppspp_channel:channel()) ->  endpoint().

build_endpoint(udp, Socket, IP, Port, Channel) ->
    Channel_Name = convert:int_to_hex(ppspp_channel:get_channel_id(Channel)),
    Peer_as_String = peer_to_string(IP, Port),
    Endpoint_as_URI = lists:concat([ Peer_as_String, "#", Channel_Name]),
    {endpoint, orddict:from_list([{ip, IP},
                                  Channel,
                                  {port, Port},
                                  {uri, Endpoint_as_URI},
                                  {transport, udp},
                                  {socket, Socket} ])}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc given an endpoint, returns the associated data e.g. uri or channel
%% @end

-spec get_peer_uri(endpoint()) -> uri().
get_peer_uri(Peer) -> get_peer_info(uri, Peer).

-spec get_peer_channel(endpoint()) -> ppspp_channel:channel().
get_peer_channel(Peer) -> get_peer_info(channel, Peer).

-spec get_peer_info( uri | endpoint | channel, endpoint()) ->
    ppspp_channel:channel() |
    uri().
get_peer_info(Option, {endpoint, Peer_Dict}) ->
    {Option, orddict:fetch(Option, Peer_Dict)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc receives datagram from peer_worker, parses and delivers to matching
%% channel. It's usually called from a spawned process, so return values are
%% eventually ignored.
%% @end

-spec handle_packet({udp, inet:socket(), inet:ip_address(), inet:port_number(),
                     binary()}) -> ok.

handle_packet({udp, Socket, Peer_IP_Address, Peer_Port, Maybe_Datagram}) ->
    %% peek at channel to enable handling channel_zero case
    Channel = ppspp_channel:unpack_channel(Maybe_Datagram),
    Endpoint = build_endpoint(udp, Socket, Peer_IP_Address, Peer_Port, Channel),
    %% channel 0 gets special treatment, all other channels should already
    %% exist and be available for lookup in gproc to find associated swarm
    %% and options. The swarm options are required for the parser.
    {Datagram, Swarm_Options} =
    case ppspp_channel:is_channel_zero(Channel) of
        true -> unpack_on_channel_zero(Channel, Maybe_Datagram, Endpoint);
        false -> unpack_on_existing_channel(Channel, Maybe_Datagram, Endpoint)
    end,
    ?DEBUG("dgram: got valid datagram~n~p~n", [Datagram]),
    handle_datagram(Datagram, Swarm_Options).

%% @doc Unpack a datagram on special channel zero
%% Channel zero is special as typically there is no existing channel
%% to refer to, for looking up the swarm and `channel_worker' that this
%% datagram should be routed to. We use an empty set of options to unpack
%% the binary data into a parsed datagram, and then use the parsed options
%% to identify the appropriate swarm and channel later on.
%% we pass in our minimum options as only a handshake should come
%% in the same datagram, and we have as yet no agreed channel or options
%% for this peer / swarm / channel -- for example, this peer can manage many
%% swarms and we do not yet know to which swarm this packet / datagram belongs.
%% @end
-spec unpack_on_channel_zero(ppspp_channel:channel(), binary(), endpoint()) ->
    {datagram(), ppspp_options:options()}.
unpack_on_channel_zero(_Channel, Maybe_Datagram, Endpoint) ->
    Datagram = unpack(Maybe_Datagram, Endpoint,
                      ppspp_options:use_minimum_options()),
    Peer_Requested_Swarm_Options = get_swarm_options(Datagram),
    {Datagram, Peer_Requested_Swarm_Options}.

-spec unpack_on_existing_channel(ppspp_channel:channel(), binary(), endpoint()) ->
    {datagram(), ppspp_options:options()}.
unpack_on_existing_channel(Channel, Maybe_Datagram, Endpoint) ->
    {ok, Swarm_id} = ppspp_channel:get_swarm_id(Channel),
    {ok, Swarm_Options} = swarm_worker:get_swarm_options(Swarm_id),
    Datagram = unpack(Maybe_Datagram, Endpoint, Swarm_Options),
    {Datagram, Swarm_Options}.

%% @doc Requested Swarm options should match Available swarm options
-spec get_swarm_options(datagram()) -> ppspp_options:options().
get_swarm_options(_Datagram) ->
    %% TODO remove the hard wired hack
    %% look for the handshake in the messages
    %% grab the options from there
    Id="c89800bfc82ed01ed6e3bfd5408c51274491f7d4",
    _Options = ppspp_options:use_default_options(Id).

%% @doc get endpoint from datagram
%% @end
-spec get_endpoint(datagram()) -> endpoint().
get_endpoint({datagram, Datagram}) ->
    {endpoint, orddict:fetch(endpoint, Datagram)}.

%% @doc get messages from datagram
%% major TODO
%% @end
-spec get_messages(datagram()) -> ppspp_message:messages().
get_messages({datagram, Datagram}) -> ppspp_message:get_messages(Datagram).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Handle a fully unpacked datagram.
%% Each PPSPP datagram is complete in itself - relies only on the swarm
%% state for deciding what responses are needed. This handle function should
%% be called from a separate channel_worker process.
%% @end

-spec handle_datagram(datagram(), ppspp_options:options()) -> ok.
handle_datagram(Datagram, Swarm_Options) ->
    Endpoint = get_endpoint(Datagram),
    Messages = get_messages(Datagram),
    ?DEBUG("~p: handle_datagram/2 using ~n~p~n~p~n~p~n", [?MODULE, Messages, Swarm_Options, Endpoint]),
    ppspp_message:handle(Messages, Swarm_Options, Endpoint),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc unpack a UDP packet into a PPSPP datagram using erlang term format
%% A single datagram MAY contain multiple PPSPP messages; these will be
%% handled recursively as needed.
%% @end

-spec unpack(binary(), endpoint(), ppspp_options:options()) -> datagram().
unpack(Raw_Datagram, Endpoint, Swarm_Options) ->
    {Channel, Maybe_Messages} = ppspp_channel:unpack_with_rest(Raw_Datagram),
    ?DEBUG("dgram: received on channel ~p~n",
           [convert:int_to_hex(ppspp_channel:get_channel_id(Channel))]),
    Parsed_Messages = ppspp_message:unpack(Maybe_Messages, Swarm_Options),
    Parsed_Datagram = orddict:from_list([Endpoint, Parsed_Messages]),
    {datagram, Parsed_Datagram}.

-spec pack(datagram()) -> binary().
pack(_Datagram) -> <<>>.
