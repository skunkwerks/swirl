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

%% @doc Library for PPSPP over UDP, aka PPSPP protocol
%% <p>This module implements a library of functions necessary to
%% handle the wire-protocol of PPSPP over UDP, including
%% functions for encoding and decoding datagrams.</p>
%% @end

-module(ppspp_datagram).
-include("swirl.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> term().
-endif.

%% api
-export([handle/1,
         handle/2,
         handle_datagram/2,
         unpack/3,
         pack/1]).

-opaque endpoint() :: {endpoint, list( endpoint_option())}.
-opaque endpoint_option() :: {ip, inet:ip_address()}
| {socket, inet:socket()}
| {port, inet:port_number()}
| {transport, udp}
| {uri, string()}
| ppspp_channel:channel().
-opaque datagram() :: {datagram, list(endpoint() | ppspp_message:messages())}.
-export_type([endpoint/0,
              endpoint_option/0,
              datagram/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc  helper functions
%% @end
-spec peer_to_string(inet:ip_address(), inet:port_number()) -> string().

peer_to_string(Peer, Port) ->
    lists:flatten([[inet_parse:ntoa(Peer)], $:, integer_to_list(Port)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc translates raw udp packet into a tidy structure for later use
%% @spec
%% @end

-spec build_endpoint(udp, inet:socket(), inet:ip_address(), inet:port_number(),
                     ppspp_channel:channel()) ->  endpoint().

build_endpoint(udp, Socket, IP, Port, Channel) ->
    Channel_Name = ppspp_channel:channel_to_string(Channel),
    Peer_as_String = peer_to_string(IP, Port),
    Endpoint_as_URI = lists:concat([ Peer_as_String, "#", Channel_Name]),
    Endpoint = {endpoint, orddict:from_list([{ip, IP},
                                             Channel,
                                             {port, Port},
                                             {uri, Endpoint_as_URI},
                                             {transport, udp},
                                             {socket, Socket} ])},
    ?DEBUG("dgram: received udp from ~s~n", [Endpoint_as_URI]),
    Endpoint.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc receives datagram from peer_worker, parses & delivers to matching channel
%% @spec handle_datagram() -> ok
%% @end

%% TODO handle/1 is simply a place holder so we can continue on parser while
%% integrating a better peer_worker. This code must be purged!
-spec handle({udp, inet:socket(), inet:ip_address(), inet:port_number(),
              binary()}) -> ok.
handle(Packet) ->
    Root_Hash = "c89800bfc82ed01ed6e3bfd5408c51274491f7d4",
    handle(Packet, ppspp_options:use_default_options(Root_Hash)),
    ok.

-spec handle({udp, inet:socket(), inet:ip_address(), inet:port_number(),
              binary()}, ppspp_options:options()) -> ok.

handle(_Packet = {udp, Socket, Peer_IP_Address, Peer_Port, Maybe_Datagram},
       Swarm_Options) ->
    Channel = ppspp_channel:unpack_channel(Maybe_Datagram),
    Endpoint = build_endpoint(udp, Socket, Peer_IP_Address, Peer_Port, Channel),
    Datagram = unpack(Maybe_Datagram, Endpoint, Swarm_Options),
    ?DEBUG("dgram: got valid datagram ~p~n", [Datagram]),
    % NB usually called from spawned process, so return values are ignored
    handle_datagram(Datagram, Swarm_Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Extract PPSPP channel ID from header of datagram
%% <p>  Each PPSPP datagram contains as the first 4 byes the inbount channel ID.
%% </p>
%% @end

-spec handle_datagram(datagram(), ppspp_options:options()) -> ok.
handle_datagram(_Datagram = {datagram, _Dgram_as_Dict}, _Swarm_Options) ->
    %% handle/1 needs to become handle/2 as the swarm state and inbound channel
    %% are needed to process the messages correctly.
    %% This also needs to be moved into ppspp_message module wrt opaque typing.
    % _Transport = orddict:fetch(transport, Datagram),
    % lists:foreach(
    %   fun(Message) -> ppspp_message:handle(Message) end,
    %   orddict:fetch(messages,Datagram)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc unpack a UDP packet into a PPSPP datagram using erlang term format
%% <p>  Deconstruct PPSPP UDP datagram into multiple erlang terms, including
%% <ul>
%% <li>Endpoint containing opaque information about Transport</li>
%% <li>list of Messages</li>
%% <li>Opaque orddict for Options, within a handshake message</li>
%% <ul>
%% A single datagram MAY contain multiple PPSPP messages; these will be handled
%% recursively as needed.
%% </p>
%% @end

%% packet() = [
%% TODO revisit specs
%% {transport, ppspp_transport()},
%% {messages, ppspp_messages()}
%% ].

-spec unpack(binary(), endpoint(), ppspp_options:options()) -> datagram().
unpack(Raw_Datagram, _Endpoint, Swarm_Options) ->
    {Channel, Maybe_Messages} = ppspp_channel:unpack_with_rest(Raw_Datagram),
    ?DEBUG("dgram: received on channel ~p~n",
           [ppspp_channel:channel_to_string(Channel)]),
    Parsed_Messages = ppspp_message:unpack(Maybe_Messages, Swarm_Options),
    Parsed_Datagram = orddict:from_list([Channel, {messages, Parsed_Messages}]),
    {datagram, Parsed_Datagram}.

-spec pack(datagram()) -> binary().
pack(_Datagram) -> <<>>.
