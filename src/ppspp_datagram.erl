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
-endif.

%% api
-export([handle/2,
         handle/1,
         unpack/3,
         pack/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc receives datagram from peer_worker, parses & delivers to matching channel
%% @spec handle_datagram() -> ok
%% @end
handle({udp, Socket, Peer, Port, Maybe_Datagram}, State) ->
    Pretty_Endpoint = convert:endpoint_to_string(Peer, Port),
    Peer = orddict:from_list([{peer, Peer},
                              {port, Port},
                              {endpoint, Pretty_Endpoint},
                              {transport, udp},
                              {socket, Socket} ]),
    ?DEBUG("dgram: received udp from ~s~n", [Pretty_Endpoint]),
    {ok, Datagram} = unpack(Maybe_Datagram, Peer, State),
    % NB usually called from spawned process, so return values are ignored
    handle(Datagram).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% example parsed datagram, containing a single HANDSHAKE message
% dgram:handle is an orddict containing transport properties, and a list of
% messages, each of which is a tuple {Message_type, Message_body}:
% [{channel,0},
%  {peer,
%    [{endpoint,"127.0.0.1:54181"},
%     {peer,{127,0,0,1}},
%     {port,54181},
%     {transport,udp}]},
%  {messages,
%    [{handshake,
%       [{channel,1107349116},
%        {options,
%          [{ppspp_chunking_method,
%             ppspp_chunking_32bit_chunks},
%           {ppspp_content_integrity_check_method,
%             ppspp_merkle_hash_tree},
%           {ppspp_merkle_hash_function,ppspp_sha1},
%           {ppspp_minimum_version,1},
%           {ppspp_swarm_id,
%             <<102,161,8,98,186,238,189,255,132,98,231,
%             69,162,222,24,207,156,225,26,229>>},
%           {ppspp_version,1}]}]}]}]

handle(Datagram) ->
    _Transport = orddict:fetch(transport, Datagram),
    lists:foreach(
      fun(Message) -> ppspp_message:handle(Message) end,
      orddict:fetch(messages,Datagram)),
    {ok, Datagram}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc unpack a UDP packet into a PPSPP datagram using erlang term format
%% <p>  Deconstruct PPSPP UDP datagram into multiple erlang terms, including
%% <ul>
%% <li>orddict for Transport</li>
%% <li>list of Messages</li>
%% <li>orddict for Options, within a handshake message</li>
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

%%-spec unpack(ppspp_transport(), packet() -> ppspp_datagram()).
unpack(<<Channel:?PPSPP_CHANNEL_SIZE, Maybe_Messages/binary>>, Peer, State ) ->
    Channel_Name = convert:channel_to_string(Channel),
    ?DEBUG("dgram: received on channel ~p~n", [Channel_Name]),
    {ok, Parsed_Messages} = ppspp_message:unpack(Maybe_Messages),
    ?DEBUG("dgram: parsed ok on channel ~p~n", [Channel_Name]),
    Datagram = orddict:store(messages, Parsed_Messages,
                             orddict:store(channel, Channel, Peer)),
    {ok, Datagram}.

pack(_) -> ok.
