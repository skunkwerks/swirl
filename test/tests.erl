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

%% @doc Library for PPSPP over UDP
%% <p>This module implements a library of functions necessary to
%% handle the wire-protocol of PPSPP over UDP, including
%% functions for encoding and decoding.</p>
%% @end

-module(tests).
-include("swirl.hrl").

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-compile([debug_info, export_all]).

-record(state, {port :: non_neg_integer(),
                socket :: port()}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unpack_datagram() ->
    [udp, Peer, Port, Maybe_Datagram] = packet0(raw),
    ok.
    %State = #state{port = Port, socket = Peer} ,
    %peer_worker:handle_datagram(udp, Peer, Port, Maybe_Datagram, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test data from spec and wire via tribler swift client
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

random_channel()    -> crypto:strong_rand_bytes(4). %% random peer channel
initiator_channel() -> << 16#77777777:32/big>>. %% initiator peer channel
responder_channel() -> << 16#88888888:32/big>>. %% responder peer channel

m74_hash() -> convert:hex_string_to_padded_binary("c89800bfc82ed01ed6e3bfd5408c51274491f7d4").

handshake_options(packed) ->
    M74_hash = m74_hash(),
    <<  ?PPSPP_VERSION,                     1, %% ppspp version
        ?PPSPP_MINIMUM_VERSION,             1, %% ppspp max version
        ?PPSPP_SWARM_ID_LENGTH,      20:?WORD, %% swarm id length (160 bits)
        M74_hash/binary                      , %% the merkle hash requested
        ?PPSPP_INTEGRITY_CHECK_METHOD,      1, %% optional integrity check method
        ?PPSPP_MERKLE_HASH_FUNCTION,        0, %% merkle please
        ?PPSPP_CHUNK_ADDRESSING_METHOD,     2, %% chunk addressing method 2
        ?PPSPP_END_OPTION >>; %% end options
handshake_options(unpacked) ->
    [{ppspp_chunking_method,ppspp_32bit_chunks},
     {ppspp_content_integrity_check_method,ppspp_merkle_hash_tree},
     {ppspp_merkle_hash_function,ppspp_sha1},
     {ppspp_minimum_version,1},
     {ppspp_swarm_id, m74_hash()},
     {ppspp_version,1}].

%% UDP wire format as passed to handle_packet_sync from gen_udp socket
packet0(raw) -> [udp, {127,0,0,1} , 52021, dgram0()];
%% and as sent to ppspp_datagram::unpack()
packet0(parsed) -> [transport(), dgram0()].

%% transport info is an orddict of properties relating to the endpoint
%% that the PPSPP transport layer uses. It may or may not be UDP; check
%% the transport property for more info. There should also be a plaintext
%% field for pretty-printing the transport peer address out in future
%% when the standard is specified. e.g.;
%%  endpoint today contains "127.0.0.1:52021"
%% but in future could equally be;
%%  udp://ppspp.peer.org:7777?channel=12345,name=m74.jpg
%%  udp://ppspp.peer.org:7777?channel=12345,hash=c89800bfc82ed01ed6e3bfd5408c51274491f7d4
%% beam://swirl_7777?hash=c89800bfc82ed01ed6e3bfd5408c51274491f7d4
%% The beam transport uses the Erlang VM's clustering services, and
%% assumes the existence of a registered service `swirl_7777` somewhere.
transport() -> orddict:from_list([{peer,{127,0,0,1}},
                                  {port,52021},
                                  {transport,udp}]).

%% requester is even #, responder is odd #
dgram0() ->
    Msg0 = msg0(),
    << ?PPSPP_UNKNOWN_CHANNEL, Msg0/binary >>.

msg0() -> Type = ?HANDSHAKE,
          Channel = random_channel(),
          Options = handshake_options(packed),
          << Type/binary, %% handshake
             Channel/binary,    %% initiator peer channel
             Options/binary >>.

msg1() -> Type = ?HANDSHAKE,
          Channel = random_channel(),
          Options = handshake_options(packed),
          << Type/binary, %% handshake
             Channel/binary,    %% initiator peer channel
             Options/binary >>.

start_farm(Workers) when is_integer(Workers), Workers > 0 ->
    [spawn(peer_worker,start, [Worker + 4000]) || Worker <- lists:seq(0,Workers)].
