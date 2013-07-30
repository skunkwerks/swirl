% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(ppspp_peer).
-include("swirl.hrl").

-export([start/1, stop/1]).

stop(_Peer) ->
    % TODO assuming _Peer is actually going to be Port instead
    % loop over open channels
    % close them
    % shut down ets tables
    ok.

start(Port) ->
    {ok, Socket} = gen_udp:open(Port, [binary, {active, false} ]),
    io:format("peer: listening on udp ~w~n", [Port]),
    loop(Socket).

loop(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {udp, Socket, Peer, Port, << Maybe_Datagram/binary >> } ->
            handle_packet(udp, Peer, Port, Maybe_Datagram),
        loop(Socket)
    end.

handle_packet(udp, Peer, Port, Maybe_Datagram ) ->
    %% TODO move this to handle_cast/async to
    %% avoid refc binaries and increase throughput
    Endpoint = ?ENDPOINT2STR(Peer, Port),
    io:format("peer: recv udp from ~s~n", [Endpoint]),
    Transport = orddict:from_list([ {peer, Peer},
                                    {port, Port},
                                    {endpoint, Endpoint},
                                    {transport, udp}]),
    {ok, Datagram} = ppspp_datagram:unpack(Transport, Maybe_Datagram),
    %% TODO it might make more sense to return some info and
    %% keep some useful state here
    {ok, _State} = ppspp_datagram:handle(Datagram).
