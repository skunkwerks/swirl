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

-module(ppspp_handshake).
-include("swirl.hrl").

%% api
-export([unpack/1,
         pack/1,
         handle/2]).

-type handshake() :: #{channel => ppspp_channel:channel(),
                       options => ppspp_options:options()}.
-export_type([handshake/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc unpack a handshake message
%% Deconstruct PPSPP UDP datagram into multiple erlang terms, including
%% parsing any additional data within the same segment. Any parsing failure
%% is fatal and will propagate back to the attempted datagram unpacking.
%% @end

-spec unpack(binary()) -> {handshake(), binary()}.

unpack(Message) ->
    {Channel, Maybe_Options} = ppspp_channel:unpack_with_rest(Message),
    {Maybe_Messages, Options} = ppspp_options:unpack(Maybe_Options),
    Handshake = #{channel => Channel, options => Options},
    {Handshake, Maybe_Messages}.

-spec pack(ppspp_message:message()) -> binary().
pack(_Message) -> <<>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc handle a handshake
%%
%% The payload of the HANDSHAKE message is a channel ID (see
%%  Section 3.11) and a sequence of protocol options.  Example options
%%  are the content integrity protection scheme used and an option to
%%  specify the swarm identifier.  The complete set of protocol options
%%  are specified in Section 7.
%%
%% handshake needs the endpoint info, and will set up a new channel if the
%% requested swarm is available, and the options are compatible. The linking
%% of the swarm and external peer's address together should be registered
%% with the swarm handler.
%%
%% @end
-spec handle(handshake(), ppspp_datagram:endpoint()) -> ok.
handle(Handshake, Endpoint) ->
    % Inbound channel is just the first 4 bytes of the datagram
    Inbound_channel = ppspp_datagram:get_peer_channel(Endpoint),
    % Source channel is where the peer would like replies sent to
    Source_channel = ppspp_channel:get_channel(Handshake),
    % These options should match with an available swarm.
    % The channel_worker checks this on startup by looking up the swarm_id.
    Requested_swarm_options = maps:get(options, Handshake),
    {ok, Pid} = case ppspp_channel:is_channel_zero(Inbound_channel) of
                    % If inbound channel is zero then we should start a new channel_worker
                    true ->{ok, _Pid} =
                           channel_worker:start(Endpoint, Requested_swarm_options);
                    % if non-zero there should be an existing channel. If not, it might
                    % prove useful to simply create a new channel anyway, which would
                    % allow peers that dropped off for some reason to restart cleanly.
                    false -> {ok, _Pid} =
                             channel_worker:where_is(Inbound_channel)
                end,
    % send handshake message to channel_worker for reply FIXME
    channel_worker:handle(Pid, {handshake, Source_channel}).
