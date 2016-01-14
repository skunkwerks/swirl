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

%% @doc A managed PPSPP Channel that represents a remote PPSPP Peer.
%%
%% The state for a specific channel includes the associated remote peer,
%% the swarm, and the channel itself. Within swirl, these three are bound
%% together as a unique value, and the channel ID as the key. The
%% `channel_worker' provides registration and lookups to allow a given
%% `peer_worker' to route messages for a specific remote peer to it, given
%% only the correct `channel_id'. This functionality is used extensively
%% by other modules to decode, process, encode, and send PPSPP datagrams
%% over the wire.
%% @end

-module(channel_worker).
-include("swirl.hrl").
-behaviour(gen_server).

%% api
-export([start_link/2,
         where_is/1,
         handle/2,
         start/2,
         stop/1]).

%% gen_server
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% records and state

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% api

%% @doc Post message into the mailbox.
%% All PPSPP messages arrive through here, and then are re-dispatched back
%% into their respective `ppspp_<message_type>.erl' modules to be processed.
%% @end
-spec handle(pid(), any()) -> ok.
handle(Pid, Message) ->
    gen_server:cast(Pid, Message).

%% @doc start a PPSPP channel, using the supplied peer info.
-spec start(ppspp_datagram:endpoint(),
            ppspp_options:options()) -> {ok, pid()} | {error,_}.
start(Peer_endpoint, Swarm_options) ->
    supervisor:start_child(channel_sup, [Peer_endpoint, Swarm_options]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Stops the server, given a channel.

-spec stop(ppspp_channel:channel()) -> ok | {error, any()}.
stop(Channel) ->
    case where_is(Channel) of
        {error, Reason} -> {error, Reason};
        {ok, Pid} -> gen_server:cast(Pid, stop)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Looks up the pid for a given channel.

-spec where_is(ppspp_channel:channel()) -> {ok, pid()} | {error,_}.
where_is(Channel)  ->
    case Pid = gproc:lookup_local_name({?MODULE, Channel}) of
        undefined -> {error, ppspp_channel_worker_not_found};
        _ -> {ok, Pid}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% callbacks

%% @doc Start the channel using provided peer info, typically a udp endpoint.
%% For channel 0, i.e. the initial handshake contact from a remote peer. We
%% effectively block any re-registration by registering this name, until the
%% first received registration completes successfully.
%% @end
-spec start_link(ppspp_datagram:endpoint(), ppspp_options:swarm_id()) ->
    ignore | {error,_} | {ok,pid()}.
start_link(Peer, Swarm_id)  ->
    Uri = ppspp_datagram:get_peer_uri(Peer),
    Registration = {via, gproc, {n, l, {?MODULE, Uri}}},
    gen_server:start_link(Registration, ?MODULE, [Peer, Swarm_id], []).

%% @doc As the channel id is not known at time of process spawning, it is
%% done during init phase, using gproc. The following values are registered:
%% <pre lang="erlang">
%% {{peer, Peer URI}, Peer}
%% {{channel, Channel id}, Swarm ID}
%% </pre>
%% Peer is the opaque data type used in the datagram module that uniquely
%% identifies a remote peer.
%% TODO change registration of peers to allow multiplexed peers per remote
%% address, and therefore multiple swarms on the same IP.
%% TODO enable updating the registration of the URI to match the acquired
%% channel that will accommodate all future datagrams from this peer.
-spec init(ppspp_options:swarm_id()) -> {ok, ppspp_channel:channel()}.
init(Swarm_id) ->
    {ok, ppspp_channel:acquire(Swarm_id)}.

%% @doc Default `gen_server' API.
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% @doc Default `gen_server' API.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Default `gen_server' API.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Default `gen_server' API.
terminate(_Reason, _State) ->
    ok.

%% @doc Default `gen_server' API.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
