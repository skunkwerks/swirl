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

-module(ppspp_channel).
-include("swirl.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% api
-export([unpack_channel/1,
         unpack_with_rest/1,
         pack/1,
         is_channel_zero/1,
         where_is/1,
         acquire/1,
         release/1,
         get_channel_id/1,
         get_channel/1,
         get_swarm_id/1]).

-opaque channel() :: {channel, channel_option()}.
-opaque channel_option() :: 0..16#ffffffff.
-export_type([channel/0, channel_option/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% api
%% @doc unpack a channel message
%% Deconstruct PPSPP UDP datagram into multiple erlang terms, including
%% parsing any additional data within the same segment. Any parsing failure
%% is fatal and will propagate back to the attempted datagram unpacking.
%% @end

-spec unpack_with_rest(binary()) -> {channel(), binary()}.
unpack_with_rest(<<Channel:?PPSPP_CHANNEL_SIZE, Rest/binary>>) ->
    {{channel, Channel}, Rest}.

-spec unpack_channel(binary()) -> channel().
unpack_channel(Binary) ->
    {Channel, _Rest} = unpack_with_rest(Binary),
    Channel.

-spec get_channel_id(channel()) -> non_neg_integer().
get_channel_id(_Channel = {channel, Channel}) -> Channel.

-spec pack(ppspp_channel:channel()) -> binary().
pack(_Message) -> <<>>.

%% @doc helper unwrapper to pull out components from a datagram orddict
%% @end
-spec get_channel(orddict:orddict()) -> channel().
get_channel(Dict) -> {channel, orddict:fetch(channel, Dict)}.

%% @doc allow requesting channel_worker to register an unused channel
%% Ensure that the channel can  be searched for using the swarm id.
%% @end
-spec acquire(ppspp_options:swarm_id()) -> channel().
acquire(Swarm_id) ->
    {channel, _Channel} = find_free_channel(Swarm_id, 0).

-spec find_free_channel(ppspp_options:swarm_id(), non_neg_integer()) ->
    channel() | {error, any()}.
find_free_channel(_, 30) -> {error, ppspp_channel_no_channels_free};
find_free_channel(Swarm_id, Failed_Tries) when Failed_Tries < 30 ->
    <<Maybe_Free_Channel:?DWORD>> = crypto:strong_rand_bytes(4),
    Channel = {channel, Maybe_Free_Channel},
    Key = {n, l, Channel},
    Self = self(),
    %% channel is unique only when returned pid matches self, otherwise
    %% just try again for a new random channel and increased counter
    case gproc:reg_or_locate(Key, Swarm_id) of
        {Self, Swarm_id} -> Channel;
        {_, _ } -> find_free_channel(Swarm_id, Failed_Tries + 1)
    end.

%% @doc allow requesting process to release an assigned channel.
%% This function will crash if the channel was not registered to this
%% process, as gproc returns badarg in this case.
%% @end
-spec release(channel()) -> ok.
release(Channel) ->
    case gproc:unreg({n,l, Channel}) of
        true -> ok;
        _ -> {error, ppspp_channel_free_unassigned_channel}
    end.

%% @doc compare given channel for the handshake channel.
%% All other channels must be assigned to a specific swarm or the datagram
%% unpacker will reject them. Channel zero is the channel used during initial
%% handshaking to negotiate and agree a dedicated channel.
%% @end
-spec is_channel_zero(channel()) -> true | false.
is_channel_zero({channel, 0}) -> true;
is_channel_zero({channel, _}) -> false.


%% @doc looks up pid of the owning swarm for a given channel.
%% Channels are only registered to swarm_workers; and peer_worker however
%% may receive inbound packets for a particular swarm. It is used to locate
%% the owning swarm in a channel when unpacking messages.
%% @end

-spec where_is(channel()) -> {ok, pid()} | {error, any()}.
where_is(Channel = {channel, _}) ->
    case gproc:lookup_local_name(Channel) of
        undefined -> {error, ppspp_channel_not_found};
        Pid -> {ok, Pid}
    end.

%% @doc Looks up the swarm options for a given channel in the registry.
%% @end
-spec get_swarm_id(channel()) ->
    {ok, ppspp_options:swarm_id() } | {error, any()}.
get_swarm_id(Channel = {channel, _}) ->
    try gproc:lookup_value({n,l,Channel}) of
        Swarm_id -> {ok, Swarm_id}
    catch
        _ ->
            {error, ppspp_channel_not_registered}
    end.
