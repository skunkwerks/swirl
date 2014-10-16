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

-module(ppspp_channel).
-include("swirl.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> term().
-endif.

%% api
-export([unpack_channel/1,
         unpack_with_rest/1,
         pack/1,
         get_channel/1,
         channel_to_string/1,
         handle/1]).

-opaque channel() :: {channel, channel_option()}.
-opaque channel_option() :: 0..16#ffffffff.
-export_type([channel/0, channel_option/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc unpack a channel message
%% <p>  Deconstruct PPSPP UDP datagram into multiple erlang terms, including
%% parsing any additional data within the same segment. Any parsing failure
%% is fatal & will propagate back to the attempted datagram unpacking.
%% </p>
%% @end

-spec unpack_with_rest(binary()) -> {channel(), binary()}.

unpack_with_rest(<<Channel:?PPSPP_CHANNEL_SIZE, Rest/binary>>) ->
    {{channel, Channel}, Rest}.

-spec unpack_channel(binary()) -> channel().
unpack_channel(Binary) ->
    {Channel, _Rest} = unpack_with_rest(Binary),
    Channel.

-spec channel_to_string(channel()) -> string().
channel_to_string(_Channel = {channel, Channel}) ->
    string:to_lower(integer_to_list(Channel, 16)).

-spec get_channel(channel()) -> channel_option().
get_channel({channel, Channel}) when is_integer(Channel) -> Channel.

-spec pack(ppspp_message:message()) -> binary().
pack(_Message) -> <<>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%-spec ... handle takes a tuple of {type, message_body} where body is a
%%    parsed orddict message and returns either
%%    {error, something} or tagged tuple for the unpacked message
%%    {ok, reply} where reply is probably an orddict to be sent to the
%%    alternate peer.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The payload of the channel message is a channel ID (see
%  Section 3.11) and a sequence of protocol options.  Example options
%  are the content integrity protection scheme used and an option to
%  specify the swarm identifier.  The complete set of protocol options
%  are specified in Section 7.
-spec handle(ppspp_message:message()) -> any().
handle({channel, _Body}) ->
    {ok, ppspp_message_handler_not_yet_implemented};

handle(Message) ->
    ?DEBUG("message: handler not yet implemented ~p~n", [Message]),
    {ok, ppspp_message_handler_not_yet_implemented}.

