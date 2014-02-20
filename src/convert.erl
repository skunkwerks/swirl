%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
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

%% @doc conversion library
%% <p>This module implements a set of functions to convert various
%% erlang or binary formats to human-readable text .</p>
%% @end

-module(convert).
-include("swirl.hrl").

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([bin_to_hex/1,
         bin_to_string/1,
         endpoint_to_string/2,
         channel_to_string/1]).

bin_to_hex(Binary) when is_binary(Binary) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || <<N>> <= Binary]).

bin_to_string(Binary) when is_binary(Binary) ->
    lists:flatten(["0x", bin_to_hex(Binary)]).

endpoint_to_string(Peer, Port) ->
    lists:flatten([[inet_parse:ntoa(Peer)], $:, integer_to_list(Port)]).

channel_to_string(Channel) when is_integer(Channel) ->
    bin_to_string(<<Channel:?PPSPP_CHANNEL_SIZE>>).
