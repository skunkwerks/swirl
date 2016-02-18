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

%% @doc Swirl Tests.
%% @end

-module(ppspp_options_SUITE).
-include("swirl.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([defaults/1,
         getters/1
        ]).

-spec all() -> [atom()].
all() -> [defaults,
          getters
         ].

-spec defaults(any()) -> true.
defaults(_Config) ->
    % minimal options should only include the RFC7574 version
    Map = #{minimum_version => ?PPSPP_LOWEST_VERSION},
    Map = ppspp_options:use_minimum_options(),
    % options must be a simple map using defaults from RFC7574
    Root_Hash ="c39e",
    Swarm_id = convert:hex_string_to_padded_binary(Root_Hash),
    Res = #{
      chunk_addressing_method => ?PPSPP_DEFAULT_CHUNK_METHOD,
      chunk_size => ?PPSPP_DEFAULT_CHUNK_SIZE,
      content_integrity_check_method => ?PPSPP_DEFAULT_INTEGRITY_METHOD,
      merkle_hash_tree_function => ?PPSPP_DEFAULT_MERKLE_HASH_FUN,
      minimum_version => ?PPSPP_LOWEST_VERSION,
      supported_version => ?SWIRL_MAX_PPSPP_VERSION,
      swarm_id => Swarm_id},
    Res = ppspp_options:use_default_options(Swarm_id).

-spec getters(any()) -> true.
%% @doc ensure that accessor functions return the RFC7574 defaults.
%% @end
getters(_Config) ->
    Root_Hash ="c39e",
    Swarm_id = convert:hex_string_to_padded_binary(Root_Hash),
    Options = ppspp_options:use_default_options(Swarm_id),
    %% confirm that we can unwrap options from within a datagram structure
    Datagram = maps:put(foo, bar, #{options => Options}),
    Options = ppspp_options:get_options(Datagram),
    % check returned values against header file defaults
    ?PPSPP_DEFAULT_CHUNK_METHOD = ppspp_options:get_chunk_addressing_method(Options),
    ?PPSPP_DEFAULT_INTEGRITY_METHOD = ppspp_options:get_content_integrity_check_method(Options),
    ?PPSPP_DEFAULT_MERKLE_HASH_FUN = ppspp_options:get_merkle_hash_tree_function(Options),
    % NB as above, when v2 of the PPSPP RFC is released this macro must change
    ?PPSPP_LOWEST_VERSION = ppspp_options:get_minimum_version(Options),
    ?PPSPP_DEFAULT_CHUNK_SIZE = ppspp_options:get_chunk_size(Options),
    Swarm_id = ppspp_options:get_swarm_id(Options),
    ?SWIRL_MAX_PPSPP_VERSION = ppspp_options:get_maximum_supported_version(Options),
    true.
