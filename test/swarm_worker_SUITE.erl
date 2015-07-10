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

-module(swarm_worker_SUITE).
-include("swirl.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([start_swarm/1,
         stop_swarm/1
        ]).

-spec all() -> [atom()].
all() -> [start_swarm,
          stop_swarm
         ].

-spec start_swarm(any()) -> true.
start_swarm(_Config) ->
    swirl:start(),
    Swarm_Options = ppspp_options:use_default_options("c39e"),
    {ok, Worker} = swirl:start_swarm(Swarm_Options),
    timer:sleep(100),
    true = is_process_alive(Worker).

-spec stop_swarm(any()) -> false.
stop_swarm(_Config) ->
    Swarm_Options = ppspp_options:use_default_options("c39e"),
    Swarm_id = ppspp_options:get_swarm_id(Swarm_Options),
    {ok, Worker} = swarm_worker:where_is(Swarm_id),
    swirl:stop_swarm(Swarm_id),
    timer:sleep(100),
    false = is_process_alive(Worker).
