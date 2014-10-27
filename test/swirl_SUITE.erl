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

-module(swirl_SUITE).
-include("swirl.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([start_peer/1,
         stop_peer/1]).

-spec all() -> [atom()].
all() -> [start_peer,stop_peer].

-spec start_peer(any()) -> true.
start_peer(_Config) ->
    swirl:start(),
    Swarm_Options = ppspp_options:use_default_options(),
    {ok, Worker} = swirl:start_peer(0, Swarm_Options),
    timer:sleep(100),
    true = erlang:is_process_alive(Worker).

-spec stop_peer(any()) -> false.
stop_peer(_Config) ->
    Worker = gproc:lookup_local_name({peer_worker, 0}),
    true = erlang:is_process_alive(Worker),
    swirl:stop_peer(0),
    timer:sleep(100),
    false = erlang:is_process_alive(Worker).
