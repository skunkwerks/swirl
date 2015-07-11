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

-module(swirl_sup).
-include("swirl.hrl").

-behaviour(supervisor).

%% api
-export([start_link/0]).

%% callbacks
-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% api

-spec start_link() -> {ok, pid()} | {error, _}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% callbacks

-spec init([]) -> {ok,{{one_for_one, 10,60}, [supervisor:child_spec()] }}.
init([]) ->
    Supervisors = [ {peer_sup, {peer_sup, start_link, []},
                     permanent,
                     infinity,
                     supervisor, [peer_sup]},
                    {swarm_sup, {swarm_sup, start_link, []},
                     permanent,
                     infinity,
                     supervisor, [swarm_sup]},
                    {channel_sup, {channel_sup, start_link, []},
                     permanent,
                     infinity,
                     supervisor, [channel_sup]} ],

    % summon the supervisors
    {ok, {{one_for_one, 10, 60}, Supervisors}}.
