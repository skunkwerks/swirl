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

-module(swirl_app).
-include("swirl.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1,
         launch/2]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% used by application:start/1
start(_Type, _Start_Args) ->
    start_link().

start_link() ->
    launch(?SWIRL_APP, ?SWIRL_PORT).

%% TODO move to swirl_sup and support multiple ports/peers/files

launch(App, Port) ->
    Instance = instance(App, Port),
    register(Instance, Pid=spawn_link(ppspp_peer, start, [Port])),
    {ok, Version} = application:get_key(swirl, vsn),
    io:format("swirl: version ~s running PPSPP release ~s~n", [Version, ?PPSPP_RELEASE]),
    io:format("swirl: peer ~p on ~p registered as ~p~n", [Pid, Port, Instance]),
    {ok, Pid}.

stop(_State) ->
    Instance = instance(?SWIRL_APP, ?SWIRL_PORT),
    terminate(instance, Instance).

terminate(instance, Instance) ->
    exit(whereis(Instance), shutdown).

instance(App, Port) when is_integer(Port), is_atom(App) ->
    Instance = [atom_to_list(App), $_ , integer_to_list(Port)],
    list_to_atom(lists:flatten(Instance)).
