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

-module(swerl_app).
-include("swerl.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0, stop/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(?SWERL_APP, Port) ->
    start_link(?SWERL_APP, Port).

stop(_State) ->
   stop().

%% TODO move to swerl_sup and support multiple ports/peers/files

start() ->
    start(?SWERL_APP, ?SWERL_PORT).

start_link(App, Port) ->
    Instance = instance(App, Port),
    register(Instance, Pid=spawn_link(ppspp_peer, start, [Port])),
    io:format("swerl: PPSPP release ~s~n", [?PPSPP_RELEASE]),
    io:format("swerl: peer on ~p registered as ~p~n", [Port, Instance]),
    Pid.

stop() ->
    Instance = instance(?SWERL_APP, ?SWERL_PORT),
    stop(instance, Instance).

stop(instance, Instance) ->
    exit(whereis(Instance), shutdown).

instance(App, Port) when is_integer(Port), is_atom(App) ->
    Instance = [atom_to_list(App), $_ , integer_to_list(Port)],
    list_to_atom(lists:flatten(Instance)).
