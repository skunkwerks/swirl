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
%% <p>Wrapper to support running directly from escript.</p>
%% @end

-module(swirl).
-include("swirl.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([main/1,
         help/0,
         quit/0,
         start_peer/0,
         start_peer/1,
         start_peers/2,
         stop_peer/0,
         stop_peer/1,
         stop_peers/2,
         start/0,
         stop/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Start the swirl application in a stand-alone fashion.
%% This should only be used for testing and in the erlang shell.
%% @end
-spec start() -> ok.
start() ->
    {ok, _} = application:ensure_all_started(?MODULE),
    ok.

%% @doc Stop the swirl application and all dependent swarms and peers.
%% Allows swirl to terminate any peer connections if necessary.
%% end
-spec stop() -> ok | {error,_}.
stop() ->
    application:stop(swirl).

%% @doc Stop the swirl application, all dependent swarms and peers, and
%% the entire BEAM virtual machine too.
%% @end
-spec quit() -> no_return().
quit() ->
    _ = stop(),
    init:stop().

%% @doc start a PPSPP listener (peer) on a given port, or the default port.
%% @end
-spec start_peer() -> {ok, pid()} | {error,_}.
start_peer() ->
    start_peer(?SWIRL_PORT).

-spec start_peer(inet:port_number()) -> {ok, pid()} | {error,_}.
start_peer(Port) when is_integer(Port), Port > 0, Port < 65535 ->
    supervisor:start_child(peer_sup, [Port]).

%% @doc start multiple PPSPP listeners (peers) quickly on a given range of
%% ports. Note there is no guarantee of success nor error checking but it
%% looks great for demos.
%% @end
-spec start_peers(inet:port_number(), inet:port_number()) ->
    [ {{ok, pid()}, inet:port_number() | {error,_}}].
start_peers(First, Last) when is_integer(First), is_integer(Last), First < Last  ->
    Ports = lists:seq(First, Last),
    lists:map(fun(Port) ->
                      {start_peer(Port), Port}
              end,
              Ports).

%% @doc stop a PPSPP peer on a given port, or the default port.
%% @end
stop_peer() ->
    stop_peer(?SWIRL_PORT).
-spec stop_peer(inet:port_number()) -> ok | {error, not_found}.
stop_peer(Port) when is_integer(Port), Port > 0, Port < 65535 ->
    Worker_pid = whereis(convert:port_to_atom(Port)),
    supervisor:terminate_child(peer_sup, Worker_pid).


%% @doc stop multiple PPSPP peers on a given range of ports.
%% @end
-spec stop_peers(inet:port_number(), inet:port_number()) ->
    [{ok | {error, _}, inet:port_number()}].
stop_peers(First, Last) when is_integer(First), is_integer(Last), First < Last  ->
    Ports = lists:seq(First, Last),
    lists:map(fun(Port) ->
                      {stop_peer(Port), Port} end,
              Ports).

%% @doc help for console users
%% Provides a summary of available commands options within the erlang console
%% @end
-spec help() -> ok.
help() ->
    io:format("~s: online help~n", [?SWIRL_APP]),
    Help =["use any of these commands, prefixed by `swirl:` to run:",
           "",
           "help().                   these help notes",
           "start().                  starts the swirl application, but no peers or swarms",
           "stop().                   stops the swirl application, active peers and swarms",
           "start_peer().             starts a single peer on the default port",
           "start_peer(Port).         starts a single peer on the given port, e.g. 7777",
           "start_peers(First, Last). starts peers on consecutive ports from First to Last",
           "stop_peer().              stops a single peer on the default port",
           "stop_peer(Port).          stops a single peer on the given port, e.g. 7777",
           "stop_peers(First, Last).  stops peers on consecutive ports from First to Last",
           "quit().                   terminates *immediately* the entire BEAM vm",
           "",
           "use ^c to exit, or type `swirl:quit().`",
           ""],

    lists:foreach(fun(Line) ->
                          io:format("~s~n", [Line])
                  end,
                  Help),
    ok.

%% for escript support
-spec main(any()) -> no_return().
main(_) ->
    help(),
    start(),
    _ = start_peer(),
    timer:sleep(infinity).
