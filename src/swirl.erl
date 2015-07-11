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
%% Wrapper to support running directly from escript.
%% @end

-module(swirl).
-include("swirl.hrl").

-export([main/1,
         help/0,
         quit/0,
         start_peer/0,
         start_peer/1,
         start_peer/2,
         start_pool/2,
         stop_peer/0,
         stop_peer/1,
         stop_pool/2,
         start_swarm/1,
         stop_swarm/1,
         start_channel/2,
         stop_channel/1,
         start/0,
         start/1,
         stop/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Start the swirl application in a stand-alone fashion.
%% This should only be used for testing and in the erlang shell.
%% @end
-spec start() -> ok.
start() ->
    {ok, _} = application:ensure_all_started(?MODULE),
    ok.

%% @doc Start the swirl application and everything ready to peer.
%% Takes either single `swarm_id' as string or binary and assumes PPSPP
%% defaults, or a full set of PPSPP options including the `swarm_id'.
%% It starts a swarm and peer, and returns the URI for connecting to.
%% @end
-spec start(string()
            | ppspp_options:swarm_id()
            | ppspp_options:options()) ->
    {ok,
     pid(),
     pid(),
     inet:port_number(),
     string()}.
start(Swarm_id) when is_binary(Swarm_id); is_list(Swarm_id) ->
    start(ppspp_options:use_default_options(Swarm_id));
start(Swarm_options) ->
    {ok, Swarm_pid} = start_swarm(Swarm_options),
    % start a peer on a random port
    {ok, Peer_pid}  = start_peer(0, Swarm_options),
    %% retrieve the new port assigned to this peer
    {ok, Peer_port} = peer_worker:pid_to_port(Peer_pid),
    %% make a pretty URL for other systems
    {ok, Peer_url} = peer_worker:get_url(Peer_port),
    ?INFO("swirl: started swarm ~p and peer ~p at ~s~n",
          [Swarm_pid, Peer_pid, Peer_url]),
    {ok,Swarm_pid, Peer_pid, Peer_port, Peer_url}.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% peer API

%% @doc start a PPSPP listener (peer) on a given port, or the default port,
%% using the supplied hash and default PPSPP swarm options, or no hash at all
%% for an inert peer that can subsequently be controlled by multiple swarms.
%% @end

%% swarm_options is quite happy with an empty binary for a root hash,
%% however it will not be useful until a usable root hash is added by a swarm
%% manager.
-spec start_peer() -> {ok, pid()} | {error,_}.
start_peer() ->
    start_peer(?SWIRL_PORT, ppspp_options:use_default_options()).

%% start_peer can be handed a root hash and assumes default options.
-spec start_peer(string() | ppspp_options:swarm_id()) ->
    {ok, pid()} | {error,_}.
start_peer(Swarm_id) ->
    Swarm_Options = ppspp_options:use_default_options(Swarm_id),
    start_peer(?SWIRL_PORT, Swarm_Options).

-spec start_peer(inet:port_number(), ppspp_options:options()) ->
    {ok, pid()} | {error,_}.
start_peer(Port, Swarm_Options) when is_integer(Port), Port >= 0, Port =< 65535 ->
    supervisor:start_child(peer_sup, [Port, Swarm_Options]).

%% @doc start multiple PPSPP listeners (peers) quickly on a given range of
%% ports. Note there is no guarantee of success nor error checking but it
%% looks great for demos.
%% @end
-spec start_pool(inet:port_number(), inet:port_number()) ->
    [ {{ok, pid()}, inet:port_number() | {error,_}}].
start_pool(First, Last) when is_integer(First), is_integer(Last), First < Last  ->
    Ports = lists:seq(First, Last),
    Swarm_Options = ppspp_options:use_default_options(),
    lists:map(fun(Port) ->
                      {start_peer(Port, Swarm_Options), Port}
              end,
              Ports).

%% @doc stop a PPSPP peer on a given port, or the default port.
%% @end
-spec stop_peer() -> ok | {error, ppspp_peer_worker_not_found}.
stop_peer() ->
    stop_peer(?SWIRL_PORT).

-spec stop_peer(inet:port_number()) -> ok | {error, ppspp_peer_worker_not_found}.
stop_peer(Port) when is_integer(Port), Port >= 0, Port =< 65535 ->
    peer_worker:stop(Port).

%% @doc stop multiple PPSPP peers on a given range of ports.
%% @end
-spec stop_pool(inet:port_number(), inet:port_number()) ->
    [{ok | {error, _}, inet:port_number()}].
stop_pool(First, Last) when is_integer(First), is_integer(Last), First < Last  ->
    Ports = lists:seq(First, Last),
    lists:map(fun(Port) ->
                      {stop_peer(Port), Port} end,
              Ports).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% channel API

%% @doc start a PPSPP channel, using the supplied peer info and swarm.
-spec start_channel(ppspp_datagram:endpoint(), ppspp_options:options()) ->
    {ok, pid()} | {error,_}.
start_channel(Peer_endpoint, Swarm_options) ->
    channel_worker:start(Peer_endpoint, Swarm_options).

%% @doc stop a PPSPP channel
%% @end
-spec stop_channel(ppspp_channel:channel()) ->
    ok | {error, any()}.
stop_channel(Channel) ->
    channel_worker:stop(Channel).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% swarm API

%% @doc start a PPSPP swarm, using the supplied hash and PPSPP swarm options.
%% If either a string (root hash), or a binary (swarm id) are supplied, then
%% start_swarm will do the right thing and assume default PPSPP options. Note
%% that the hashing algorithm and chunk size must match for this to work.
%% -- caveat coder
-spec start_swarm(string()
                  | ppspp_options:swarm_id()
                  | ppspp_options:options()) ->
    {ok, pid()} | {error,_}.
start_swarm(Swarm_id) when is_binary(Swarm_id); is_list(Swarm_id) ->
    Swarm_Options = ppspp_options:use_default_options(Swarm_id),
    start_swarm(Swarm_Options);
start_swarm(Swarm_Options) ->
    supervisor:start_child(swarm_sup, [Swarm_Options]).

%% @doc stop a PPSPP swarm for a given root_hash or swarm_id.
%% @end
-spec stop_swarm(string() | ppspp_options:swarm_id()) ->
    ok | {error, ppspp_swarm_worker_not_found}.
stop_swarm(Swarm) when is_binary(Swarm); is_list(Swarm) ->
    Swarm_Options = ppspp_options:use_default_options(Swarm),
    swarm_worker:stop(ppspp_options:get_swarm_id(Swarm_Options)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc help for console users
%% Provides a summary of available commands options within the erlang console
%% @end
-spec help() -> ok.
help() ->
    io:format("~s: online help ~p~n", [?SWIRL_APP, swirl_app:version()]),
    Help =["use any of these commands, prefixed by `swirl:` to run:",
           "",
           "help().                    these help notes",
           "start().                   starts the swirl application, but no peers or swarms",
           "stop().                    stops the swirl application and all peers and swarms",
           "start(Options | Id).       starts a new swarm and peer on OS selected random port",
           "start_swarm(Opts|Id).      starts a swarm using given options or swarm id",
           "stop_swarm(Opts|Id).       stops a swarm using given options or swarm id",
           "start_peer(Opts|Id).       starts a peer with default port, supplied options or id",
           "start_peer(Port, Opts|Id). starts a peer with given port",
           "stop_peer().               stops a single peer on the default port",
           "stop_peer(Port).           stops a single peer on the given port",
           "start_pool(First, Last).   starts peers on consecutive ports from First to Last",
           "stop_pool(First, Last).    stops peers on consecutive ports from First to Last",
           "quit().                    immediately terminates the entire BEAM vm",
           "",
           "e.g. swirl:start(\"c89800bfc82ed01ed6e3bfd5408c51274491f7d4\").",
           "use ^c twice to exit, or type `swirl:quit().` for a graceful shutdown.",
           "", ""],

    lists:foreach(fun(Line) -> io:format("~s~n", [Line]) end, Help),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% for escript support
-spec main(any()) -> no_return().
main(_) ->
    help(),
    start(),
    _ = start_peer(),
    timer:sleep(infinity).
