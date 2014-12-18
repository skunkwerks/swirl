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

-module(peer_worker).
-include("swirl.hrl").
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> term().
-endif.

%% api
-export([start_link/2,
         stop/1]).

%% callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% records and state
-record(state, {
          options :: ppspp_options:options(),
          port    :: inet:port_number(),
          socket  :: port()
         }).
-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% api

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc start the server with a port number, and ppspp options which includes
%% the root hash and chunk addressing specification. These are required during
%% subsequent packet parsing, even if the peer supports multiple swarms.
%% If start_link/2 is called with a root hash, the default ppspp options are
%% assumed.
-spec start_link(inet:port_number(), ppspp_options:options()) ->
    ignore | {error,_} | {ok,pid()}.
start_link(Port, Swarm_Options) when is_integer(Port) ->
    Registration = {via, gproc, {n, l, {?MODULE, Port}}},
    gen_server:start_link(Registration, ?MODULE,
                          [Port, Swarm_Options], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Stops the server.

-spec stop(inet:port_number()) -> ok | {error, any()}.
stop(Port) ->
    case where_is(Port) of
        {error, Reason} -> {error, Reason};
        {ok, Pid} -> gen_server:cast(Pid, stop)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Looks up the pid for a given port.

-spec where_is(inet:port_number()) -> {ok, pid()} | {error,_}.
where_is(Port) when is_integer(Port), Port >= 0, Port =< 65535 ->
    case Pid = gproc:lookup_local_name({?MODULE, Port}) of
        undefined -> {error, ppspp_peer_worker_not_found};
        _ -> {ok, Pid}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% callbacks

-spec init([inet:port_number() | ppspp_options:options()]) -> {ok,[state()]}.
init([Port, Swarm_Options]) ->
    process_flag(trap_exit, true),
    {ok, Socket} = gen_udp:open(Port,  [binary,
                                        {reuseaddr, true},
                                        {active, true} ]),
    ?INFO("peer: ~p listening on udp:~p~n  options: ~p~n",
          [self(), Port, Swarm_Options]),
    {ok, [#state{port = Port, socket = Socket, options= Swarm_Options}] }.

-spec handle_cast(stop | any(), [state()]) -> {noreply, state()}.
handle_cast(stop, State) -> {stop, normal, State};
handle_cast(Message, State) ->
    ?WARN("~s: unexpected cast ~p~nin state ~p~n", [?MODULE, Message, State]),
    {noreply, State}.

-spec handle_call(any(), {pid(), any()}, [state()]) -> {reply, ok, state()}.
handle_call(Message, From, State) ->
    ?WARN("~s: unexpected call from ~p ~p~nin state ~p~n",
          [?MODULE, From, Message, State]),
    {reply, ok, State}.

-spec handle_info(_,[state()]) -> {noreply,[state()]} | {stop,{error,{unknown_info,_}},_}.
handle_info(Packet={udp, _Socket, _Peer, _Port, _Maybe_Datagram}, State) ->
    proc_lib:spawn(ppspp_datagram, handle, [Packet]),
    {noreply, State}.

-spec code_change(_,[state()],_) -> {ok,[state()]}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec terminate(_,[state()]) -> ok.
terminate(Reason, [#state{socket=Socket, port=Port}]) ->
    gproc:goodbye(),
    gen_udp:close(Socket),
    {memory, Bytes} = erlang:process_info(self(), memory),
    ?INFO("peer: ~p terminating port ~p, using ~p bytes, due to reason: ~p~n",
          [self(), Port, Bytes, Reason]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test

-ifdef(TEST).
-spec start_test() -> term().
start_test() ->
    {ok, _} = application:ensure_all_started(swirl),
    Swarm_Options = ppspp_options:use_default_options(),
    {ok, Worker} = ?MODULE:start_link(0, Swarm_Options),
    ?assertEqual(true, erlang:is_process_alive(Worker)).

-spec stop_test() -> term().
stop_test() ->
    Worker = gproc:lookup_local_name({?MODULE, 0}),
    ?MODULE:stop(0),
    io:format("worker is ~p~n", [Worker]),
    ?assertEqual(false, erlang:is_process_alive(Worker)).
-endif.
