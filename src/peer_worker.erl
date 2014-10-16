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
-export([start_link/1,
         start_link/2,
         stop/1]).

%% callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% records
-record(state, {
          port :: inet:port_number(),
          socket :: port()
         }).
-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% api

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc start the server

-spec start_link(inet:port_number()) -> {error,_} | {ok,pid()}.
start_link(Port) when is_integer(Port) ->
    start_link(Port, convert:port_to_atom(Port)).

-spec start_link(inet:port_number(),atom()) -> {error,_} | {ok,pid()}.
start_link(Port, Name) when is_atom(Name), is_integer(Port) ->
    gen_server:start_link({local, Name}, ?MODULE, [Port], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Stops the server.

-spec stop(inet:port_number()) -> ok.
stop(Port) ->
    gen_server:call(convert:port_to_atom(Port), stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% callbacks

-spec init(nonempty_string()) -> {ok,[state(), ...]}.
init([Port]) ->
    process_flag(trap_exit, true),
    {ok, Socket} = gen_udp:open(Port,  [binary,
                                        {reuseaddr, true},
                                        {active, true} ]),
    ?INFO("peer: ~p listening on udp:~p~n", [self(), Port]),
    {ok, [#state{port = Port, socket = Socket}] }.

-spec handle_call(_,_,_) ->
    {stop,{error,{unknown_call,_}},_}.
handle_call(Message, _From, State) ->
    ?WARN("peer: unexpected call: ~p~n", [Message]),
    {stop, {error, {unknown_call, Message}}, State}.

-spec handle_cast(_,_) ->
    {stop,normal | {error,{unknown_cast,_}},_}.
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Message, State) ->
    ?WARN("peer: unexpected cast: ~p~n", [Message]),
    {stop, {error, {unknown_cast, Message}}, State}.


-spec handle_info(_,_) -> {noreply,_} | {stop,{error,{unknown_info,_}},_}.
handle_info(Packet={udp, _Socket, _Peer, _Port, _Maybe_Datagram}, State) ->
    spawn(ppspp_datagram, handle_raw, [Packet]),
    {noreply, State};
handle_info(timeout, State) ->
    ?WARN("peer: timeout: ~p~n", State);
handle_info(Message, State) ->
    ?WARN("peer: unexpected info: ~p~n", [Message]),
    {stop, {error, {unknown_info, Message}}, State}.

-spec code_change(_,_,_) -> {ok,_}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec terminate(_,[state(), ...]) -> ok.
terminate(Reason, [#state{socket=Socket, port=Port}]) ->
    gen_udp:close(Socket),
    {memory, Bytes} = erlang:process_info(self(), memory),
    ?INFO("peer: ~p terminating port ~p, using ~p bytes, due to reason: ~p~n",
          [self(), Port, Bytes, Reason]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test

-ifdef(TEST).
-spec start_test() -> term().
start_test() ->
    {ok, _} = ?MODULE:start_link(?SWIRL_PORT).
-endif.
