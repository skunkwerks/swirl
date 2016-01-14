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

%% @doc A managed PPSPP Swarm server.
%%
%% This modules implements a PPSPP swarm server, including registering
%% the unique `swarm_id' and associated PPPSPP options, and provides
%% lookup services to locate the `swarm_worker', and retrieve configuration
%% such as swarm options.
%% @type state() = {ppspp_options:swarm_id(), ppspp_options:options()}.
%% @end

-module(swarm_worker).
-include("swirl.hrl").
-behaviour(gen_server).

%% api
-export([start_link/1,
         where_is/1,
         get_swarm_options/1,
         stop/1]).

%% callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% records and state
-record(state, {
          swarm_id ::  ppspp_options:swarm_id(),
          options :: ppspp_options:options()
         }).
-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% api

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Start the server with a given set of PPSPP options that must include
%% the swarm's root hash.
-spec start_link(ppspp_options:options()) ->
    ignore | {error,_} | {ok,pid()}.
start_link(Swarm_Options)  ->
    Swarm_id  = ppspp_options:get_swarm_id(Swarm_Options),
    Registration = {via, gproc, {n, l, {?MODULE, Swarm_id}}},
    gen_server:start_link(Registration, ?MODULE,
                          [Swarm_id , Swarm_Options], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Stops the server.

-spec stop(ppspp_options:swarm_id()) -> ok | {error, any()}.
stop(Swarm_id) ->
    case where_is(Swarm_id) of
        {error, Reason} -> {error, Reason};
        {ok, Pid} -> gen_server:cast(Pid, stop)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Looks up the pid for a given swarm id.

-spec where_is(ppspp_options:swarm_id()) -> {ok, pid()} | {error,_}.
where_is(Swarm_id) ->
    case gproc:lookup_local_name({?MODULE, Swarm_id}) of
        undefined -> {error, ppspp_swarm_worker_not_found};
        Pid -> {ok, Pid}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Given a swarm id, either as string or binary, returns the associated
%% PPSPP options from the `gproc' registry.

-spec get_swarm_options(ppspp_options:swarm_id()) ->
    {ok, ppspp_options:options() } | {error, any()}.
get_swarm_options(Swarm_id) ->
    case gproc:lookup_local_properties({?MODULE, Swarm_id}) of
        [] -> {error, ppspp_swarm_not_registered};
        [{_Pid, Swarm_Options} | _ ] -> {ok, Swarm_Options }
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% callbacks

%% @doc Starts a `swarm_worker' with given swarm id and PPSPP options.
-spec init([ppspp_options:swarm_id() | ppspp_options:options()]) -> {ok,state()}.
init([Swarm_id , Swarm_Options]) ->
    process_flag(trap_exit, true),
    gproc:add_local_property({?MODULE, Swarm_id}, Swarm_Options),
    ?INFO("swarm: ~p started with swarm_id:~p~n and options: ~p~n",
          [self(), Swarm_id , Swarm_Options]),
    {ok, #state{swarm_id = Swarm_id , options= Swarm_Options} }.

%% @doc Default `gen_server' API.
-spec handle_cast(stop | any(), state()) -> {noreply, state()}.
handle_cast(stop, State) -> {stop, normal, State};
handle_cast(Message, State) ->
    ?WARN("~s: unexpected cast ~p~n  in state ~p~n", [?MODULE, Message, State]),
    {noreply, State}.

%% @doc Default `gen_server' API.
-spec handle_call(any(), {pid(), any()}, state()) -> {reply, ok, state()}.
handle_call(Message, From, State) ->
    ?WARN("~s: unexpected call from ~p ~p~n  in state ~p~n",
          [?MODULE, From, Message, State]),
    {reply, ok, State}.

%% @doc Default `gen_server' API.
-spec handle_info(_, state()) ->
    {noreply, state()} | {stop,{error,{unknown_info,_}},_}.
handle_info(Message, State) ->
    ?WARN("~s: unexpected info ~p~n  in state ~p~n", [?MODULE, Message, State]),
    {noreply, State}.

%% @doc Default `gen_server' API.
-spec code_change(_,state(),_) -> {ok,state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc Closes down the `swarm_worker'. Returns reason and brief statistics.
-spec terminate(_,state()) -> ok.
terminate(Reason, State=#state{swarm_id=Swarm_id}) ->
    gproc:goodbye(),
    {memory, Bytes} = erlang:process_info(self(), memory),
    ?INFO("swarm: ~p terminating swarm ~p, using ~p bytes, due to reason: ~p~n  with state ~p~n",
          [self(), Swarm_id, Bytes, Reason, State]).
