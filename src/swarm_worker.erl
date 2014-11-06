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

-module(swarm_worker).
-include("swirl.hrl").
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-spec test() -> term().
-endif.

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
%% @doc start the server with a given set of options which includes
%% the root hash.
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
%% @doc Looks up the pid for a given swarm id, either as string or binary.

-spec where_is(string() | ppspp_options:swarm_id()) -> {ok, pid()} | {error,_}.
where_is(Root_Hash) when is_list(Root_Hash) ->
    Swarm_Options = ppspp_options:use_default_options(Root_Hash),
    Swarm_id = ppspp_options:get_swarm_id(Swarm_Options),
    where_is(Swarm_id);
where_is(Swarm_id) ->
    case Pid = gproc:lookup_local_name({?MODULE, Swarm_id}) of
        undefined -> {error, ppspp_swarm_worker_not_found};
        _ -> {ok, Pid}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Looks up the swarm options for a given swarm id, either as string or binary.

-spec get_swarm_options(string() | ppspp_options:swarm_id()) ->
    {ok, ppspp_options:options() } | {error, any()}.
get_swarm_options(Swarm) ->
    case where_is(Swarm) of
        {ok, Pid} -> gen_server:call(Pid, get_swarm_options);
        Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% callbacks

-spec init([ppspp_options:swarm_id() | ppspp_options:options()]) -> {ok,state()}.
init([Swarm_id , Swarm_Options]) ->
    process_flag(trap_exit, true),
    ?INFO("swarm: ~p started with swarm_id:~p~n and options: ~p~n",
          [self(), Swarm_id , Swarm_Options]),
    {ok, #state{swarm_id = Swarm_id , options= Swarm_Options} }.

-spec handle_cast(stop | any(), state()) -> {noreply, state()}.
handle_cast(stop, State) -> {stop, normal, State};
handle_cast(Message, State) ->
    ?WARN("~s: unexpected cast ~p~n  in state ~p~n", [?MODULE, Message, State]),
    {noreply, State}.

-spec handle_call(any(), {pid(), any()}, state()) -> {reply, ok, state()}.
handle_call(Message, From, State) ->
    ?WARN("~s: unexpected call from ~p ~p~n  in state ~p~n",
          [?MODULE, From, Message, State]),
    {reply, ok, State}.

-spec handle_info(_, state()) ->
    {noreply, state()} | {stop,{error,{unknown_info,_}},_}.
handle_info(Message, State) ->
    ?WARN("~s: unexpected info ~p~n  in state ~p~n", [?MODULE, Message, State]),
    {noreply, State}.

-spec code_change(_,state(),_) -> {ok,state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec terminate(_,state()) -> ok.
terminate(Reason, State=#state{swarm_id=Swarm_id}) ->
    gproc:goodbye(),
    {memory, Bytes} = erlang:process_info(self(), memory),
    ?INFO("swarm: ~p terminating swarm ~p, using ~p bytes, due to reason: ~p~n  with state ~p~n",
          [self(), Swarm_id, Bytes, Reason, State]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test

-ifdef(TEST).
-spec start_test() -> term().
start_test() ->
    application:ensure_all_started(swirl),
    Root_Hash = "c39e",
    Swarm_Options = ppspp_options:use_default_options(Root_Hash),
    {ok, Worker} = ?MODULE:start_link(Swarm_Options),
    ?assertEqual(true, erlang:is_process_alive(Worker)).

-spec stop_test() -> term().
stop_test() ->
    Root_Hash = "c39e",
    Swarm_Options = ppspp_options:use_default_options(Root_Hash),
    Swarm_id = ppspp_options:get_swarm_id(Swarm_Options),
    Worker = gproc:lookup_local_name({?MODULE, Swarm_id}),
    ?MODULE:stop(Swarm_id),
    io:format("worker is ~p~n", [Worker]),
    ?assertEqual(false,  erlang:is_process_alive(Worker)).
-endif.
