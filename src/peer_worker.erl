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

%% @doc A managed PPSPP UDP-based Peer.
%%
%% The peer_worker provides a wrapper around the standard `gen_udp' server
%% spawning an additional erlang process for every received datagram.
%% At startup, the `peer_worker' registers itself in the `gproc' registry
%% with the assigned UDP port.
%%
%% Note that every port has a 1-to-1 association to a specific swarm,
%% comprising options, IP, and port. The main reason for this is
%% to be able to pass these swarm options through to the packet decoder
%% when checking that the requested hash is available in this swarm or not,
%% without requiring additional lookups.
%% @type state() = {ppspp_options:options(), inet:port_number(), port()}.
%% @end

-module(peer_worker).
-include("swirl.hrl").
-behaviour(gen_server).

%% api
-export([start_link/2,
         where_is/1,
         pid_to_port/1,
         get_url/1,
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
%% @doc Start the peer with given port number and ppspp options which
%% includes the root hash and chunk addressing specification. These are
%% required during subsequent packet parsing, even if the peer itself
%% supports multiple swarms.
-spec start_link(inet:port_number(), ppspp_options:options()) ->
    ignore | {error,_} | {ok,pid()}.
start_link(Port, Swarm_Options) when is_integer(Port) ->
    Registration = {via, gproc, {n, l, {?MODULE, Port}}},
    gen_server:start_link(Registration, ?MODULE,
                          [Port, Swarm_Options], []).

%% @doc Stops the server, given a corresponding UDP port.
%% Returns an error tuple if the port is no longer active.
%% @end
-spec stop(inet:port_number()) -> ok | {error, any()}.
stop(Port) ->
    case where_is(Port) of
        {error, Reason} -> {error, Reason};
        {ok, Pid} -> gen_server:cast(Pid, stop)
    end.

%% @doc Looks up the pid for a given port
%% Returns an error tuple if the port cannot be found in the registry.
%% @end
-spec where_is(inet:port_number()) -> {ok, pid()} | {error,_}.
where_is(Port) when is_integer(Port), Port >= 0, Port =< 65535 ->
    case Pid = gproc:lookup_local_name({?MODULE, Port}) of
        undefined -> {error, ppspp_peer_worker_port_not_found};
        _ -> {ok, Pid}
    end.

%% @doc Looks up the port for a given pid.
%% Returns an error tuple if the port cannot be found in the registry.
%% @end
-spec pid_to_port(pid()) -> {ok, inet:port_number()} | {error,_}.
pid_to_port(Pid) when is_pid(Pid) ->
    % TODO change registration to make this less reliant on gproc internals
    Gproc = proplists:get_value(gproc, gproc:info(Pid)),
    try hd(Gproc) of
        {{n,l,{?MODULE, Port}}, _Options } -> {ok, Port}
    catch
        _ -> {error, ppspp_peer_worker_pid_not_found}
    end.

%% @doc Create a URL for this endpoint that another peer could connect to.
%% As there is no official spec for URLs yet, I made one up. See `url.md'.
%% A simple example is ppspp://example.net:7777/c89800bf
%% @end
-spec get_url(inet:port_number()) -> {ok, string()}.
get_url(Port) ->
    {ok, Address} = get_local_ipv4_address(),
    {ok, lists:flatten(["ppspp://", Address, ":", integer_to_list(Port), $/])}.

%% @doc Retrieve the IPv4 address of this system.
%% This is likely to be of marginal use in practice because computers are
%% tricky and networks are downright devious. STUN or ICE or at least
%% NAT_UPNP may prove more useful
%% @end
-spec get_local_ipv4_address() -> {ok, string()} | {error, any()}.
get_local_ipv4_address() ->
    {ok, Host} = inet:gethostname(),
    {ok, IP} = inet:getaddr(Host, inet),
    case inet:ntoa(IP) of
        Resp = {error, _} -> Resp;
        Resp -> {ok, Resp}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% callbacks

%% @doc Initialises the `peer_worker' and associated UDP port.
%% Registers the worker with provided swarm options and UDP port in `gproc'.
%% @end

-spec init([inet:port_number() | ppspp_options:options()]) -> {ok,[state()]}.
init([Requested_port, Swarm_options]) ->
    process_flag(trap_exit, true),
    {ok, Socket} = gen_udp:open(Requested_port,  [binary,
                                                  {reuseaddr, true},
                                                  {active, true} ]),
    % If zero was passed as a port, a random one is chosen. This also
    % must be registered in gproc, and the original port then unregistered.
    {ok,{_IP,Assigned_port }} = inet:sockname(Socket),
    ok = case Assigned_port =:= Requested_port of
             true -> ok; % nothing to see, move along here
             false -> % register a new port and drop the old one
                 gproc:reg({n,l,{?MODULE, Assigned_port}}, Swarm_options),
                 gproc:unregister_name({n,l,{?MODULE, 0}}),
                 ok
         end,
    ?INFO("peer: ~p listening on udp:~p~n  options: ~p~n",
          [self(), Assigned_port, Swarm_options]),
    {ok, [#state{port = Assigned_port, socket = Socket, options= Swarm_options}] }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% default callbacks

%% @doc Default `gen_server' API.
-spec handle_cast(stop | any(), [state()]) -> {noreply, state()}.
handle_cast(stop, State) -> {stop, normal, State};
handle_cast(Message, State) ->
    ?WARN("~s: unexpected cast ~p~nin state ~p~n", [?MODULE, Message, State]),
    {noreply, State}.

%% @doc Default `gen_server' API.
-spec handle_call(any(), {pid(), any()}, [state()]) -> {reply, ok, state()}.
handle_call(Message, From, State) ->
    ?WARN("~s: unexpected call from ~p ~p~nin state ~p~n",
          [?MODULE, From, Message, State]),
    {reply, ok, State}.

%% @doc Receives UDP datagrams from `gen_udp' server and spawns a subsidiary
%% process to manage it. Returns `noreply' tuple.
-spec handle_info(_,[state()]) -> {noreply,[state()]}
| {stop,{error,{unknown_info,_}},_}.
handle_info(Packet={udp, _Socket, _Peer, _Port, _Maybe_Datagram}, State) ->
    proc_lib:spawn(ppspp_datagram, handle_packet, [Packet]),
    {noreply, State}.

%% @doc Default `gen_server' API.
-spec code_change(_,[state()],_) -> {ok,[state()]}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc Closes down the `peer_worker'. Returns reason and brief statistics.
-spec terminate(_,[state()]) -> ok.
terminate(Reason, [#state{socket=Socket, port=Port}]) ->
    gproc:goodbye(),
    gen_udp:close(Socket),
    {memory, Bytes} = erlang:process_info(self(), memory),
    ?INFO("peer: ~p terminating port ~p, using ~p bytes, due to reason: ~p~n",
          [self(), Port, Bytes, Reason]).
