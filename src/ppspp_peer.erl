%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%  http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Live seeder is responsible for serving live stream data.
%% <p>description goes here</p>
%% @end

-module(live_seeder).

-behaviour(gen_server).

%% -include("../include/ppspp.hrl").
-include("../include/ppspp_records.hrl").
-include("../include/swirl.hrl").
%% API
-export([start_link/1,
         start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
%-spec start_link({atom(), hash()})
% TODO for injector the Role is seeder always !
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          [Args], []).

%% TODO implement init for this. 
start_link(Args, Swarm_Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          [Args, Swarm_Options], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([{Type, Role, Swarm_ID, State_Table}]) ->
    {Integrity_Check_Method, Live_Sig_Algo, Live_Discard_Window} =
            if
                Type =:= static ->
                    { ?PPSPP_DEFAULT_INTEGRITY_CHECK_METHOD, none, none};
                Type =:= live orelse Type =:= injector -> 
                    { ?PPSPP_DEFAULT_LIVE_INTEGRITY_CHECK_METHOD,
                      ?PPSPP_DEFAULT_LIVE_SIGNATURE_ALGORITHM,
                      ?PPSPP_DEFAULT_LIVE_DISCARD_WINDOW 
                    } 
            end,
    {ok, #state{
          server_type           = Type,
          role                  = Role,
          peer_table            = State_Table, 
          %% TODO : decide how to name the merkle tree
          mtree                 = Swarm_ID,
          ppspp_swarm_id        = Swarm_ID, 
          ppspp_version         = ?PPSPP_CURRENT_VERSION,
          ppspp_minimum_version = ?PPSPP_CURRENT_VERSION,
          ppspp_chunking_method = ?PPSPP_DEFAULT_CHUNK_ADDRESSING_METHOD, 
          ppspp_integrity_check_method   = Integrity_Check_Method,
          ppspp_merkle_hash_function     = ?PPSPP_DEFAULT_MERKLE_HASH_FUNCTION,
          ppspp_live_signature_algorithm = Live_Sig_Algo,
          ppspp_live_discard_window      = Live_Discard_Window 
         }};

init([{_Type, _Swarm_ID}, _Swarm_Options]) ->
    %% TODO : discuss the data type for Swarm_Options 
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
%% Use call to receive new data becuse so the next data packets are not sent
%% untill the current one processed correctly.
handle_call({newData, _Data}, _From, #state{server_type=live}) ->
    %% TODO : check if the NCHUNKS_PER_SIG number of data packets have arrived
    %% and then create a new subtree and declare the new packets using HAVE
    %% messages in the swarm.
    ok;
handle_call(Message, _From, State) ->
    ?WARN("peer: unexpected call: ~p~n", [Message]),
    {stop, {error, {unknown_call, Message}}, State}.

%%--------------------------------------------------------------------
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    %% TODO get the state of peer from peer_state table and store it in State
    %% if peer lookup fails then add the peer to the table with the new dict as
    %% follows : orddict:from_list([{range, []}])
    %% State#state.peer_state will contain ACKed Chunk_IDs
    _Reply = handle_msg({State#state.server_type, State#state.role}, Msg,
                        State, []), 
    %% spwan(?MODULE, handle_msg, [{Type, Role}, Msg, State, []]),  
    {noreply, State}.

%%
%% Handle messages.
%%
handle_msg(_, [], _State, _Reply) ->
    %% TODO send packed message to the listener
    %% lists:reverse(lists:flatten(Reply)).
    %ppspp_datagram:pack(Reply)
    ok;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% replies with HANDSHAKE and HAVE message.
%% Payload is expected to be an orddict.
handle_msg({Type, Role}, [{handshake, Payload} | Rest], State, Reply) ->
    {ok, Response} = ppspp_message:handle({Type, Role},
                                          {handshake, Payload}, State),
    handle_msg({Type, Role}, Rest, State, [Response | Reply]);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ACK
handle_msg({Type, leecher}, [{ack,_Payload} | Rest], State, Reply) ->
    ?WARN("~p ~p: unexpected ACK message ~n", [Type, leecher]),
    handle_msg({Type, leecher}, Rest, State, Reply);

handle_msg({Type, Role}, [{ack, Payload} | Rest], State, Reply) ->
    ppspp_message:handle({Type, Role}, {ack, Payload}, State),
    %% Update State
    {Peer, _}        = State#state.peer_state,
    {ok, Peer_State} = peer_store:lookup(State#state.peer_table, Peer),
    New_State        = State#state{peer_state = {Peer, Peer_State}},
    handle_msg({Type, Role}, Rest, New_State, Reply);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HAVE
handle_msg({Type, leecher}, [{have, Payload} | Rest], State, Reply) ->
    %% TODO discuss how to prepare REQUEST for DATA from multiple peers. 
    {ok, Response} = ppspp_message:handle({Type, leecher},
                                          {have, Payload}, State),
    handle_msg({Type, leecher}, Rest, State, [Response | Reply]);

handle_msg({Type, Role}, [{have,_Payload} | _Rest], State, _Reply) ->
    ?WARN("~p ~p: unexpected HAVE message ~n", [Type, Role]),
    {noreply, State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTEGRITY 
handle_msg({Type, leecher}, [{integrity, Payload} | Rest], State, Reply) ->
    {ok, _Response} = ppspp_message:handle({Type, leecher},
                                          {integrity, Payload}, State),
    handle_msg({Type, leecher}, Rest, State, Reply);

handle_msg({Type, Role}, [{integrity, _Data} | Rest], State, Reply) ->
    ?WARN("~p ~p: unexpected integrity message ~n", [Type, Role]),
    handle_msg({Type, seeder}, Rest, State, Reply);

%%
%% currently NO implementation
handle_msg({Type, Role}, [{pex_resv4, _Data} | Rest], State, Reply) ->
    ?WARN("live_seeder: unexpected pex_resv4 message ~n", []),
    handle_msg({Type, Role}, Rest, State, Reply);
handle_msg({Type, Role}, [{pex_req, _Data} | Rest], State, Reply) ->
    ?WARN("live_seeder: unexpected pex_req message ~n", []),
    handle_msg({Type, Role}, Rest, State, Reply);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Seeder should not receive signed_integrity message.
handle_msg({Type, Role}, [{signed_integrity, _Data} | Rest], State, Reply) ->
    ?WARN("live_seeder: unexpected signed_integrity message ~n", []),
    handle_msg({Type, Role}, Rest, State, Reply);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% REQUEST
handle_msg({Type, leecher}, [{request,_Payload} | Rest], State, Reply) ->
    ?WARN("leecher: unexpected REQUEST message ~n", []),
    handle_msg({Type, leecher}, Rest, State, Reply);

handle_msg({Type, Role}, [{request, Payload} | Rest], State, Reply) ->
    {ok, Response} = ppspp_message:handle({Type, Role},
                                          {request, Payload}, State) ,
    handle_msg({Type, Role}, Rest, State, [Response | Reply]);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CANCEL
handle_msg({_Type, _Role}, [{cancel, _Data} | _Rest], State, _Reply) ->
    {noreply, State};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Seeder should not receive choke message.
handle_msg({Type, Role}, [{choke, _Data} | Rest], State, Reply) ->
    ?WARN("live_seeder: unexpected choke message ~n", []),
    handle_msg({Type, Role}, Rest, State, Reply);

%% Seeder should not receive unchoke message.
handle_msg({Type, Role}, [{unchoke, _Data} | Rest], State, Reply) ->
    ?WARN("live_seeder: unexpected unchoke message ~n", []),
    handle_msg({Type, Role}, Rest, State, Reply);

%% currently no implementation
handle_msg({Type, Role}, [{pex_resv6, _Data} | Rest], State, Reply) ->
    ?WARN("live_seeder: unexpected pex_resv6 message ~n", []),
    handle_msg({Type, Role}, Rest, State, Reply);
handle_msg({Type, Role}, [{pex_rescert, _Data} | Rest], State, Reply) ->
    ?WARN("live_seeder: unexpected pex_rescert message ~n", []),
    handle_msg({Type, Role}, Rest, State, Reply).

%%--------------------------------------------------------------------
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
