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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Live seeder is responsible for serving live stream data.
%% <p>description goes here</p>
%% @end

-module(live_seeder).

-behaviour(gen_server).

-include("../include/ppspp.hrl").
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
start_link({Type, Swarm_ID}) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          [{Type, Swarm_ID}], []).

start_link({Type, Swarm_ID}, Swarm_Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          [{Type, Swarm_ID}, Swarm_Options], []).


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
init([{Type, Swarm_ID}]) ->
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
          %% TODO : decide how to name the merkle tree
          mtree                 = Swarm_ID,
          ppspp_swarm_id        = Swarm_ID, 
          ppspp_version         = ?PPSPP_CURRENT_VERSION,
          ppspp_minimum_version = ?PPSPP_CURRENT_VERSION,
          ppspp_chunking_method = ?PPSPP_DEFAULT_CHUNK_ADDRESSING_METHOD, 
          ppspp_integrity_check_method   = Integrity_Check_Method,
          ppspp_merkle_hash_function     = ?PPSPP_DEFAULT_MERKLE_HASH_FUNCTION,
          ppspp_live_signature_algorithm = Live_Sig_Algo,
          ppspp_live_discard_window       = Live_Discard_Window 
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
    _Reply = handle_msg(Msg, State, []), 
    {noreply, State}.


%% TODO : write module that prepres prepares messages, which will be packed
%% using the ppspp_datagram:pack() function and sent to the listener.
%% TASKS:
%% - write the message handler in ppspp_message.erl
%% - write prepre_messge func in the ppspp_message.erl
%%
handle_msg([], _State, _Reply) ->
    %% TODO send packed message to the listener
    %ppspp_datagram:pack(Reply)
    ok;

%% replies with HANDSHAKE and HAVE message.
%% Payload is expected to be an orddict.
handle_msg([{handshake, Payload} | Other_Messages], State, Reply) ->
    {ok, Response} = ppspp_message:handle(State#state.server_type,
                                 {handshake, Payload}),
    handle_msg(Other_Messages, State, lists:concat([Response, Reply]));

handle_msg([{ack, _Data} | _Other_Messages], State, _Reply) ->
    {noreply, State};

%% Seeder only sends the HAVE messages and receive them
handle_msg([{have, _Data} | _Other_Messages], State, _Reply) ->
    ?WARN("live_seeder: unexpected HAVE message ~n", []),
    {noreply, State};

%% Seeder should not receive integrity message, the leecher should.
handle_msg([{integrity, _Data} | Other_Messages], State, Reply) ->
    ?WARN("live_seeder: unexpected integrity message ~n", []),
    handle_msg(Other_Messages, State, Reply);

%% currently no implementation
handle_msg([{pex_resv4, _Data} | Other_Messages], State, Reply) ->
    ?WARN("live_seeder: unexpected pex_resv4 message ~n", []),
    handle_msg(Other_Messages, State, Reply);
handle_msg([{pex_req, _Data} | Other_Messages], State, Reply) ->
    ?WARN("live_seeder: unexpected pex_req message ~n", []),
    handle_msg(Other_Messages, State, Reply);

%% Seeder should not receive signed_integrity message.
handle_msg([{signed_integrity, _Data} | Other_Messages], State, Reply) ->
    ?WARN("live_seeder: unexpected signed_integrity message ~n", []),
    handle_msg(Other_Messages, State, Reply);

handle_msg([{request, _Data} | _Other_Messages], State, _Reply) ->
    {noreply, State};

handle_msg([{cancel, _Data} | _Other_Messages], State, _Reply) ->
    {noreply, State};

%% Seeder should not receive choke message.
handle_msg([{choke, _Data} | Other_Messages], State, Reply) ->
    ?WARN("live_seeder: unexpected choke message ~n", []),
    handle_msg(Other_Messages, State, Reply);

%% Seeder should not receive unchoke message.
handle_msg([{unchoke, _Data} | Other_Messages], State, Reply) ->
    ?WARN("live_seeder: unexpected unchoke message ~n", []),
    handle_msg(Other_Messages, State, Reply);

%% currently no implementation
handle_msg([{pex_resv6, _Data} | Other_Messages], State, Reply) ->
    ?WARN("live_seeder: unexpected pex_resv6 message ~n", []),
    handle_msg(Other_Messages, State, Reply);
handle_msg([{pex_rescert, _Data} | Other_Messages], State, Reply) ->
    ?WARN("live_seeder: unexpected pex_rescert message ~n", []),
    handle_msg(Other_Messages, State, Reply).

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
