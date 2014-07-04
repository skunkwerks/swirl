%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy
%% of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations
%% under the License.

%% TODO insert metadata of the table in the init funciton

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc This module implements storage API for merkle trees.
%% <p> API for merkle hash tree </p>
%% @end
-module(peer_state).

-type peer()  :: string().
-type state() :: list().

-export([init/1,
         insert/2,
         lookup/2,
         is_member/2,
         delete/2,
         table_to_file/1,
         file_to_table/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Initialize a new ETS table. The table is named and public and elements
%% are sorted as ordered set.
%% @end
-spec init(atom()) -> Table :: atom().
init(Table) ->
    ets:new(Table, [set,
                    %% the first element is considered as key.
                    {keypos, 1},
                    {read_concurrency,true},
                    %% TODO decide the access type for the table
                    public,
                    named_table]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Inserts the peer and its state into the table. 
%% @end
-spec insert(atom(), {peer(), state()}) -> true.
insert(Table, {Peer, State}) ->
    ets:insert(Table, {Peer, State}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Insert the tuple {bin number, hash, chunk} if the bin number is not
%% already in the tree.
%% @end
%-spec insert_new(atom(), {bin(), hash(), binary()}) -> true | false.
%insert_new(Table, {Bin, Hash, Data}) ->
    %ets:insert_new(Table, {Bin, Hash, Data}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc searches for a given peer in the table and returns its state.
%% @end
-spec lookup(atom(), peer()) -> {ok, state()} |
                                {error, _}.
lookup(Table, Peer) ->
    case ets:lookup(Table, Peer) of
        [{_, State}] -> {ok, State};
        []           -> {error, peer_state_not_found}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc searches for a given peer in the table.
%% @end
-spec is_member(atom(), peer()) -> true | false.
is_member(Table, Peer) ->
    ets:member(Table, Peer).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Deletes peer from table 
%% @end
-spec delete(atom(), peer()) -> true.
delete(Table, Peer) ->
    ets:match_delete(Table, {Peer, '_'}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Write the ets table to the disk and deletes the table from the current
%% running erlang shell.
%% @end
-spec table_to_file(atom()) -> true | {error , _}.
table_to_file(Table) ->
    ets:tab2file(Table, lists:concat([Table, ".ETS"])),
    ets:delete(Table).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Loads the ets table from the given file.
%% @end
-spec file_to_table(string()) -> {ok, atom()} | {error, _}.
file_to_table(File_Name) ->
    ets:file2tab(File_Name).
