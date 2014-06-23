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
-module(mtree_store).

-type bin()  :: non_neg_integer().
-type hash() :: binary().

-export([init/1,
         insert/2,
         insert_new/2,
         lookup/2,
         get_first/1,
         is_member/2,
         highest_bin/1,
         delete/2,
         table_to_file/1,
         file_to_table/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Initialize a new ETS table. The table is named and public and elements
%% are sorted as ordered set.
%% @end
-spec init(atom()) -> Table :: atom().
init(Table) ->
    ets:new(Table, [ordered_set,
                        %% the first element is considered as key.
                        {keypos, 1},
                        {read_concurrency,true},
                        public,
                        named_table]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Insert the tuple {bin number, hash, chunk} into the table. Note : this
%% will replace any old object with a key same as new one.
%% @end
-spec insert(atom(), {bin(), binary(), binary() | atom()}) -> true.
insert(Table, {Bin, Hash, Data}) ->
    ets:insert(Table, {Bin, Hash, Data}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Insert the tuple {bin number, hash, chunk} if the bin number is not
%% already in the tree.
%% @end
-spec insert_new(atom(), {bin(), hash(), binary()}) -> true | false.
insert_new(Table, {Bin, Hash, Data}) ->
    ets:insert_new(Table, {Bin, Hash, Data}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc searches for a given Bin in the table.
%% @end
-spec lookup(atom(), bin()) -> {ok, hash(), binary()}
                             | {error, _}.
lookup(Table, Bin) ->
    case ets:lookup(Table, Bin) of
        [{Bin, Hash, Data}] -> {ok, Hash, Data};
        []                  -> {error, mtree_store_bin_not_found}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc gets the first bin from the tree
%% @end
-spec get_first(atom()) -> term() | '$end_of_table'.
get_first(Table) ->
    ets:first(Table).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc searches for a given Bin in the table.
%% @end
-spec is_member(atom(), bin()) -> true | false.
is_member(Table, Bin) ->
    ets:member(Table, Bin).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc returns the highest bin number in the ets table, which will always be a
%% leaf node.
%% @end
-spec highest_bin(atom()) -> term() | '$end_of_table'.
highest_bin(Table) ->
    ets:last(Table).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Deletes an object with the given Bin from the table
%% @end
-spec delete(atom(), bin()) -> true.
delete(Table, Bin) ->
    ets:match_delete(Table, {Bin, '_', '_'}).

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
