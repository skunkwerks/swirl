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

%% TODO (thoughts) -> use ETS table to store the merkle tree and use segment
%% tree structure the process the queries quickly. Ensure that the tree is
%% padded and has 2^n number of leaf nodes.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc This library module implements merkle tree API functions.
%% <p> API for merkle hash tree </p>
%% @end
-module(mtree).

-define(EMPTY_HASH, <<0:160>>).
-define(ALGO, sha).

-export([new/1,
         insert/2,
         build_tree/1,
         prune_bin_range/2,
         root_hash1/1,
         root_hash/1,
         get_peak_hash/1,
         get_hash_by_index/2,
         get_uncle_hashes/2,
         get_munro_hash/2,
         verify/3,
         verify_peak_hash/1,
         verify_munro_hash/3,
         verify_uncle_hash/3,
         dump_tree/1,
         load_tree/1]).

-opaque mtree()     :: {term()}.
-type bin()         :: non_neg_integer().
-type hash()        :: binary().
-type hash_list()   :: list(hash()).

-export_type([mtree/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Initialize a new merkle hash tree.
%% @end
-spec new(mtree()) -> ok.
new(Tree) ->
    mtree_store:init(Tree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Status : Done and checked
%% @doc insert/2 adds {Bin, Hash, Data} into the ETS table
%% <p> Provides interface to insert hashes into the ETS table. </p>
%% @end
-spec insert(mtree(), binary()) -> true.
insert(Tree, Data) ->
    %% get next bin number where the hash is to be inserted and call insert
    Hash = mtree_core:hash(Data),
    Bin  = mtree_core:next_bin(Tree),
    mtree_store:insert(Tree, {Bin, Hash, Data}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Status : Incomplete 
%% TODO decide what this function will do !!
%% @doc remove/3 removes a given hash from tree and re-calculate the root hash
%% @end
-spec prune_bin_range(mtree(), integer()) -> true.
prune_bin_range(_Tree, _Bin) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO maybe use spawn(build_tree/3) to speed up the process.
%% @doc Takes input the start and end Bin of the leaf nodes and
%% constructs a tree using the leaf nodes stored in the ETS table and stores
%% the tree back into the ETS table.
%% @end
-spec build_tree(mtree()) -> Root_Bin :: bin().
build_tree(Tree) ->
    %% pad tree with empty hashes, returns the last Bin that was added to pad
    %% the tree.
    Tree_Size = mtree_core:pad_tree(Tree),
    build_tree(Tree, lists:seq(0, Tree_Size, 2), []).

build_tree(_Tree, [], [Root_Bin]) ->
    Root_Bin;
build_tree(Tree, [], Bin_List) ->
    build_tree(Tree, lists:reverse(Bin_List), []);
build_tree(Tree, [Bin1, Bin2 | Tail], Acc) ->
    {ok, Hash1, _} = mtree_store:lookup(Tree, Bin1),
    {ok, Hash2, _} = mtree_store:lookup(Tree, Bin2),
    Hash           = mtree_core:hash([Hash1, Hash2]),
    Bin            = erlang:round((Bin1+Bin2)/2),
    mtree_store:insert(Tree, {Bin, Hash, empty}),
    build_tree(Tree, Tail, [Bin | Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO discuss this !
%% @doc root_hash/1 get the root hash of the tree. The root of a full binary
%% tree that uses bin number scheme will always be of the form 2^N -1 where N
%% is the number of leaf nodes in the tree. This function assumes that the
%% binary tree is full.
%% @end
%% Core idea : get the highest bin stored in the table. Round it off to the
%% nerarest power of 2 and search for this power/2 -1 in the table (which will
%% be our root hash)
%% NOTE : works even if the tree has only uncle hashes an not all
-spec root_hash(mtree()) -> {bin(), hash()} | {error, term()}.
root_hash(Tree) ->
    Last_Bin = mtree_store:highest_bin(Tree),
    Root_Bin = erlang:round(mtree_core:nearest_power_2(Last_Bin)/2) -1,
    case mtree_store:lookup(Tree, Root_Bin) of
        {ok, Root_Hash, _} ->
            {Root_Bin, Root_Hash};
        {error, not_found} ->
            {error, inconsistant_tree}
    end.

%% ALTERNATE IMPLEMENTATION (REMOVE THIS)
%% TODO Check if this is required
root_hash1(Tree) ->
    case mtree_core:is_complete(Tree) of
        {false, none}    ->
            build_tree(Tree),
            root_hash(Tree);
        {true, Root_Bin} ->
            {ok, Hash, _} = mtree_store:lookup(Tree, Root_Bin),
            {Root_Bin, Hash}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc get_peak_hash/1 returns the peak hashes of the given tree.
%% <p> Provides an interface to get the peak hashes that are use by the
%% receiver for reliable file size detection and download/live streaming
%% unification </p>
%% @end
%% TODO : discuss whether to remove build_tree
-spec get_peak_hash(mtree()) -> hash_list().
get_peak_hash(Tree) ->
    case mtree_core:is_complete(Tree) of
        {false, none}    ->
            build_tree(Tree),
            get_peak_hash(Tree);
        {true, Root_Bin} ->
            Start    = mtree_store:get_first(Tree),
            Bin_List = lists:seq(Start, 2*Root_Bin,2),
            get_peak_hash(Tree, Bin_List, [], [])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO write a calculate peaks function to reduce the no. of lookup
%% operations hash is found
%% Takes a list of leaf nodes and traverses up the tree to get the peak hashes.
%% the moment an empty leaf is encountered we more a level up in the layer.
get_peak_hash(_Tree, [], [], Peaks) ->
    Peaks;
get_peak_hash(Tree, [], Acc, Peaks) ->
    get_peak_hash(Tree, lists:reverse(Acc), [], Peaks);
get_peak_hash(Tree, [Bin1, Bin2 | Tail], Acc, Peaks) ->
    case [mtree_store:lookup(Tree, Bin1),
          mtree_store:lookup(Tree, Bin2)] of
        %% move a layer up if the leaf is empty because we have reached the
        %% padding
        [{ok, ?EMPTY_HASH, _}, _] ->
            get_peak_hash(Tree, lists:reverse(Acc), [], Peaks);
        %% Since the leaf leaf is not empty store it as peak and move level up
        %% because we have reached the padding.
        [_, {ok, ?EMPTY_HASH, _}] ->
            {ok, Hash,_} = mtree_store:lookup(Tree, Bin1),
            get_peak_hash(Tree, lists:reverse(Acc), [], [{Bin1,Hash}|Peaks]);

        _   ->
            Parent_Bin = erlang:round((Bin1+Bin2)/2),
            get_peak_hash(Tree, Tail, [Parent_Bin | Acc], Peaks)
    end;
get_peak_hash(Tree, [Bin], Acc, Peaks) ->
    {ok, Hash,_} = mtree_store:lookup(Tree, Bin),
    get_peak_hash(Tree, [], Acc, [{Bin,Hash}|Peaks]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Status : write test cases to check
%% @doc verify_peak_hash/1 takes a peak hash list and verifies its against the
%% given root hash hence enabling the peer to guess the size of the DATA
%% @end
%% NOTE the Hash_List is being processed from higher to lower bin because the
%% highest bin will be in the lowest layer.
%% TODO decide if Root_Hash = {Root_Bin, Root_Hash}
%% TODO write function to calculate data size from peak hashes.
-spec verify_peak_hash({list({bin(), hash()})}) -> true | false.
verify_peak_hash({Peak_List, Root_Hash}) ->
    %% sort the list acc to the bin numbers and reverse it
    Hash_List = lists:reverse(lists:keysort(1, Peak_List)),
    verify_peak_hash(Hash_List, Root_Hash).

verify_peak_hash([{_Bin, Hash}], Root_Hash) ->
    if
        Hash =:= Root_Hash -> true;
        true               -> false
    end;
verify_peak_hash([{Bin, Hash} | Tail], Root_Hash) ->
    Sibling     = mtree_core:get_sibling(Bin),
    %% find the sibling in the list, if it doesn't exist it means that the
    %% sibling bigger than the Bin and it is the root of an empty subtree.
    Parent_Hash = case lists:keyfind(Sibling, 1, Tail) of
                      false ->
                          Sibling_Hash = get_subtree_hash(Sibling),
                          mtree_core:hash([Hash, Sibling_Hash]);
                      {_, Sibling_Hash} ->
                          mtree_core:hash([Sibling_Hash, Hash])
                  end,
    Parent_Bin  = erlang:round((Bin+Sibling)/2),
    Hash_List   = [{Parent_Bin, Parent_Hash} | Tail],
    verify_peak_hash(Hash_List, Root_Hash).

%% LOCAL internal function.
%% Get the root hash of the empty subtree
%% Note : The bin numbers are actually not needed. So, remove them
get_subtree_hash(Bin) ->
    [Start, End] = mtree_core:bin_to_range(Bin),
    Hash_List = [{X,?EMPTY_HASH} || X <- lists:seq(Start, End, 2)],
    get_subtree_hash(Hash_List, []).

get_subtree_hash([], [{_, Root_Hash}]) ->
    Root_Hash;
get_subtree_hash([], Acc) ->
    get_subtree_hash(Acc, []);
get_subtree_hash([{Bin1, Hash1}, {Bin2,Hash2} | Tail], Acc) ->
    Parent_Bin  = erlang:round((Bin1+Bin2)/2),
    Parent_Hash = mtree_core:hash([Hash1, Hash2]),
    get_subtree_hash(Tail, [{Parent_Bin, Parent_Hash} | Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc get_hash_by_index/2 returns a hash or list of hashes for a given
%% Bin which may represent a single chunk or a range of chunks.
%% @end
-spec get_hash_by_index(mtree(), bin()) -> hash_list().
get_hash_by_index(Tree, Bin) ->
    [Start,End] = mtree_core:bin_to_range(Bin),
    get_hash_by_index(Tree, lists:seq(Start,End,2), []).

get_hash_by_index(_Tree, [], Acc) ->
    {ok, lists:reverse(Acc)};
get_hash_by_index(Tree, [Bin | Tail], Acc) ->
    case mtree_store:lookup(Tree, Bin) of
        {ok, Hash, _} ->
            get_hash_by_index(Tree, Tail, [{Bin, Hash}|Acc]);
        {error, not_found} ->
            {error, hash_range_not_avail}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc get_uncle_hashes/2 returns list of uncle hashes required to verify
%% a given chunk.
%% @end
-spec get_uncle_hashes(mtree(), bin()) -> hash_list().
get_uncle_hashes(Tree, Bin) when Bin rem 2 =:= 0 ->
    get_uncle_hashes(Tree, {Bin, 0}, []);
get_uncle_hashes(_Tree, _Bin) ->
    {error, not_a_leaf}.

get_uncle_hashes(Tree, {Bin, Layer}, Acc) ->
    Sibling = mtree_core:get_sibling(Bin),
    if
        %% if the required sibling is less than bin it means that that no
        %% further uncle hashes are required because further hashes have been
        %% already sent for the verifying the previous bin or chunk
        Sibling < Bin ->
            lists:reverse(Acc);
        Sibling > Bin ->
            %% TODO check if this condition works
            case mtree_store:lookup(Tree, Sibling) of
                %% assuming that the tree is complete, so when no sibling is
                %% found it means that we have reached the ROOT
                {error, not_found} -> lists:reverse(Acc);
                {ok, Hash, _}      ->
                    Root = erlang:round((Sibling+Bin)/2),
                    get_uncle_hashes(Tree, {Root, Layer+1},
                                     [{Sibling, Hash} | Acc])
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO : verify if it matches the specs.
%% @doc Given a Hash_List of the type [{Bin1,Hash1}, {Bin2,Hash2}, ...]
%% verify_uncle_hash/3 for a given chunk ID and its Hash (i.e. Bin Number),
%% inserts the Uncle_Hashes into the tree and returns true if the new Root_Hash
%% of the tree after addition of a new chunk is same as the received Root_Hash.
%% <p> When a peer receives a new chunk it MUST receive Uncle_Hashes required
%% to verify to calculate the Root_Hash and check it against the received
%% Root_Hash </p>
%% 
%% IMPORTANT NOTE : the verify function assumes optimistic approach i.e. if
%% reciever is trying to verify chuck 3 (C3), then the function assumes that it
%% has already verified C0, C1, C2.
%% @end
%%
%% TODO incomplete, needs thorough checking.
%% TODO pending : add uncle hashes to tree.
-spec verify_uncle_hash(mtree(), bin(),
                        {hash_list(), bin(), hash()}) -> true
                                                       | false.
verify_uncle_hash(Tree, {Bin, Hash}, {Hash_List, Root_Bin, Root_Hash})
  when Bin rem 2 =:= 0 ->
    case verify_uncle_hash(Tree, Hash_List, {Root_Bin, Root_Hash},
                           {Bin, Hash}, []) of
        %% Uncle_Hashes also includes all the intermediate parents calculated
        %% using the actual uncle hashes
        {ok, true,_Uncle_Hashes} ->
            ok;
            %{ok, insert_hashes(Uncle_Hashes)};
        {ok, false, _} ->
            {error, hash_mismatch};
        {error, Msg, _} ->
            {error, Msg}
    end;
verify_uncle_hash(_Tree, _Bin, _) ->
    not_a_leaf.

verify_uncle_hash(_Tree, [], {R_Bin, R_Hash}, {Bin, Hash}, Acc)
  when Bin =:= R_Bin ->
    {ok, mtree_core:compare(R_Hash, Hash), Acc};
verify_uncle_hash(Tree, Hash_List, {R_Bin, R_Hash}, {Bin, Hash}, Acc) ->
    case mtree_core:get_sibling(Bin) of
        %% if the sibling is less than bin it means that any further required
        %% hashes will be in Tree itself and not the Hash_List.
        Sibling when Sibling < Bin ->
            {Status, Value} = verify(Tree, {R_Bin, R_Hash}, {Bin, Hash}),
            {Status, Value, Acc};

        Sibling when Sibling > Bin ->
            case get_hash(Hash_List, Sibling) of
                error        ->
                    {error, missing_uncle_hashes_in_list, empty};
                Sibling_Hash ->
                    Parent_Hash   = mtree_core:hash([Hash, Sibling_Hash]),
                    Parent_Bin    = erlang:round((Bin + Sibling)/2),
                    New_Hash_List = lists:keydelete(Sibling,1,Hash_List),
                    New_Acc = [{Sibling, Sibling_Hash},
                               {Parent_Bin, Parent_Hash} | Acc],

                    verify_uncle_hash(Tree, New_Hash_List,{R_Bin, R_Hash},
                                      {Parent_Bin, Parent_Hash}, New_Acc)
            end
    end.

%% get hash from the Hash_List
get_hash(Hash_List, Bin) ->
    case lists:keyfind(Bin, 1, Hash_List) of
        {_, Hash} -> Hash;
        false     -> error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc verify/2 for a given bin bumber and its hash it recalculates the root
%% hash and verifies its against the root hash
%% @end
%% TODO thorough checking.
-spec verify(mtree(), {bin(), hash()}, {bin(), hash()}) -> true | false.
verify(_, {Root_Bin, Root_Hash}, {Bin, Hash}) when Root_Bin =:= Bin ->
    {ok, mtree_core:compare_hash(Root_Hash, Hash)};
verify(Tree, {Root_Bin, Root_Hash}, {Bin, Hash}) ->
    Sibling = mtree_core:get_sibling(Bin),
    case mtree_store:lookup(Tree, Sibling) of
        {ok,Sibling_Hash, _} ->
            Parent_Bin  = erlang:round((Bin + Sibling)/2),
            Parent_Hash = if
                              Bin < Sibling ->
                                  mtree_core:hash([Hash, Sibling_Hash]);
                              Bin > Sibling ->
                                  mtree_core:hash([Sibling_Hash, Hash])
                          end,
            verify(Tree, {Root_Bin, Root_Hash}, {Parent_Bin, Parent_Hash});
        {error, not_found} ->
            {error, missing_hashes_in_tree}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc get_munro_hash/2 return the munro hash for a range of chunk numbers.
%% <p> When new chunks are generated and added to the tree during live feed we
%% get a transient root hash or munro hash of this newly generated subtree. Now
%% when the peer requests a chunk of this new subtree, the munro hash is sent
%% along with the necessary uncle hashes which will be used to calculate the
%% munro hash of the received chunk and compare it to the received munro
%% hash.</p>
%% @end
-spec get_munro_hash(mtree(), [integer()]) -> hash().
get_munro_hash(_Tree, _Bins) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc verify_munro_hash/2 returns true if the calculated Munro_Hash from the
%% Uncle_Hashes is equal to the received Munro_Hash.
%% <p> Uses verify function internally which is provided the new subtree as
%% Tree and Munro_Hash as Root_Hash as its arguments.</p>
%% @end
-spec verify_munro_hash(mtree(), hash(), hash_list()) -> {true, mtree()}
                                                       | {false, mtree()}.
verify_munro_hash(_Tree, _Munro_Hash, _Uncle_Hashes) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc dump_tree/2 write the merkle hash tree into a file with name as
%% File_Name. Returns ok if the operation succeeded.
%% @end
-spec load_tree(string()) -> {ok, atom()} | {error, term()}.
load_tree(File_Name) ->
    mtree_store:file_to_table(File_Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc load_tree/1 loads merkle hash tree from a file with name as File_Name.
%% @end
-spec dump_tree(mtree()) -> ok.
dump_tree(Tree) ->
    mtree_store:table_to_file(Tree).
