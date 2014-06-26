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

-include("ppspp.hrl"). 
%% NCHUNKS_PER_SIG has to be a fixed power of 2

-export([new/1,
         insert/2,
         build_tree/1,
         prune_bin_range/2,
         root_hash1/1,
         root_hash/1,
         get_data_range/1,
         get_peak_hash/1,
         get_hash_by_index/2,
         get_uncle_hashes/2,
         get_all_uncle_hashes/2,
         get_latest_munro/1,
         get_munro_uncles/2,
         get_all_munro_uncles/2,
         get_subtree_hash/1,
         verify/3,
         verify_peak_hash/2,
         verify_munro_hash/3,
         verify_uncle_hash/3,
         peaks_to_size/1,
         dump_tree/1,
         load_tree/1]).

-opaque mtree()     :: atom().
-type bin()         :: non_neg_integer().
-type hash()        :: binary().
-type bin_hash()    :: {bin(), hash()}.
-type hash_list()   :: list({bin(), hash()}).

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
-spec prune_bin_range(mtree(), bin()) -> true.
prune_bin_range(_Tree, _Bin) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO maybe use spawn(build_tree/3) to speed up the process.
%% @doc Takes input the start and end Bin of the leaf nodes and
%% constructs a tree using the leaf nodes stored in the ETS table and stores
%% the tree back into the ETS table.
%% @end
%% IMPORTANT NOTE : This function will fail incase we prune the tree i.e. if
%% the required leaf nodes are not in the tree, also the Start must be leaf
%% node number 0
-spec build_tree(mtree()) -> Root_Bin :: bin().
build_tree(Tree) ->
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
    %% insert_new is used becoz there may be uncle hashes already in the tree.
    mtree_store:insert_new(Tree, {Bin, Hash, empty}),
    build_tree(Tree, Tail, [Bin | Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc root_hash/1 get the root hash of the tree. The root of a full binary
%% tree that uses bin number scheme will always be of the form 2^N -1 where N
%% is the number of leaf nodes in the tree. This function assumes that the
%% binary tree is full.
%% @end
%% Core idea : get the highest bin stored in the table. Round it off to the
%% nerarest power of 2 and search for this power/2 -1 in the table (which will
%% be our root hash). This implementation is bound work even if the tree is not
%% complete
%% NOTE : works even if the tree has only uncle hashes and not all leaf nodes,
%% such as during the initial creation of a tree from the first received PPSPP
%% datagrams, or during live streaming.
-spec root_hash(mtree()) -> bin_hash() | {error, _}.
root_hash(Tree) ->
    Root_Bin = mtree_core:root_bin(Tree),
    case mtree_store:lookup(Tree, Root_Bin) of
        {ok, Root_Hash, _} ->
            {Root_Bin, Root_Hash};
        {error, _} ->
            erlang:error({error, mtree_inconsistant})
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
%% @doc root_hash/1 get the root hash of the tree. The root of a full binary
%% tree that uses bin number scheme will always be of the form 2^N -1 where N
%% is the number of leaf nodes in the tree. This function assumes that the
%% binary tree is full.
%% @end
-spec get_data_range(mtree()) -> {ok, bin(), bin()}.
get_data_range(Tree) ->
    End = case mtree_core:next_bin(Tree) of
              Bin when Bin > 0 -> Bin-2
          end,
    case mtree_store:get_first(Tree) of
        First when First rem 2 =:= 0 ->
            {ok, get_first_leaf(Tree, First, End), End};
        First                        ->
            {ok, get_first_leaf(Tree, First+1, End), End} 
    end.

get_first_leaf(_Tree, End, End) ->
    End;
get_first_leaf(Tree, Bin, End) ->
    case mtree_store:lookup(Tree, Bin) of
        {ok, ?SHA1_EMPTY_HASH, _} -> get_first_leaf(Tree, Bin+2, End);
        {error, _}                -> get_first_leaf(Tree, Bin+2, End);
        _                         -> Bin
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc get_peak_hash/1 returns the peak hashes of the given tree.
%% <p> Provides an interface to get the peak hashes that are use by the
%% receiver for reliable file size detection and download/live streaming
%% unification </p>
%% @end
%% Will fail if root hash doesnt exist, the leaf nodes do not exist or if the
%% peaks dont exist
-spec get_peak_hash(mtree()) -> hash_list().
get_peak_hash(Tree) ->
    Root_Bin = mtree_core:root_bin(Tree),
    Bin_List = filter_bins(Tree, lists:seq(0,2*Root_Bin,2), []),
    get_peak_hash(Tree, Bin_List, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Takes a list of leaf nodes and traverses up the tree to get the peak hashes.
get_peak_hash(_Tree, [], [], Peaks) ->
    Peaks;
get_peak_hash(Tree, [], Acc, Peaks) ->
    get_peak_hash(Tree, lists:reverse(Acc), [], Peaks);
get_peak_hash(Tree, [Bin1,Bin2 | Tail], Acc, Peaks) ->
    Parent_Bin = erlang:round((Bin1 + Bin2)/2),
    get_peak_hash(Tree, Tail, [Parent_Bin | Acc], Peaks);
get_peak_hash(Tree, [Bin], Acc, Peaks) ->
    {ok, Hash,_} = mtree_store:lookup(Tree, Bin),
    get_peak_hash(Tree, [], Acc, [{Bin,Hash} | Peaks]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% filter leaf nodes to get only those which are not empty in order to get the
%% peak hashes
%% NOTE : in future we'd want to support multiple hash types and therefore
%% empty hash will change.
filter_bins(_Tree, [], Acc) ->
    lists:reverse(Acc);
filter_bins(Tree, [Bin1, Bin2 | Tail], Acc) ->
    case [mtree_store:lookup(Tree, Bin1), mtree_store:lookup(Tree, Bin2)] of
        [{ok, ?SHA1_EMPTY_HASH, _}, _] ->
            lists:reverse(Acc);
        [_, {ok, ?SHA1_EMPTY_HASH, _}] ->
            lists:reverse([Bin1 | Acc]);
        [{ok,_,_}, {ok,_,_}] ->
            filter_bins(Tree, Tail, [Bin2,Bin1 | Acc]);
        [{error, _}, _] ->
            lists:reverse(Acc)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO : write test cases to check
%% @doc verify_peak_hash/1 takes a peak hash list and verifies its against the
%% given root hash hence enabling the peer to guess the size of the DATA
%% @end
%% NOTE the Hash_List is being processed from higher to lower bin because the
%% highest bin will be in the lowest layer.
%% TODO decide if Root_Hash = {Root_Bin, Root_Hash}
-spec verify_peak_hash(hash_list(), hash()) -> true | false.
verify_peak_hash(Peak_List, Root_Hash) ->
    %% sort the list acc to the bin numbers and reverse it
    Hash_List = lists:reverse(lists:keysort(1, Peak_List)),
    verify_peaks(Hash_List, Root_Hash).

verify_peaks([{_Bin, Root_Hash}], Root_Hash) -> true; 
verify_peaks([{_Bin, _Hash}],    _Root_Hash) -> false; 
verify_peaks([{Bin, Hash} | Tail], Root_Hash) ->
    Sibling     = mtree_core:get_sibling(Bin),
    %% if it doesn't exist in list it means that the sibling bigger than the
    %% Bin and sibling the root of an empty subtree.
    Parent_Hash = case lists:keyfind(Sibling, 1, Tail) of
                      false ->
                          Sibling_Hash = get_subtree_hash(Sibling),
                          mtree_core:hash([Hash, Sibling_Hash]);
                      {_, Sibling_Hash} ->
                          mtree_core:hash([Sibling_Hash, Hash])
                  end,
    Parent_Bin  = erlang:round((Bin+Sibling)/2),
    verify_peaks([{Parent_Bin, Parent_Hash} | Tail], Root_Hash).

%% LOCAL internal function.
%% Get the root hash of the empty subtree
%% Note : The bin numbers are actually not needed. So, remove them
get_subtree_hash(Bin) ->
    [Start, End] = mtree_core:bin_to_range(Bin),
    Hash_List = [ ?SHA1_EMPTY_HASH || _X <- lists:seq(Start, End, 2)],
    get_subtree_hash(Hash_List, []).

get_subtree_hash([Root_Hash], []) ->
    Root_Hash;
get_subtree_hash([], [Root_Hash]) ->
    Root_Hash;
get_subtree_hash([], Acc) ->
    get_subtree_hash(Acc, []);
get_subtree_hash([Hash1,Hash2 | Tail], Acc) ->
    Parent_Hash = mtree_core:hash([Hash1, Hash2]),
    get_subtree_hash(Tail, [Parent_Hash | Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc get_hash_by_index/2 returns a hash or list of hashes for a given
%% Bin which may represent a single chunk or a range of chunks.
%% @end
-spec get_hash_by_index(mtree(), bin()) -> {ok, hash_list()}.
get_hash_by_index(Tree, Bin) ->
    [Start,End] = mtree_core:bin_to_range(Bin),
    get_hash_by_index(Tree, lists:seq(Start,End,2), []).

get_hash_by_index(_Tree, [], Acc) ->
    {ok, lists:reverse(Acc)};
get_hash_by_index(Tree, [Bin | Tail], Acc) ->
    {ok, Hash, _} = mtree_store:lookup(Tree, Bin),
    get_hash_by_index(Tree, Tail, [{Bin, Hash}|Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc get_uncle_hashes/2 returns list of uncle hashes required to verify
%% a given chunk.
%% @end
-spec get_uncle_hashes(mtree(), bin()) -> hash_list() | {error, _}.
get_uncle_hashes(Tree, Bin) when Bin rem 2 =:= 0 ->
    get_uncle_hashes(Tree, Bin, [], mtree_core:root_bin(Tree));
get_uncle_hashes(_Tree, _Bin) ->
    {error, mtree_not_a_leaf}.

get_uncle_hashes(_Tree, Bin, Acc, Root_Bin) when Root_Bin =:= Bin ->
    lists:reverse(Acc);
get_uncle_hashes(Tree, Bin, Acc, Root_Bin) ->
    case mtree_core:get_sibling(Bin) of
        %% if the required sibling is less than bin it means that that no
        %% further uncle hashes are required because further hashes have
        %% already been sent to verify previous bins or chunk
        Sibling when Sibling < Bin ->
            lists:reverse(Acc);
        Sibling when Sibling > Bin ->
            case mtree_store:lookup(Tree, Sibling) of
                {error, _} -> {error, mtree_missing_uncle_hashes};
                {ok, Hash, _}      ->
                    New_Root = erlang:round((Sibling+Bin)/2),
                    get_uncle_hashes(Tree, New_Root,
                                     [{Sibling, Hash} | Acc], Root_Bin)
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc get_uncle_hashes/2 returns list of uncle hashes required to verify
%% a given chunk.
%% @end
-spec get_all_uncle_hashes(mtree(), bin()) -> hash_list() | {error, _}.
get_all_uncle_hashes(Tree, Bin) when Bin rem 2 =:= 0 ->
    get_all_uncle_hashes(Tree, Bin, [], mtree_core:root_bin(Tree));
get_all_uncle_hashes(_Tree, _Bin) ->
    {error, mtree_not_a_leaf}.

get_all_uncle_hashes(_Tree, Bin, Acc, Root_Bin) when Root_Bin =:= Bin ->
    lists:reverse(Acc);
get_all_uncle_hashes(Tree, Bin, Acc, Root_Bin) ->
    Sibling = mtree_core:get_sibling(Bin),
    case mtree_store:lookup(Tree, Sibling) of
        {error, _}    -> {error, mtree_missing_uncle_hashes};
        {ok, Hash, _} ->
            New_Root = erlang:round((Sibling+Bin)/2),
            get_all_uncle_hashes(Tree, New_Root,
                                 [{Sibling, Hash} | Acc], Root_Bin)
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
%% TODO incomplete, needs thorough testing
-spec verify_uncle_hash(mtree(),hash_list(),bin_hash()) -> {ok, true}
                                                         | {error, _}.
verify_uncle_hash(Tree, Hash_List, {Bin, Hash}) ->
    verify_uncle_hash(Tree, Hash_List, root_hash(Tree), {Bin, Hash}).

verify_uncle_hash(Tree, Hash_List, {Root_Bin, Root_Hash}, {Bin, Hash})
  when Bin rem 2 =:= 0 ->
    case verify_uncle_hash(Tree, Hash_List, {Root_Bin, Root_Hash},
                           {Bin, Hash}, []) of
        %% Uncle_Hashes also includes all the intermediate parents calculated
        %% using the actual uncle hashes
        {ok, true, Uncle_Hashes} ->
            {ok, insert_uncle_hashes(Tree, Uncle_Hashes)};
        {ok, false, _} ->
            {error, mtree_uncle_hash_mismatch};
        {error, Msg, _} ->
            {error, Msg}
    end;
verify_uncle_hash(_, _, _, _) ->
    {error, mtree_not_a_leaf}.

verify_uncle_hash(_Tree, [], {R_Bin, R_Hash}, {R_Bin, Hash}, Acc) ->
    {ok, mtree_core:compare(R_Hash, Hash), Acc};
verify_uncle_hash(Tree, [], {R_Bin, R_Hash}, {Bin, Hash}, Acc) ->
    {Status, Value} = verify(Tree, {R_Bin, R_Hash}, {Bin, Hash}),
    {Status, Value, Acc};
verify_uncle_hash(Tree, Hash_List, {R_Bin, R_Hash}, {Bin, Hash}, Acc) ->
    Sibling =  mtree_core:get_sibling(Bin),
    case get_hash(Hash_List, Sibling) of
        error        ->
            {error, mtree_missing_uncle_hashes_in_list, empty};
        Sibling_Hash ->
            Parent_Hash   = mtree_core:hash([Hash, Sibling_Hash]),
            Parent_Bin    = erlang:round((Bin + Sibling)/2),
            New_Hash_List = lists:keydelete(Sibling, 1, Hash_List),
            New_Acc = [{Sibling, Sibling_Hash},
                       {Parent_Bin, Parent_Hash} | Acc],

            verify_uncle_hash(Tree, New_Hash_List,{R_Bin, R_Hash},
                              {Parent_Bin, Parent_Hash}, New_Acc)
    end.

%% HELPER fun
%% get hash from the Hash_List
get_hash(Hash_List, Bin) ->
    case lists:keyfind(Bin, 1, Hash_List) of
        {_, Hash} -> Hash;
        false     -> error
    end.

%% HELPER fun
%% insert Uncle_Hashes into the tree
insert_uncle_hashes(Tree, Hash_List) ->
    lists:map(fun({Bin, Hash}) ->
                      mtree_store:insert(Tree,{Bin, Hash, empty})
              end, Hash_List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc verify/2 for a given bin bumber and its hash it recalculates the root
%% hash And verifies its against the root hash
%% @end
%% TODO thorough checking.
-spec verify(mtree(), bin_hash(), bin_hash()) -> {ok, true}
                                               | {ok, false}
                                               | {error, _}.
verify(_, {Root_Bin, Root_Hash}, {Bin, Hash}) when Root_Bin =:= Bin ->
    {ok, mtree_core:compare_hash(Root_Hash, Hash)};
verify(Tree, {Root_Bin, Root_Hash}, {Bin, Hash}) ->
    Sibling = mtree_core:get_sibling(Bin),
    case mtree_store:lookup(Tree, Sibling) of
        {ok, Sibling_Hash, _} ->
            Parent_Bin  = erlang:round((Bin + Sibling)/2),
            Parent_Hash = if
                              Bin < Sibling ->
                                  mtree_core:hash([Hash, Sibling_Hash]);
                              Bin > Sibling ->
                                  mtree_core:hash([Sibling_Hash, Hash])
                          end,
            verify(Tree, {Root_Bin, Root_Hash}, {Parent_Bin, Parent_Hash});
        {error, _} ->
            {error, mtree_missing_hashes_in_tree}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc get_munro_uncles/2 returns the uncle hashes required to check the
%% intergrity of the given chunk.
%% @end
-spec get_munro_uncles(mtree(), [integer()]) -> hash().
get_munro_uncles(Tree, Bin) when Bin rem 2 =:= 0 ->
    Munro_Root = mtree_core:get_munro_root(Bin),
    get_uncle_hashes(Tree, Bin, [], Munro_Root);
get_munro_uncles(_Tree, _Bin) ->
    {error, mtree_not_a_leaf}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc get_all_munro_uncles/2 returns the all munro hashes required to verify
%% the intergrity regardless of previous chunks sent.
%% @end
-spec get_all_munro_uncles(mtree(), [integer()]) -> hash().
get_all_munro_uncles(Tree, Bin) when Bin rem 2 =:= 0 ->
    Munro_Root = mtree_core:get_munro_root(Bin),
    get_all_uncle_hashes(Tree, Bin, [], Munro_Root);
get_all_munro_uncles(_Tree, _Bin) ->
    {error, mtree_not_a_leaf}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc This function is helpful during SECURE TUNE IN in a live stream, this
%% funcitons gets the most recent munro root bin.  Since the injector will
%% generated chunks that are a fixed power of 2 so the muro root will always
%% lie in layer corresponding to the power of 2.
%% @end
-spec get_latest_munro(mtree()) -> bin() | {error, _}.
get_latest_munro(Tree) ->
    get_latest_munro(Tree, ?NCHUNKS_PER_SIG -1, ?NCHUNKS_PER_SIG *2).

get_latest_munro(Tree, Bin, Diff) ->
    case mtree_store:is_member(Tree, Bin) of
        true  -> get_latest_munro(Tree, Bin+Diff, Diff);
        false -> mtree_core:lookup(Tree, Bin-Diff)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO : write test cases to check this.
%% @doc verify_munro_hash/2 returns true if the calculated Munro_Hash from the
%% Uncle_Hashes is equal to the received Munro_Hash.
%% <p> Uses verify function internally which is provided the new subtree as
%% Tree and Munro_Hash as Root_Hash as its arguments.</p>
%% @end
-spec verify_munro_hash(mtree(), hash(), hash_list()) -> {true, mtree()}
                                                       | {false, mtree()}.
verify_munro_hash(Tree, {Bin, Hash}, Munro_Uncle_Hashes)
    when Bin rem 2 =:= 0 ->
    Munro_Root = mtree_core:get_munro_root(Bin),
    case mtree_store:lookup(Tree, Munro_Root) of
        {error, _}          -> {error, mtree_missing_munro_root};
        {ok, Munro_Hash, _} ->
            verify_uncle_hash(Tree, Munro_Uncle_Hashes,
                              {Munro_Root, Munro_Hash}, {Bin, Hash})
    end;
verify_munro_hash(_Tree, _, _) ->
    {error, mtree_not_a_leaf}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc determines the APPROXIMATE size of the given data. Returns the number
%% of data chunks spanned by the given peaks which can be multiplied with
%% CHUNK_SIZE to get the approx. data size.
%% @end
-spec peaks_to_size(hash_list()) -> non_neg_integer().
peaks_to_size(Hash_List) ->
    lists:mapfoldl( fun(Bin, Size) ->
                            [Start, End] = mtree_core:bin_to_range(Bin),
                            erlang:round((End - Start)/2) + 1 + Size
                    end, [Bin || {Bin,_} <- Hash_List]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc dump_tree/2 write the merkle hash tree into a file with name as
%% File_Name. Returns ok if the operation succeeded.
%% @end
-spec load_tree(string()) -> {ok, atom()} | {error, _}.
load_tree(File_Name) ->
    mtree_store:file_to_table(File_Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc load_tree/1 loads merkle hash tree from a file with name as File_Name.
%% @end
-spec dump_tree(mtree()) -> ok.
dump_tree(Tree) ->
    mtree_store:table_to_file(Tree).
