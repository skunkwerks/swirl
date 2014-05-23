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
         root_hash/1,
         get_peak_hash/1,
         get_hash_by_index/2,
         get_uncle_hashes/2,
         get_munro_hash/2,
         verify_peak_hash/1,
         verify_munro_hash/3,
         verify_uncle_hash/3,
         dump_tree/1,
         load_tree/1,
         bin_to_range/1]).

-opaque mtree()     :: {term()}.
-type hash()        :: binary().
-type hash_list()   :: [hash()].

-export_type([mtree/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Initialize a new merkle hash tree.
%% @end
-spec new(atom()) -> ok.
new(Tree) ->
    mtree_store:init(Tree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc insert/2 adds {Bin, Hash, Data} into the ETS table
%% <p> Provides interface to insert hashes into the ETS table. </p>
%% @end
-spec insert(mtree(),{binary(), term()}) -> true.
insert(Tree, {Hash,Data}) ->
    %% get next bin number where the hash is to be inserted
    Bin = next_bin(Tree),
    mtree_store:insert(Tree, {Bin, Hash, Data}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
-spec build_tree(list()) -> Root_Hash :: binary().
build_tree(Tree) ->
    %% pad the tree with empty hashes, returns the last Bin that was
    %% added to pad the tree.
    Tree_Size = pad_tree(Tree),
    Bin_List  = lists:seq(0, Tree_Size, 2),
    build_tree(Tree, Bin_List, []).

build_tree(Tree, [], [Root_Bin]) ->
    {ok, Root_Hash, _} = mtree_store:lookup(Tree, Root_Bin),
    Root_Hash;

build_tree(Tree, [], Bin_List) ->
    build_tree(Tree, Bin_List, []);

build_tree(Tree, [Bin1, Bin2 | Tail], Acc) ->
    {ok, Hash1, _} = mtree_store:lookup(Tree, Bin1),
    {ok, Hash2, _} = mtree_store:lookup(Tree, Bin2),
    Hash           = hash([Hash1, Hash2]),
    Bin            = math:round((Bin1+Bin2)/2),

    mtree_store:insert(Tree, {Bin, Hash, empty}),
    build_tree(Tree, Tail, lists:append([Acc, [Bin]])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO discuss this !
%% @doc root_hash/1 get the root hash of the tree. The root of a full binary
%% tree that uses bin number scheme will always be of the form 2^N -1 where N
%% is the number of leaf nodes in the tree. This function assumes that the
%% binary tree is full.
%% @end
-spec root_hash(mtree()) -> hash().
root_hash(Tree) ->
    case is_complete(Tree) of

        {false, none}    ->
            build_tree(Tree),
            root_hash(Tree);

        {true, Root_Bin} ->
            {ok, Hash, _} = mtree_store:lookup(Tree, Root_Bin),
            Hash
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc get_peak_hash/1 returns the peak hashes of the given tree.
%% <p> Provides an interface to get the peak hashes that are use by the receiver
%% for reliable file size detection and download/live streaming unification
%% </p>
%% @end
-spec get_peak_hash(mtree()) -> hash_list().
get_peak_hash(Tree) ->
    case is_complete(Tree) of
        {false, none}    ->
            build_tree(Tree),
            get_peak_hash(Tree);
        {true, Root_Bin} ->
            Bin_List = lists:seq(0,2*Root_Bin,2),
            get_peak_hash(Tree, Bin_List, [], [])
    end.

%% TODO write a calculate peaks function to reduce the no. of lookup operations.
%% TODO Also to enhance the speed one can stop moving forward once the first empty
%% hash is found
%%
%% Takes a list of leaf nodes and traverses up the tree to get the peak hashes.
get_peak_hash(_Tree, [], [], Peaks) ->
    Peaks;

get_peak_hash(Tree, [], Acc, Peaks) ->
    get_peak_hash(Tree, lists:reverse(Acc), [], Peaks);

get_peak_hash(Tree, [Bin1,Bin2 | Tail], Acc, Peaks) ->
    case [mtree_store:lookup(Tree, Bin1),
          mtree_store:lookup(Tree, Bin2)] of

        [{ok, ?EMPTY_HASH, _}, _] ->
            get_peak_hash(Tree, Tail, Acc, Peaks);

        [_, {ok, ?EMPTY_HASH, _}] ->
            {ok, Hash,_} = mtree_store:lookup(Tree, Bin1),
            get_peak_hash(Tree, Tail, Acc, [{Bin1,Hash}|Peaks]);

        _   ->
            New_Bin = erlang:round((Bin1+Bin2)/2),
            get_peak_hash(Tree, Tail, [New_Bin | Acc], Peaks)
    end;

get_peak_hash(Tree, [Bin], Acc, Peaks) ->
    {ok, Hash,_} = mtree_store:lookup(Tree, Bin),
    get_peak_hash(Tree, [], Acc, [{Bin,Hash}|Peaks]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc verify_peak_hash/1 takes a peak hash list and verifies its against the
%% given root hash hence enabling the peer to guess the size of the DATA
%% @end
%% TODO decide if Root_Hash = {Root_Bin, Root_Hash}
%% TODO write function to calculate data size from peak hashes.
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
    Sibling     = get_sibling(Bin, get_layer_num(Bin)-1),
    Parent_Hash = case lists:keyfind(Sibling, 1, Tail) of
                      false ->
                          Sibling_Hash = get_subtree_hash(Sibling),
                          hash([Hash, Sibling_Hash]);
                      {_, Sibling_Hash} ->
                          hash([Sibling_Hash, Hash])
                  end,
    Parent_Bin  = erlang:round((Bin+Sibling)/2),
    Hash_List   = [{Parent_Bin, Parent_Hash} | Tail],
    verify_peak_hash(Hash_List, Root_Hash).

%% Get the root hash of the empty subtree
get_subtree_hash(Bin) ->
    [Start, End] = bin_to_range(Bin),
    Hash_List = [{X,?EMPTY_HASH} || X <- lists:seq(Start, End, 2)],
    get_subtree_hash(Hash_List, []).

get_subtree_hash([], [{_, Root_Hash}]) ->
    Root_Hash;
get_subtree_hash([], Acc) ->
    get_subtree_hash(Acc, []);
get_subtree_hash([{Bin1, Hash1}, {Bin2,Hash2} | Tail], Acc) ->
    Parent_Bin  = erlang:round((Bin1+Bin2)/2),
    Parent_Hash = hash([Hash1, Hash2]),
    get_subtree_hash(Tail, [{Parent_Bin, Parent_Hash} | Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc get_hash_by_index/2 returns a hash or list of hashes for a given
%% Bin which may represent a single chunk or a range of chunks.
%% @end
-spec get_hash_by_index(mtree(), integer()) -> hash_list().
get_hash_by_index(Tree, Bin) ->

    case is_complete(Tree) of
        {false, none}    ->
            build_tree(Tree),
            get_hash_by_index(Tree, Bin);
        {true, _Root_Bin} ->
            [Start,End] = bin_to_range(Bin),
            get_hash_by_index(Tree, lists:seq(Start,End,2), [])
    end.

get_hash_by_index(_Tree, [], Acc) ->
    lists:reverse(Acc);
get_hash_by_index(Tree, [Bin | Tail], Acc) ->
    {ok, Hash, _} = mtree_store:lookup(Tree, Bin),
    get_hash_by_index(Tree, Tail, [Hash|Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc get_uncle_hashes/2 returns list of uncle hashes required to verify
%% a given chunk.
%% @end
-spec get_uncle_hashes(mtree(), integer()) -> hash_list().
get_uncle_hashes(Tree, Bin) when Bin rem 2 =:= 0 ->
    get_uncle_hashes(Tree, Bin, 0);
get_uncle_hashes(_Tree, _Bin) ->
    {error, not_a_leaf}.

get_uncle_hashes(Tree, {Bin, Layer}, Acc) ->
    Sibling = get_sibling(Bin,Layer),
    if
        Sibling < Bin ->
            lists:reverse(Acc);
        Sibling > Bin ->
            {ok, Hash, _} = mtree_store:lookup(Tree, Sibling),
            Root  = erlang:round((Sibling+Bin)/2),
            get_uncle_hashes(Tree, {Root, Layer+1}, [{Sibling, Hash} | Acc])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO : verify if it matches the specs.
%% @doc Given a Hash_List of the type [{Bin1,Hash1}, {Bin2,Hash2}, ...]
%% verify_uncle_hash/3 for a given chunk ID (i.e. Bin Number), inserts the
%% Uncle_Hashes into the tree and returns true if the new Root_Hash of the tree
%% after addition of a new chunk is same as the received Root_Hash.
%% <p> When a peer receives a new chunk it MUST receive Uncle_Hashes required
%% to verify to calculate the Root_Hash and check it against the received
%% Root_Hash </p>
%% @end
%%
%%
%% TODO incomplete
-spec verify_uncle_hash(mtree(), hash_list(), hash()) -> {true, mtree()}
                                                       | {false,term()}.
verify_uncle_hash(Tree, Bin, {Hash_List,_Root_Hash}) when Bin rem 2 =:= 0 ->
    {ok, Hash, _} = mtree_store:lookup(Tree, Bin),
    verify_uncle_hash(Tree, Hash_List, {Bin, Hash}, 0);
verify_uncle_hash(_Tree, _Bin, _) ->
    not_a_leaf.

verify_uncle_hash(Tree, Hash_List, {Bin, Bin_Hash}, Layer) ->
    Sibling = get_sibling(Bin, Layer),
    if
        Sibling < Bin ->
            ok;
            %verify(Tree, Bin, Bin_Hash, Layer);

        Sibling > Bin ->
            case get_sibling_hash(Hash_List, Sibling) of
                error        -> {error, hash_not_found};
                Sibling_Hash ->
                    Parent_Hash = hash([Bin_Hash, Sibling_Hash]),
                    Parent_Bin  = erlang:round((Bin + Sibling)/2),
                    verify_uncle_hash(Tree, Hash_List,
                              {Parent_Bin, Parent_Hash}, Layer+1)
            end
    end.

get_sibling_hash(Hash_List, Bin) ->
    case lists:keyfind(Bin, 1, Hash_List) of
        {_, Hash} -> Hash;
        false     -> error
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
-spec dump_tree(string()) -> {ok, atom()} | {error, term()}.
dump_tree(File_Name) ->
    mtree_store:file_to_table(File_Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc load_tree/1 loads merkle hash tree from a file with name as File_Name.
%% @end
-spec load_tree(atom()) -> ok.
load_tree(Tree) ->
    mtree_store:table_to_file(Tree).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GENERAL INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc returns hash of Hash_List
%% @end
hash(Hash_List) ->
    crypto:hash(?ALGO, Hash_List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc returns the sibling bin number for a bin number belonging to a given
%% layer
%% @end
get_sibling(Bin, Layer) ->
    Nth = erlang:round((Bin-math:pow(2,Layer)+1)/math:pow(2,Layer+1))+1,
    if
        Nth rem 2 =:= 0 -> Bin - erlang:round(math:pow(2,Layer+1));
        true            -> Bin + erlang:round(math:pow(2,Layer+1))
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc returns the sibling bin number for a bin number belonging to a given
%% layer
%% @end
get_layer_num(Bin) ->
    [Start, End] = bin_to_range(Bin),
    erlang:round(math:log(End+2 -Start)/math:log(2)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO add check to ensure that Bin is an integer
%% @doc Return [Start, End] range of leaf nodes for a given Bin
%% @end
-spec bin_to_range(integer()) -> list().
bin_to_range(Bin) ->
    if
        %% if Bin is even then its a leaf, hence no range is returned
        Bin rem 2 =:= 0 ->
            [];
        %% Bin has to be a positive integer.
        Bin >= 0 ->
            %io:format("~w~n", [bin_to_range(Bin, 1)]);
            bin_to_range(Bin, 1);
        true ->
            error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Using arithmetic progression check if the given Bin lies in the
%% given level of the tree..
%% @end
bin_to_range(Bin, Level) ->
    Nth = ((Bin+1)/math:pow(2,Level)-1)/2,
    case Nth - erlang:round(Nth)  of
        0.0 ->
            Range = erlang:round(math:pow(2,Level)),
            [Bin+1-Range, Bin-1+Range];

        _ ->
            bin_to_range(Bin, Level+1)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Pad tree with empty leaf hashes.
%% @end
pad_tree(Tree) ->
    Curr_Length = tree_length(Tree),
    New_Length  = next_power_2(Curr_Length),
    pad_tree(Tree, Curr_Length, New_Length).

pad_tree(Tree,Start,End) ->
    if
        Start=:=End -> End-2;
        true        ->
            mtree_store:insert(Tree, {Start, ?EMPTY_HASH, empty}),
            pad_tree(Tree, Start+2, End)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Get the next nearest power of 2
%% @end
next_power_2(Number) ->
    if
        Number band (Number-1) =:= 0 -> Number;
        true -> next_power_2(Number, 0)
    end.

next_power_2(0, Count) ->
    1 bsl Count;
next_power_2(Number, Count) ->
    next_power_2(Number bsr 1, Count+1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc check if tree has 2^N leaf nodes and also check if the root (2^(N-1)-1
%% or Bin/2 -1) exists.
is_complete(Tree) ->
    Bin      = tree_length(Tree),
    Root_Bin = erlang:round(Bin/2)-1,
    case {Bin band (Bin-1), mtree_store:is_member(Tree, Root_Bin)} of
        {0, true} -> {true, Root_Bin};
        _         -> {false, none}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO speed up this function.
%% @doc generate the next bin number where the hash has to inserted. The
%% next bin number should either not exist in the tree or it should have an
%% empty hash
%% @end
next_bin(Tree) ->
    next_bin(Tree, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc next_bin/3 returns Bin which next to the highest Bin in the tree.
%% @end
next_bin(Tree, Bin) ->
    case mtree_store:lookup(Tree, Bin) of
        {error, not_found}    -> Bin;
        {ok, ?EMPTY_HASH, _D} -> Bin;
        {ok, _H, _D}          -> next_bin(Tree, Bin+2)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc get highest Bin of the leaf node, which indicates the number of
%% elements in the tree or the tree breadth/size.
%% NOTE: Since the ETS table is an ordered storage the last element will always
%% be a leaf node. So, generated bin will get the last entry in the ETS table
%% and return it which can used to calculate the ROOT of the tree
%% @end
tree_length(Tree) ->
    mtree_store:highest_bin(Tree)+2.
