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
         remove/2,
         build_tree/1,
         root_hash/1,
         get_peak_hash/1,
         get_hash_by_index/2,
         get_uncle_hashes/2,
         get_munro_hash/2,
         verify_munro_hash/3,
         verify/3,
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
new(Name) ->
    mtree_store:init(Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc insert/2 adds {Bin_Number, Hash, Data} into the ETS table
%% <p> Provides interface to insert hashes into the ETS table. </p>
%% @end
-spec insert(mtree(),{binary(), term()}) -> true.
insert(Tree, {Hash,Data}) ->
    %% get next bin number where the hash is to be inserted
    Bin_Number = next_bin(Tree),
    mtree_store:insert(Tree, {Bin_Number, Hash, Data}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO decide what this function will do !!
%% @doc remove/3 removes a given hash from tree and re-calculate the root hash
%% @end
-spec remove(mtree(), integer()) -> true.
remove(Tree, Bin_Number) ->
    mtree_store:delete(Tree, Bin_Number).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO maybe use spawn(build_tree/3) to speed up the process.
%% @doc Takes input the start and end Bin_Number of the leaf nodes and
%% constructs a tree using the leaf nodes stored in the ETS table and stores
%% the tree back into the ETS table.
%% @end
-spec build_tree(list()) -> Root_Hash :: binary().
build_tree(Tree) ->
    %% pad the tree with empty hashes, and return size of new tree
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
    Hash           = crypto:hash(?ALGO, [Hash1, Hash2]),
    Bin_Number     = math:round((Bin1+Bin2)/2),

    mtree_store:insert(Tree, {Bin_Number, Hash, empty}),
    build_tree(Tree, Tail, lists:append([Acc, [Bin_Number]])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO discuss this !
%% @doc root_hash/1 get the root hash of the tree. The root of a full binary
%% tree that uses bin number scheme will always be of the form 2^N -1 where N
%% is the number of leaf nodes in the tree. This function assumes that the
%% binary tree is full.
%% @end
-spec root_hash(mtree()) -> hash().
root_hash(Tree) ->
    %% TODO check if the tree is complete or not
    root_hash({Tree, 0}, ?EMPTY_HASH).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc This function assumes that the binary tree is full i.e. there are 2^N
%% leaf nodes in the tree.
%% @end
root_hash({Tree, N}, Root_Hash) ->
    Root_Bin = erlang:round(math:pow(2,N))-1,

    case mtree_store:lookup(Tree, Root_Bin) of
        {error,not_found}   -> Root_Hash;
        {ok, Hash, _Data}   -> root_hash({Tree, N+1}, Hash)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc get_peak_hash/1 returns the peak hashes of the given tree.
%% <p> Provides an interface to get the peak hashes that are use by the receiver
%% for reliable file size detection and download/live streaming unification
%% </p>
%% @end
-spec get_peak_hash(mtree()) -> hash_list().
get_peak_hash(_Tree) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc get_uncle_hashes/2 returns list of uncle hashes required to verify
%% a given chunk.
%% @end
-spec get_uncle_hashes(mtree(), integer()) -> hash_list().
get_uncle_hashes(_Tree, _Chunk_Id) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc get_hash_by_index/2 returns a hash or list of hashes for a given
%% Bin_Number which may represent a single chunk or a range of chunks.
%% @end
-spec get_hash_by_index(mtree(), integer()) -> hash_list().
get_hash_by_index(_Tree, _Bin_Number) -> ok.

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
get_munro_hash(_Tree, _Bin_Numbers) -> ok.

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
%% TODO : verify if it matches the specs.
%% @doc verify/3 inserts the Uncle_Hashes into the tree and returns true if the
%% new Root_Hash of the tree after addition of a new chunk is same as the
%% received Root_Hash.
%% <p> When a peer receives a new chunk it MUST receive Uncle_Hashes required
%% to verify to calculate the Root_Hash and check it against the received
%% Root_Hash </p>
%% @end
-spec verify(mtree(), hash_list(), hash()) -> {true, mtree()}
                                            | {false,mtree()}.
verify(_Tree, _Uncle_Hashes, _Root_Hash) -> ok.

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
%% TODO add check to ensure that Bin_Number is an integer
%% @doc Return [Start, End] range of leaf nodes for a given Bin_Number
%% @end
-spec bin_to_range(integer()) -> list().
bin_to_range(Bin_Number) ->
    if
        %% if Bin_Number is even then its a leaf, hence no range is returned
        Bin_Number rem 2 =:= 0 ->
            [];
        %% Bin_Number has to be a positive integer.
        Bin_Number >= 0 ->
            %io:format("~w~n", [bin_to_range(Bin_Number, 1)]);
            bin_to_range(Bin_Number, 1);
        true ->
            error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Using arithmetic progression check if the given Bin_Number lies in the
%% given level of the tree..
%% @end
bin_to_range(Bin_Number, Level) ->
    Nth = ((Bin_Number+1)/math:pow(2,Level)-1)/2,
    case Nth - erlang:round(Nth)  of
        0.0 ->
            Range = erlang:round(math:pow(2,Level)),
            [Bin_Number+1-Range, Bin_Number-1+Range];

        _ ->
            bin_to_range(Bin_Number, Level+1)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Pad tree with empty leaf hashes.
%% @end
pad_tree(Tree) ->
    Curr_Length = tree_length(Tree)+2,
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
%% @doc generate the next bin number where the hash has to inserted. Here the
%% next bin number should either not exist in the tree or it should have an
%% empty hash
%% @end
next_bin(Tree) ->
    generate_bin(Tree, 0, ?EMPTY_HASH).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc get highest Bin_Number of the leaf node, which indicates the number of
%% elements in the tree or the tree breadth/size. NOTE : the EMPTY_HASH has
%% been set to none so that all the empty leaf nodes are also included in the
%% tree size.
%% @end
tree_length(Tree) ->
    generate_bin(Tree, 0, none)-2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc generate the next bin number where the hash has to inserted. Here the
%% next bin number should either not exist in the tree or it should have an
%% empty hash
%% NOTE : next_bin/3 return Bin_Number which on next the highest Bin_Number in
%% the tree.
%% @end
generate_bin(Tree, Bin_Number, Hash) ->
    case mtree_store:lookup(Tree, Bin_Number) of
        {error, not_found}  -> Bin_Number;
        {ok, Hash, _D}      -> Bin_Number;
        {ok, _H, _D}        -> generate_bin(Tree, Bin_Number+2, Hash)
    end.
