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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc This module contatins helpers function for the merkle tree API
%% functions.
%% @end
-module(mtree_core).

-define(EMPTY_HASH, <<0:160>>).
-define(ALGO, sha).

-export([hash/1,
         compare_hash/2,
         get_sibling/1,
         get_layer_num/1,
         bin_to_range/1,
         pad_tree/1,
         nearest_power_2/1,
         is_complete/1,
         next_bin/1,
         tree_length/1]).

-opaque mtree()     :: {term()}.
%-type hash()        :: binary().
%-type hash_list()   :: [hash()].

-export_type([mtree/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc returns hash of Hash_List
%% @end
hash(Hash_List) ->
    crypto:hash(?ALGO, Hash_List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc returns hash of Hash_List
%% @end
compare_hash(Hash1, Hash2) when Hash1 =:= Hash2 ->
    true;
compare_hash(_H1, _H2) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc returns the sibling bin number for a bin number belonging to a given
%% layer
%% @end
get_sibling(Bin) ->
    Layer = get_layer_num(Bin)-1,
    Nth   = erlang:round((Bin-math:pow(2,Layer)+1)/math:pow(2,Layer+1))+1,
    if
        Nth rem 2 =:= 0 -> Bin - erlang:round(math:pow(2,Layer+1));
        true            -> Bin + erlang:round(math:pow(2,Layer+1))
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Return the layer number to which a particular bin belongs.
%% @end
get_layer_num(Bin) ->
    [Start, End] = bin_to_range(Bin),
    erlang:round(math:log(End+2 -Start)/math:log(2)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO add check for Bin to be an integer in the gaurd
%% @doc Return [Start, End] range of leaf nodes for a given Bin
%% @end
-spec bin_to_range(integer()) -> list().
bin_to_range(Bin) when Bin >= 0 ->
    if
        %% if Bin is even then its a leaf, hence no range is returned
        Bin rem 2 =:= 0 ->
            [Bin, Bin];
        true ->
            bin_to_range(Bin, 1)
    end;
bin_to_range(_Bin) ->
    invalid_bin.

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
    New_Length  = nearest_power_2(Curr_Length),
    pad_tree(Tree, Curr_Length, New_Length).

pad_tree(_Tree, Start, End) when Start =:= End ->
    End-2;
pad_tree(Tree, Start, End) ->
    mtree_store:insert(Tree, {Start, ?EMPTY_HASH, empty}),
    pad_tree(Tree, Start+2, End).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Get the next nearest power of 2
%% @end
nearest_power_2(Number) when Number band (Number-1) =:= 0 ->
    Number;
nearest_power_2(Number) ->
    nearest_power_2(Number, 0).

nearest_power_2(0, Count) ->
    1 bsl Count;
nearest_power_2(Number, Count) ->
    nearest_power_2(Number bsr 1, Count+1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO check if this will return correct answer in only uncle hashes are
%% inserted
%% @doc check if tree has 2^N leaf nodes i.e. Bin band (Bin-1) == 0 or not and
%% also check if the root (2^(N-1)-1 or Bin/2 -1) exists.
%% @end
is_complete(Tree) ->
    Bin      = tree_length(Tree),
    Root_Bin = erlang:round(Bin/2)-1,
    case {Bin band (Bin-1), mtree_store:is_member(Tree, Root_Bin)} of
        {0, true} -> {true, Root_Bin};
        _         -> {false, none}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO speed up this function by staring from last and moving back to find the
%% %% empty leaf node
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
