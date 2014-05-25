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
%% @doc This module implements test cases for mtree_store module.
%% @end
-module(mtree_tests).

-define(DATA, "/home/kansi/Documents/done/code/swirl/test/data/m74.jpg").
-define(CHUNK, 1024).
%-define(ALGO, sha).
-define(TABLE, test).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

prop_insert() ->
    mtree:new(?TABLE),
    ?FORALL({Data}, {binary()}, mtree:insert(?TABLE, Data)).

prop_test_layer_num() ->
    ?FORALL({Num}, {non_neg_integer()},
             begin
                Layer = mtree_core:get_layer_num(Num),
                Start = math:pow(2,Layer-1) -1,
                Diff  = math:pow(2,Layer),
                Nth   = (Num - Start)/Diff,
                Nth  == erlang:round(Nth)
             end
           ).

prop_get_sibling() ->
    ?FORALL({Bin}, {non_neg_integer()},
             begin
                Sibling = mtree_core:get_sibling(Bin),
                Diff    = erlang:abs(Sibling - Bin),
                Diff   == mtree_core:nearest_power_2(Diff)
             end
           ).

prop_bin_to_range() ->
    ?FORALL({Bin}, {non_neg_integer()},
             begin
                 [Start, End] = mtree_core:bin_to_range(Bin),
                 Diff         = erlang:abs(Start - End) + 2,
                 Start rem 2  =:= 0 andalso
                 End rem 2    =:= 0 andalso
                 Diff         == mtree_core:nearest_power_2(Diff)
             end
           ).

pad_tree_test() ->
    End = mtree_core:pad_tree(?TABLE),
    ?assertEqual((End+2), mtree_core:nearest_power_2(End)).

prop_is_complete() ->
    ok.



%init_table() ->
    %%% Initialize table
    %mtree:new(?TABLE),
    %{ok, Fdr} = file:open(?DATA, [read]),
    %read_file(Fdr),
    %%mtree:build_tree(test),
    %ok.

%read_file(Fd) ->
    %case file:read(Fd, ?CHUNK) of
        %{ok, Data} ->
            %mtree:insert(?TABLE, Data),
            %read_file(Fd);
        %eof ->
            %ok
    %end.
