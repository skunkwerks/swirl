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
-module(mtree_store_tests).

-define(DATA, "TestData").
-define(HASH, crypto:sha(?DATA)).
-define(TABLE, test).

-include_lib("eunit/include/eunit.hrl").

storage_test() ->

    %% Initialize table
    mtree_store:init(?TABLE),

    %% insert data into table
    insert_data(),

    %% lookup test
    ?assertEqual({ok, ?HASH, ?DATA}, mtree_store:lookup(0, ?TABLE)),
    ?assertEqual({error, not_found}, mtree_store:lookup(5, ?TABLE)),

    %% delete test
    mtree_store:delete(0, ?TABLE),
    ?assertEqual({error, not_found}, mtree_store:lookup(0, ?TABLE)),

    %% dump table to disk
    mtree_store:table_to_file(?TABLE),
    %% dump table test
    ?assertEqual(undefined, ets:info(?TABLE)),

    %% load table back
    mtree_store:file_to_table(lists:concat([?TABLE, ".ets"])),
    %% load table test
    ?assertNotEqual(undefined, ets:info(?TABLE)),

    %% clear up the table
    ets:delete(?TABLE),
    ok.

insert_data() ->
    mtree_store:insert({0, ?HASH, ?DATA}, ?TABLE),
    mtree_store:insert({1, ?HASH, ?DATA}, ?TABLE),
    mtree_store:insert({2, ?HASH, ?DATA}, ?TABLE),
    mtree_store:insert({3, ?HASH, ?DATA}, ?TABLE),
    mtree_store:insert({4, ?HASH, ?DATA}, ?TABLE).
