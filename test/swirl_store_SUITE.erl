%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.

%% @doc Swirl Tests.
%% @end

-module(_SUITE).
-include("swirl.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([minimal_store/1
        ]).

-spec all() -> [atom()].
all() -> [minimal_store
         ].

-spec minimal_store(any()) -> true.
minimal_store(_Config) ->
    %% store format using maps
    {ok, File} = file:read_file("./test/data/m74-1Kb.jpg"),
    Root = crypto:hash(sha, File),
    % TODO add setters to ppspp_options module and remove this cruft.
    % We want standard options apart from sha256 and chunk size.
    Options = {options,[{chunk_addressing_method,chunking_32bit_chunks},
              {chunk_size,1024},
              {content_integrity_check_method,merkle_hash_tree},
              {merkle_hash_tree_function,sha},
              {minimum_version,1},
              {supported_version,1},
              {swarm_id,Root}]}.
    Chunk_size = ppspp_options:get_chunk_size(Options).
    Hash_algorithm =  ppspp_options:get_merkle_hash_tree_function(Options).
    Number_of_chunks = (byte_size(File) div Chunk_size) + 1.
    Empty_array = array:new(Number_of_chunks).
    Filled_array = array:set(0, {Root, File}, Empty_array).
    
    Store = #{
      store => Filled_array,
      options => Options}.
      
        true = ok.
