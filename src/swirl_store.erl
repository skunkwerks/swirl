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

%% @author Dave Cottlehuber <dch@skunkwerks.at>
%% @copyright 2014-2038 Dave Cottlehuber
%% @end


%% @doc A Merkle Hash tree store with binmap index support.
%%
%% This module provides an API to a generic indexed key value store.
%% The swirl_store provides a simple API around an `array`-based index. It
%% should be reasonably memory efficient, but clearly not as efficient as
%% a byte array. Erlang arrays are actually nested 10-tuples under the hood.
%%
%% Some relevant links on byte arrays are:
%% - [native Erlang/OTP HIPE byte arrays](https://github.com/erlang/otp/blob/master/erts/emulator/hipe/hipe_bif0.tab)
%% - [@mononcqc's bitarray NIF-based implementation](https://github.com/ferd/bitarray)
%% - [@kivikakk's](https://kivikakk.ee/2013/05/13/k6_bytea.html) implementation as a NIF is now [hosted elsewhere](https://kivikakk.ee/2013/05/13/k6_bytea.html)
%% @end

-module(swirl_store).
-include("swirl.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% api
-export([fetch/2,       % get a specific chunk
         exists/1,      % confirm or deny presence of complete store
         exists/2,      % or specify a partial range within the store
         get_cache_config/2,
         set_cache_config/2
         %list_hashes/2,
         %fetch_hashes/2,
        ]).

-opaque store() :: term().
% cache config
% how to retrieve external chunks
% active retrievals
% on-disk storage
% in ram storage
% list of available chunks and respective hashes

-export_type([store/0]).

%% @doc fetch gets a chunk of data, from the `Store', according to the `Spec'
%% <p> fetch/2 takes a store() and a spec() and returns the desired binary().
%% if the desired spec() is out of range, or does not exist in store(), fetch/2
%% will return an error tuple.
%% </p>
-spec fetch(store(), ppspp_chunk:spec()) -> {ok, binary()} | {error, any()}.
fetch(_Store, _Spec) -> {ok, << >>}.

%% @doc confirm presence of chunks either as a range or individual chunkspecs.
%% <p> exists/1-2 provide further details and more flexible query options.
%% exists/1 only confirms if the chunkspec is full, empty, or partially full,
%% for the supplied store().
%% exists/2 takes a store() and a range(), and returns a list of available
%% chunks as indexes into the store.
%% exists/2 takes a store() and a range(), and returns a list of available
%% chunks as indexes into the store.
%% </p>
%% @end
-spec exists(store()) -> {ok, full | empty | incomplete } | {error, any()}.
exists(_Store) -> {error, swirl_store_not_implemented}.
-spec exists(store(), ppspp_chunk:spec()) -> {ok, full | empty | incomplete } | {error, any()}.
exists(_Store, _Spec) -> {error, swirl_store_not_implemented}.

-spec get_cache_config(store(), ppsp_options:option()) -> any().
get_cache_config(_Store, _Options) -> ok.

-spec set_cache_config(store(), ppsp_options:option()) -> any().
set_cache_config(_Store, _Options) -> ok.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test

% -ifdef(TEST).
% -spec _test() -> {ok, pid()}.
% _test() ->
%     start(),
%     ?assertMatch({ok, _}, ).
% -endif.
