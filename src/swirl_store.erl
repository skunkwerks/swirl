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

%% @doc Swirl Storage API
%%
%% This module provides an API to a generic indexed key value store.
%%
%% <div>
%% ## Nota Bene
%% You can of course embed [markdown] for hugo to deal with, via divs.
%% [markdown]: http://daringfireball.net/projects/markdown/syntax
%% </div>
%% And code samples just as easily:
%% <pre lang="erlang">
%% incr(X) ->
%%   %% This should be formatted with Erlang syntax highlighting
%%   X + 1.
%% </pre>
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
         list_hashes/2,
         fetch_hashes/2,
         get_cache_config/1,
         set_cache_config/2
        ]).

-opaque store() :: term().
% cache config
% how to retrieve external chunks
% active retrievals
% on-disk storage
% in ram storage
% list of available chunks and respective hashes

-export_type([store/0]).

%% @doc confirm presence of chunks either as a range or individual chunkspecs.
%% <p> exists/1-3 provide further details and more flexible query options.
%% exists/1 only confirms if the chunkspec is full, empty, or partially full,
%% for the supplied store().
%% exists/2 takes a store() and a range(), and returns a list of available
%% chunks as indexes into the store.
%% exists/2 takes a store() and a range(), and returns a list of available
%% chunks as indexes into the store.
%% </p>
%% @end
-spec fetch(store(), ppspp_chunk:spec()) -> binary().
-spec exists(store()) -> {ok, full | empty | incomplete } | {error, any()}.
-spec exists(store(), ppspp_chunk:spec()) -> {ok, full | empty | incomplete } | {error, any()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test

% -ifdef(TEST).
% -spec _test() -> {ok, pid()}.
% _test() ->
%     start(),
%     ?assertMatch({ok, _}, ).
% -endif.
