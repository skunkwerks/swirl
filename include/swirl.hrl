% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-include("ppspp.hrl").
-ifndef(SWIRL_PORT).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% application macros
-define(SWIRL_PORT, 7777).
-define(SWIRL_APP, swirl).

-define(DEBUG_SWIRL(Where, What), io:format("~s ~p~n", [Where, What])).

-define(BIN2STR(Binary), helper:binary_to_string(Binary)).
-define(CHAN2STR(Binary), helper:binary_to_string( <<Binary:?PPSPP_CHANNEL_SIZE>> )).

-define(DEBUG_DGRAM_RECV(Channel), io:format("dgram: recv on channel ~s~n", [?CHAN2STR(Channel)])).
-define(DEBUG_DGRAM_PARSE_OK(Channel), io:format("dgram: parse ok on channel ~s~n", [?CHAN2STR(Channel)])).

-define(IP2STR(Host), [inet_parse:ntoa(Host)]).
-define(ENDPOINT2STR(Host,Port), lists:flatten([?IP2STR(Peer), $:, integer_to_list(Port)])).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-endif.
