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

-ifndef(PPSPP_RELEASE).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PPSPP_RELEASE must only change wrt IETF RFC status
-define(PPSPP_RELEASE, "d-08").

%% useful bits and bytes
-define(QWORD, 64/big).
-define(DWORD, 32/big).
-define(WORD,  16/big).
-define(BYTE,   8/big).

%% PPSP Protocol Options -- section 7.0
-define(PPSPP_VERSION,	                0:?BYTE). %%% MUST, version 1:8
-define(PPSPP_MINIMUM_VERSION,	        1:?BYTE). %%% MUST, version 1:8
-define(PPSPP_SWARM_ID_LENGTH,          2:?BYTE). %%% MUST for initiator,
                                                  %%% MAY for peer,
                                                  %%% length:16, swarm_id:length
-define(PPSPP_INTEGRITY_CHECK_METHOD,	3:?BYTE). %%% MAY, method:8
-define(PPSPP_MERKLE_HASH_FUNCTION,	4:?BYTE). %%% MUST if Merkle tree is used
                                                  %%% function:8
-define(PPSPP_LIVE_SIGNATURE_ALGORITHM, 5:?BYTE). %%% MUST if CIPM is sign_all
                                                  %%% or unified merkle_tree
                                                  %%% algorithm:8
-define(PPSPP_CHUNK_ADDRESSING_METHOD,	6:?BYTE). %%% MAY, chunking:8
-define(PPSPP_LIVE_DISCARD_WINDOW,	7:?BYTE). %%% MUST for live swarms,
                                                  %%% window: QWORD/DWORD*
-define(PPSPP_SUPPORTED_MESSAGES,	8:?BYTE). %%% MUST unless all supported
                                                  %%% length:8, messages:length
-define(PPSPP_END_OPTION,             255:?BYTE). %%% 0. The key is sufficient
%% values used within the option parers
-define(PPSPP_DEFAULT_CHUNK_SIZE,          1024). %%%
-define(PPSPP_CURRENT_VERSION,                1). %%%

%% PPSPP Datagram Fields -- section 8.4
-define(PPSPP_CHANNEL_SIZE,      ?DWORD).
-define(PPSPP_UNKNOWN_CHANNEL, 0:?DWORD).

%% PPSPP Message Types -- section 8.2
-define(PPSPP_MESSAGE_SIZE,             ?BYTE   ).
-define(HANDSHAKE,	            << 0:?BYTE>>).
-define(DATA,   	            << 1:?BYTE>>).
-define(ACK,                        << 2:?BYTE>>).
-define(HAVE,   	            << 3:?BYTE>>).
-define(INTEGRITY,	            << 4:?BYTE>>).
-define(PEX_RESv4,	            << 5:?BYTE>>).
-define(PEX_REQ,	            << 6:?BYTE>>).
-define(SIGNED_INTEGRITY,           << 7:?BYTE>>).
-define(REQUEST,	            << 8:?BYTE>>).
-define(CANCEL,	                    << 9:?BYTE>>).
-define(CHOKE,	                    <<10:?BYTE>>).
-define(UNCHOKE,	            <<11:?BYTE>>).
-define(PEX_RESv6,	            <<12:?BYTE>>).
-define(PEX_REScert,	            <<13:?BYTE>>).
-define(PPSPP_MAXIMUM_MESSAGE_TYPE,   14).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-endif.
