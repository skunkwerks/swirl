%%%----FILE ppspp_records.hrl----

-type version() :: 1..255.
%% server type defines the role of the server.
-type server_type() :: static | live | injector. 

%% record to store the state of seeder.
-record(state, {
          %% SERVER state variables
          server_type                       :: server_type(),
          mtree                             :: atom(),
          %% SWARM options
          ppspp_swarm_id                    :: bitstring(), 
          ppspp_version                     :: version(),
          ppspp_minimum_version             :: version(),
          ppspp_chunking_method             :: byte(),
          ppspp_integrity_check_method      :: byte(),
          ppspp_merkle_hash_function        :: byte(),
          ppspp_live_signature_algorithm,
          ppspp_live_discard_window
          %% supported_messges,
         }).

%%%----END FILE----
