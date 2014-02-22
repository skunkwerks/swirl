# Swirl Hierarchy

Swirl the Erlang/OTP application comprises a number of separate modules:

- `swirl_app` the top level container for the entire swirl OTP application
- `peer_sup` and `swarm_sup`, supervisors that manage pools of peer and swarm workers respectively

<pre>


                            +----------------------------------+
                            |         swirl_app                |
                            |----------------------------------|
                            |      two child supervisors       |
                            |       - for peers                |
                            |       - for swarms               |
                            |                                  |
                            +---------------+------------------+
                                            |
                                            |
                      +---------------------+------------------------+
                      |                                              |
                      v                                              v
       +------------------------------+             +------------------------------+
       |       peer_sup               |             |      swarm_sup               |
       |------------------------------|             |------------------------------|
       |    manages udp listener      |             |   manages swarms             |
       |     - sends packets to       |             |      - responds to messages  |
       |       the correct swarm      |             |        from listener         |
       |                              |             |                              |
       +---------------+--------------+             +------------+-----------------+
                       |                                         |
                       |                                         +-------------+-----+
               +-------+------+----+                             |             |     |
               |              |    |                             v             |     |
               v              |    |                    +--------------------+ |     |
      +--------------------+  |    |                    |  swarm_worker      | v     |
      |   peer_worker      |  v    |                    |--------------------|+---+  |
      |--------------------|+---+  |                    |- receives parsed   |    |  v
      |- receives udp dgram|    |  v                    |  dgram/messages    |    |+---+
      |- routes to correct |    |+---+                  |- replies via peer  |    |    |
      |  swarm             |    |    | +------------+   |- maintains swarm   |    |    |
      |- sends dgrams back |    |    |                  |  metadata &amp; chunks |    |    |
      |- manages bad peers |    |    |                  +---+----------------+    |    |
      +---+----------------+    |    | +--------------&gt;     |                     |    |
          |                     |    |                      +---------------------+    |
          +---------------------+    |                       +   |                     |
               |                     |  &lt;--------------------+   +---------------------+
               +---------------------+                                +
                                                                      |
                                &lt;-------------------------------------+

</pre>

## peer_sup

The Peer supervisor manages a dynamic set of PPSPP transport listeners and ensures that
all required transport addresses (UDP port bound to a specific IP address) are active within a given `peer_worker`. While currently each peer_worker manages a single IP/port, it is straightforwards to run multiple peer_workers sharing the same socket for increased throughput, and indeed multiple peer_workers across different IP & port ranges.

## peer_worker

The peer worker is a simple `gen_server`-based loop bound to a specific transport address. It has minimal state and has not special start or stop requirements. It receives inbound UDP packets, determines the correct PPSPP swarm & channel, and spawns a process to parse and route the parsed datagram to the correct `swarm_worker` via its requested `channel`.

## swarm_sup

The swarm supervisor manages a dynamic set of PPSPP swarms, effectively a set of workers that maintain an individual swarm (unique root hash and therefore also chunk type, size & related metadata).

## swarm_worker

The swarm worker is a simple `gen_server`-based loop bound to a specific transport address. It has minimal state and has not special start or stop requirements. It receives inbound UDP packets, determines the correct PPSPP swarm & channel, and spawns a process to parse and route the parsed datagram to the correct `swarm_worker` via its requested `channel`.
