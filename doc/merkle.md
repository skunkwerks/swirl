# Merkle Hash Trees

In PPSP, by design, the integrity of any given chunk of data can be verified
efficiently using a cryptographic signature scheme. The hashing algorithm
itself is the well-known Merkle hash tree:

![merkle hash tree](https://github.com/skunkwerks/swirl/wiki/images/merkletree-v4.png)

By transmitting with each new datagram, a set of new chunk hashes, and the new data,
a peer can verify using any already downloaded / hashed data, and the new data, if in
fact, is valid (untampered with during transit) and part of the desired data, by comparing all the way up to the root hash, which was obtained outside the PPSP protocol, from a trusted endpoint.

This is a significant advantage over BitTorrent, which requires downloading in advance
all required hash data, and in addition, forces seeding peers to maintain a full list
of hashes for every chunk. PPSPP only needs to store sibling and uncle hashes for select
nodes within the tree, and can still maintain cryptographically secure chunk distribution.

## Algorithm Overview

- take a file or stream of binary data
- split the file into chunks, not necessarily of the same size, but usually the case
- for an odd number of chunks, use an all-zeros value to complete the pair
- generate a hash digest, e.g. SHA1, of each chunk
- arrange these in the same order as the content in the file
- interleave an empty space after each chunk hash, for subsequent hashes
- optionally, these can be inserted into an array using the even indexes
- concatenate each pair of chunk hashes, and hash the resulting data again with SHA1
- move "up" a row, or level, in the tree-to-be
- insert the new hash digest in the spare slot between the two hashed chunks
- repeat across the whole file
- between each pair of original chunks, there will still be a spare slot left
- move "up" a level in the tree again
- this time, hash the hash digests from the second layer, not the bottom layer
- repeat this process using only the newly generated hashes, across the whole file
- finally, repeat this entire process until there is a single hash digest of the entire file

## Verification

Note that the root hash represents a significant proof of work by the bearer,
that, if provided with an appropriate set of hashes further down the tree,
can be used to determine even from a very small number of chunks, if the
provided data is in fact a valid part of the original file / hash.

For example, given the very first chunk in a file, it will be only necessary
to provide the hash of the adjacent siblings repeatedly to the top of the
tree, to confirm that in fact the downloaded chunk is valid. In most cases
the increased packet size to accommodate the additional hashes to verify a
chunk will be very small.

## Live Streaming

In the live streaming scheme, obviously, the root hash will continually
shift along a sliding window. As the subsidiary or child hashes will
largely remain the same, only a few additional hashes (munro hashes) will
ultimately need to be re-calculated each time. By using a signed public key,
the peers can be sure that the new root hash is in fact from a legitimate
source, & untampered.

## References

Full details are of course in the [PPSPP] specification.

- Modern Generalised Merkle Signature Scheme, known as [GMSS]
- An [extract] from the published paper
- [Ralph Merkle]'s original 1979 [thesis]
- the [SHA1] specification, based on the original NIST Standard

[PPSPP]: http://tools.ietf.org/html/draft-ietf-ppsp-peer-protocol
[extract]: https://github.com/skunkwerks/swirl/wiki/papers/Merkle_extract_from_thesis.pdf
[GMSS]: https://github.com/skunkwerks/swirl/wiki/papers/Merkle_Signatures_with_Virtually_Unlimited_Signature_Capacity.pdf
[thesis]: https://github.com/skunkwerks/swirl/wiki/papers/Secrecy_Authentication_and_Public_Key_Systems_Merkle1979.pdf
[SHA1]: http://tools.ietf.org/html/rfc3174
[Ralph Merkle]: http://www.merkle.com/merkleDir/papers.html
