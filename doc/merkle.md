# Merkle Hash Trees

In PPSP, by design, the integrity of any given chunk of data can be verified
efficiently using a cryptographic hash. The hashing algorithm itself is the
well-known Merkle hash tree, which can be simply described as follows:

- take a file or stream of binary data
- divide it into chunks, not necessarily equal
- for an odd number of chunks, use an all-zeros value to complete the pair
- hash each chunk, typically using SHA1
- insert the hash of each chunk into an array
- the order of the hashes matches the original chunk order
- interleave an empty space after each chunk
- for each pair of chunks, concatenate them and hash again
- store this new hash in the previously empty array slot between those chunks
- repeat this process using only the newly generated hashes

Note that the root hash represents a proof of work by the bearer, that, if
provided with an appropriate set of hashes further down the tree, can be used
to determine even from a very small number of chunks, if the provided data
is in fact a valid part of the original file / hash.

For example, given the very first chunk in a file, it will be only necessary
to provide the hash of the adjacent siblings repeatedly to the top of the
tree, to confirm that in fact the downloaded chunk is valid. In most cases
the increased packet size to accommodate the additional hashes to verify a
chunk will be very small.

In the live streaming scheme, obviously, the root hash will continually
shift along a sliding window. As the subsidiary or child hashes will
largely remain the same, only a few additional hashes (munro hashes) will
ultimately need to be re-calculated each time. By using a signed public key,
the peers can be sure that the new root hash is in fact from a legitimate
source, & untampered.

## References

Full details are of course in the PPSPP specification.

- Modern Generalised Merkle Signature Scheme, known as [GMSS]
- An [extract] from the published paper
- Ralph Merkle's original 1979 [thesis]

[extract]: https://github.com/skunkwerks/swirl/wiki/papers/Merkle_extract_from_thesis.pdf
[GMSS]: https://github.com/skunkwerks/swirl/wiki/papers/Merkle_Signatures_with_Virtually_Unlimited_Signature_Capacity.pdf
[thesis]: https://github.com/skunkwerks/swirl/wiki/papers/Secrecy_Authentication_and_Public_Key_Systems_Merkle1979.pdf