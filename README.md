# sml-blake3

BLAKE3 cryptographic hash function in pure Standard ML

## Installation

```
smlpkg add github.com/sjqtentacles/sml-blake3
smlpkg sync
```

## Usage

```sml
(* Hash a message — returns 32 bytes by default *)
val digest = Blake3.hash "hello, world"
(* String.size digest = 32 *)

(* Hex-encoded hash *)
val hex = Blake3.hashHex "hello, world"
(* "e4e5327..." — 64 hex chars *)

(* Official empty-string test vector *)
val emptyHex = Blake3.hashHex ""
(* "af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262" *)

(* Keyed hash (MAC) with a 32-byte key *)
val mac = Blake3.keyedHash key "message"

(* Key derivation function *)
val derived = Blake3.deriveKey "context string" "key material"

(* Hex siblings of the byte-returning functions (lowercase hex). *)
val macHex = Blake3.hashKeyedHex key "message"
val dkHex  = Blake3.deriveKeyHex "context string" "key material"
val xofHex = Blake3.hashLenHex 64 "data"   (* extended-output hash, hex *)
```

## Example

`make example` builds and runs [`examples/demo.sml`](examples/demo.sml), which
hashes fixed inputs with BLAKE3 in plain, keyed, derive-key, and extended-output
(XOF) modes using the official test vectors, printing lowercase hex:

```
$ make example
BLAKE3 plain hash (official vectors):
  hash("")  = af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262
  hash("abc")= 6437b3ac38465133ffb63b75273a8db548c558465d79db03fd359c6cd5bd9d85

BLAKE3 keyed hash (official key):
  keyed("")  = 92b2b75604ed3c761f9d6f62392c8a9227ad0ea3f09573e783f1498a4ed60d26

BLAKE3 derive_key (official context):
  derive("") = 2cc39783c223154fea8dfb7c1b1660f2ac2dcbd1c1de8277b0b0dd39b7e50d7d

BLAKE3 extended output (XOF, 64 bytes of empty input):
  hashLen 64 = af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262e00f03e7b69af26b7faaf09fcd333050338ddfe085b8cc869ca98b206c08243a
```

## Testing

```
make test       # MLton
make test-poly  # Poly/ML
make example    # build + run the demo
```

## License

MIT
