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

## Testing

```
make test       # MLton
make test-poly  # Poly/ML
```

## License

MIT
