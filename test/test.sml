structure Tests =
struct

  fun makeInput (len : int) : string =
    String.implode (List.tabulate (len, fn i => Char.chr (i mod 251)))

  fun hexChars s =
    String.implode (List.concat (List.map (fn c =>
      let val v = Char.ord c
      in [ String.sub ("0123456789abcdef", v div 16)
         , String.sub ("0123456789abcdef", v mod 16) ]
      end) (String.explode s)))

  fun run () : bool =
    let
      val _ = Harness.reset ()

      val _ = Harness.section "BLAKE3 hash - basic"

      (* Official empty string test vector *)
      val emptyHex = Blake3.hashHex ""
      val _ = Harness.checkString "hash empty string (hex)"
        ("af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262",
         emptyHex)

      val emptyBytes = Blake3.hash ""
      val _ = Harness.checkInt "hash empty returns 32 bytes"
        (32, String.size emptyBytes)

      val _ = Harness.checkInt "hashHex empty returns 64 chars"
        (64, String.size emptyHex)

      val hexA = Blake3.hashHex "a"
      val _ = Harness.checkInt "hashHex 'a' returns 64 chars" (64, String.size hexA)

      val hexAbc = Blake3.hashHex "abc"
      val _ = Harness.checkInt "hashHex 'abc' returns 64 chars" (64, String.size hexAbc)

      val _ = Harness.checkString "hash deterministic (empty)"
        (Blake3.hashHex "", Blake3.hashHex "")
      val _ = Harness.checkString "hash deterministic ('hello')"
        (Blake3.hashHex "hello", Blake3.hashHex "hello")
      val _ = Harness.check "hash 'a' <> hash 'b'"
        (Blake3.hashHex "a" <> Blake3.hashHex "b")
      val _ = Harness.check "hash 'abc' <> hash empty"
        (Blake3.hashHex "abc" <> Blake3.hashHex "")

      val _ = Harness.section "BLAKE3 hash - official vectors"

      (* Official test vectors: input[i] = i mod 251 *)

      (* 1-byte: official vector *)
      val _ = Harness.checkString "hash 1 byte (official)"
        ("2d3adedff11b61f14c886e35afa036736dcd87a74d27b5c1510225d0f592e213",
         Blake3.hashHex (makeInput 1))

      (* 63-byte vector *)
      val _ = Harness.checkString "hash 63 bytes (official)"
        ("e9bc37a594daad83be9470df7f7b3798297c3d834ce80ba85d6e207627b7db7b",
         Blake3.hashHex (makeInput 63))

      (* 64-byte vector: exactly 1 block *)
      val _ = Harness.checkString "hash 64 bytes (official)"
        ("4eed7141ea4a5cd4b788606bd23f46e212af9cacebacdc7d1f4c6dc7f2511b98",
         Blake3.hashHex (makeInput 64))

      (* 65-byte vector *)
      val _ = Harness.checkString "hash 65 bytes (official)"
        ("de1e5fa0be70df6d2be8fffd0e99ceaa8eb6e8c93a63f2d8d1c30ecb6b263dee",
         Blake3.hashHex (makeInput 65))

      (* 1023-byte vector *)
      val h1023 = Blake3.hashHex (makeInput 1023)
      val _ = Harness.checkString "hash 1023 bytes (official)"
        ("10108970eeda3eb932baac1428c7a2163b0e924c9a9e25b35bba72b28f70bd11",
         h1023)

      (* 1024-byte vector: exactly one full chunk *)
      val h1024 = Blake3.hashHex (makeInput 1024)
      val _ = Harness.checkString "hash 1024 bytes (official)"
        ("42214739f095a406f3fc83deb889744ac00df831c10daa55189b5d121c855af7",
         h1024)

      (* 1025-byte vector: just over one chunk *)
      val h1025 = Blake3.hashHex (makeInput 1025)
      val _ = Harness.checkString "hash 1025 bytes (official)"
        ("d00278ae47eb27b34faecf67b4fe263f82d5412916c1ffd97c8cb7fb814b8444",
         h1025)

      (* 1024 <> 1025 *)
      val _ = Harness.check "hash 1024 <> hash 1025" (h1024 <> h1025)

      val _ = Harness.section "BLAKE3 hashLen"

      val h16 = Blake3.hashLen 16 ""
      val _ = Harness.checkInt "hashLen 16 empty" (16, String.size h16)
      val h64 = Blake3.hashLen 64 ""
      val _ = Harness.checkInt "hashLen 64 empty" (64, String.size h64)
      val h1  = Blake3.hashLen 1 ""
      val _ = Harness.checkInt "hashLen 1 empty" (1, String.size h1)

      val _ = Harness.checkString "hashLen 32 == hash for empty"
        (Blake3.hash "", Blake3.hashLen 32 "")
      val _ = Harness.checkString "hashLen 32 == hash for 'abc'"
        (Blake3.hash "abc", Blake3.hashLen 32 "abc")

      (* First 32 bytes of hashLen 64 equal hash 32 *)
      val _ = Harness.checkString "hashLen 64 first 32 bytes == hash 32 (empty)"
        (String.substring (Blake3.hashLen 64 "", 0, 32), Blake3.hash "")

      val _ = Harness.section "BLAKE3 hashKeyed"

      (* Official keyed_hash vectors use key = "whats the Elvish word for friend" (32 bytes) *)
      val officialKey = "whats the Elvish word for friend"
      val _ = Harness.checkString "keyed_hash empty (official)"
        ("92b2b75604ed3c761f9d6f62392c8a9227ad0ea3f09573e783f1498a4ed60d26",
         hexChars (Blake3.hashKeyed officialKey ""))

      val _ = Harness.checkString "keyed_hash 1 byte (official)"
        ("6d7878dfff2f485635d39013278ae14f1454b8c0a3a2d34bc1ab38228a80c95b",
         hexChars (Blake3.hashKeyed officialKey (makeInput 1)))

      val zeroKey = String.implode (List.tabulate (32, fn _ => Char.chr 0))
      val hk0 = Blake3.hashKeyed zeroKey ""
      val _ = Harness.checkInt "hashKeyed(zeroKey, empty) returns 32 bytes" (32, String.size hk0)
      val _ = Harness.checkString "hashKeyed deterministic"
        (Blake3.hashKeyed zeroKey "hello", Blake3.hashKeyed zeroKey "hello")
      val _ = Harness.check "hashKeyed != plain hash"
        (Blake3.hash "" <> Blake3.hashKeyed zeroKey "")
      val _ = Harness.check "hashKeyed != keyed with diff key"
        (Blake3.hashKeyed officialKey "" <> Blake3.hashKeyed zeroKey "")

      val _ = Harness.section "BLAKE3 deriveKey"

      (* Official derive_key context *)
      val ctx = "BLAKE3 2019-12-27 16:29:52 test vectors context"
      val _ = Harness.checkString "deriveKey empty (official)"
        ("2cc39783c223154fea8dfb7c1b1660f2ac2dcbd1c1de8277b0b0dd39b7e50d7d",
         hexChars (Blake3.deriveKey ctx ""))

      val _ = Harness.checkString "deriveKey 1 byte (official)"
        ("b3e2e340a117a499c6cf2398a19ee0d29cca2bb7404c73063382693bf66cb06c",
         hexChars (Blake3.deriveKey ctx (makeInput 1)))

      val dk0 = Blake3.deriveKey ctx ""
      val _ = Harness.checkInt "deriveKey empty returns 32 bytes" (32, String.size dk0)
      val _ = Harness.checkString "deriveKey deterministic"
        (Blake3.deriveKey ctx "hello", Blake3.deriveKey ctx "hello")
      val _ = Harness.check "deriveKey != plain hash"
        (Blake3.hash "" <> Blake3.deriveKey ctx "")
      val _ = Harness.check "deriveKey different context -> different output"
        (Blake3.deriveKey "ctx1" "msg" <> Blake3.deriveKey "ctx2" "msg")

      val _ = Harness.section "BLAKE3 *Hex siblings"

      (* hashKeyedHex matches the official keyed_hash hex vectors directly *)
      val _ = Harness.checkString "hashKeyedHex empty (official)"
        ("92b2b75604ed3c761f9d6f62392c8a9227ad0ea3f09573e783f1498a4ed60d26",
         Blake3.hashKeyedHex officialKey "")
      val _ = Harness.checkString "hashKeyedHex 1 byte (official)"
        ("6d7878dfff2f485635d39013278ae14f1454b8c0a3a2d34bc1ab38228a80c95b",
         Blake3.hashKeyedHex officialKey (makeInput 1))
      (* hashKeyedHex == hex of hashKeyed bytes *)
      val _ = Harness.checkString "hashKeyedHex == hex(hashKeyed)"
        (hexChars (Blake3.hashKeyed officialKey "abc"),
         Blake3.hashKeyedHex officialKey "abc")

      (* deriveKeyHex matches the official derive_key hex vectors directly *)
      val _ = Harness.checkString "deriveKeyHex empty (official)"
        ("2cc39783c223154fea8dfb7c1b1660f2ac2dcbd1c1de8277b0b0dd39b7e50d7d",
         Blake3.deriveKeyHex ctx "")
      val _ = Harness.checkString "deriveKeyHex 1 byte (official)"
        ("b3e2e340a117a499c6cf2398a19ee0d29cca2bb7404c73063382693bf66cb06c",
         Blake3.deriveKeyHex ctx (makeInput 1))
      val _ = Harness.checkString "deriveKeyHex == hex(deriveKey)"
        (hexChars (Blake3.deriveKey ctx "abc"), Blake3.deriveKeyHex ctx "abc")

      (* hashLenHex == hex of hashLen bytes; and hashLenHex 32 == hashHex *)
      val _ = Harness.checkString "hashLenHex 32 empty == hashHex empty"
        (Blake3.hashHex "", Blake3.hashLenHex 32 "")
      val _ = Harness.checkString "hashLenHex == hex(hashLen)"
        (hexChars (Blake3.hashLen 16 "abc"), Blake3.hashLenHex 16 "abc")
      val _ = Harness.checkInt "hashLenHex 16 length = 32 chars"
        (32, String.size (Blake3.hashLenHex 16 ""))
    in
      Harness.run ()
    end

end
