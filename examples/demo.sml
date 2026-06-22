(* demo.sml - hash fixed inputs with BLAKE3 in plain, keyed, derive-key, and
   extended-output (XOF) modes, printing lowercase hex. Values are the official
   BLAKE3 test vectors. Deterministic: same digests on every run and compiler. *)

val () = print "BLAKE3 plain hash (official vectors):\n"
val () = print ("  hash(\"\")  = " ^ Blake3.hashHex "" ^ "\n")
val () = print ("  hash(\"abc\")= " ^ Blake3.hashHex "abc" ^ "\n")

(* Official keyed_hash key: "whats the Elvish word for friend" (32 bytes) *)
val key = "whats the Elvish word for friend"
val () = print "\nBLAKE3 keyed hash (official key):\n"
val () = print ("  keyed(\"\")  = " ^ Blake3.hashKeyedHex key "" ^ "\n")

(* Official derive_key context string *)
val ctx = "BLAKE3 2019-12-27 16:29:52 test vectors context"
val () = print "\nBLAKE3 derive_key (official context):\n"
val () = print ("  derive(\"\") = " ^ Blake3.deriveKeyHex ctx "" ^ "\n")

val () = print "\nBLAKE3 extended output (XOF, 64 bytes of empty input):\n"
val () = print ("  hashLen 64 = " ^ Blake3.hashLenHex 64 "" ^ "\n")
