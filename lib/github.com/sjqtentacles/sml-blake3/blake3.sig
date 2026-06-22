signature BLAKE3 = sig
  val hash          : string -> string
  val hashHex       : string -> string
  val hashKeyed     : string -> string -> string
  val hashKeyedHex  : string -> string -> string
  val deriveKey     : string -> string -> string
  val deriveKeyHex  : string -> string -> string
  val hashLen       : int -> string -> string
  val hashLenHex    : int -> string -> string
end
