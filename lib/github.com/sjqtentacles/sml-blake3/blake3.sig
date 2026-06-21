signature BLAKE3 = sig
  val hash       : string -> string
  val hashHex    : string -> string
  val hashKeyed  : string -> string -> string
  val deriveKey  : string -> string -> string
  val hashLen    : int -> string -> string
end
