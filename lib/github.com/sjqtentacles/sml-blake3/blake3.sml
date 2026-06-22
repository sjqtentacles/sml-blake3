structure Blake3 :> BLAKE3 =
struct

  (* BLAKE3 IV constants *)
  val iv0 = 0wx6A09E667 : Word32.word
  val iv1 = 0wxBB67AE85 : Word32.word
  val iv2 = 0wx3C6EF372 : Word32.word
  val iv3 = 0wxA54FF53A : Word32.word
  val iv4 = 0wx510E527F : Word32.word
  val iv5 = 0wx9B05688C : Word32.word
  val iv6 = 0wx1F83D9AB : Word32.word
  val iv7 = 0wx5BE0CD19 : Word32.word

  val ivVec = Vector.fromList [iv0, iv1, iv2, iv3, iv4, iv5, iv6, iv7]

  (* Domain separation flags *)
  val flag_CHUNK_START         = 0wx00000001 : Word32.word
  val flag_CHUNK_END           = 0wx00000002 : Word32.word
  val flag_PARENT              = 0wx00000004 : Word32.word
  val flag_ROOT                = 0wx00000008 : Word32.word
  val flag_KEYED_HASH          = 0wx00000010 : Word32.word
  val flag_DERIVE_KEY_CONTEXT  = 0wx00000020 : Word32.word
  val flag_DERIVE_KEY_MATERIAL = 0wx00000040 : Word32.word

  val blockSize = 64
  val chunkSize = 1024
  val outSize   = 32

  fun rotr32 (w : Word32.word, n : int) : Word32.word =
    Word32.orb (Word32.>> (w, Word.fromInt n),
                Word32.<< (w, Word.fromInt (32 - n)))

  (* The single BLAKE3 permutation vector *)
  val msgPerm = Vector.fromList [2,6,3,10,7,0,4,13,1,11,12,5,9,14,15,8]

  (* Apply the permutation to a 16-element array in-place *)
  fun permute (blk : Word32.word array) : unit =
    let val tmp = Array.tabulate (16, fn i => Array.sub (blk, Vector.sub (msgPerm, i)))
    in Array.copyVec { src = Array.vector tmp, dst = blk, di = 0 }
    end

  (* G mixing function *)
  fun g (st : Word32.word array, a : int, b : int, c : int, d : int,
         mx : Word32.word, my : Word32.word) : unit =
    let
      val va  = Array.sub (st, a)
      val vb  = Array.sub (st, b)
      val vc  = Array.sub (st, c)
      val vd  = Array.sub (st, d)
      val va2 = Word32.+ (Word32.+ (va, vb), mx)
      val vd2 = rotr32 (Word32.xorb (vd, va2), 16)
      val vc2 = Word32.+ (vc, vd2)
      val vb2 = rotr32 (Word32.xorb (vb, vc2), 12)
      val va3 = Word32.+ (Word32.+ (va2, vb2), my)
      val vd3 = rotr32 (Word32.xorb (vd2, va3), 8)
      val vc3 = Word32.+ (vc2, vd3)
      val vb3 = rotr32 (Word32.xorb (vb2, vc3), 7)
    in
      Array.update (st, a, va3);
      Array.update (st, b, vb3);
      Array.update (st, c, vc3);
      Array.update (st, d, vd3)
    end

  fun round (st : Word32.word array, m : Word32.word array) : unit =
    ( g (st, 0, 4,  8, 12, Array.sub (m, 0),  Array.sub (m, 1))
    ; g (st, 1, 5,  9, 13, Array.sub (m, 2),  Array.sub (m, 3))
    ; g (st, 2, 6, 10, 14, Array.sub (m, 4),  Array.sub (m, 5))
    ; g (st, 3, 7, 11, 15, Array.sub (m, 6),  Array.sub (m, 7))
    ; g (st, 0, 5, 10, 15, Array.sub (m, 8),  Array.sub (m, 9))
    ; g (st, 1, 6, 11, 12, Array.sub (m, 10), Array.sub (m, 11))
    ; g (st, 2, 7,  8, 13, Array.sub (m, 12), Array.sub (m, 13))
    ; g (st, 3, 4,  9, 14, Array.sub (m, 14), Array.sub (m, 15))
    )

  (* Core compress: returns 16-word output array.
     The output is:
       out[i]   = state[i] XOR state[i+8]   for i in 0..7
       out[i+8] = state[i+8] XOR cv[i]      for i in 0..7
  *)
  fun compress (cv       : Word32.word vector,
                msgWords : Word32.word array,
                counter  : Word64.word,
                blockLen : Word32.word,
                flags    : Word32.word)
               : Word32.word array =
    let
      val counterLo = Word32.fromLarge (Word64.toLarge counter)
      val counterHi = Word32.fromLarge (Word64.toLarge (Word64.>> (counter, 0w32)))
      val state = Array.fromList
        [ Vector.sub (cv, 0), Vector.sub (cv, 1)
        , Vector.sub (cv, 2), Vector.sub (cv, 3)
        , Vector.sub (cv, 4), Vector.sub (cv, 5)
        , Vector.sub (cv, 6), Vector.sub (cv, 7)
        , iv0, iv1, iv2, iv3
        , counterLo, counterHi, blockLen, flags
        ]
      (* Copy of block words that will be permuted *)
      val blk = Array.tabulate (16, fn i => Array.sub (msgWords, i))
      val _ = round (state, blk)
      val _ = permute blk
      val _ = round (state, blk)
      val _ = permute blk
      val _ = round (state, blk)
      val _ = permute blk
      val _ = round (state, blk)
      val _ = permute blk
      val _ = round (state, blk)
      val _ = permute blk
      val _ = round (state, blk)
      val _ = permute blk
      val _ = round (state, blk)
      (* XOR as per reference: state[i] ^= state[i+8]; state[i+8] ^= cv[i] *)
      val _ = Array.modifyi (fn (i, _) =>
        if i < 8
        then Word32.xorb (Array.sub (state, i), Array.sub (state, i + 8))
        else Word32.xorb (Array.sub (state, i), Vector.sub (cv, i - 8))) state
    in state end

  (* Compress and return first 8 words as chaining value *)
  fun compressCV (cv       : Word32.word vector,
                  msgWords : Word32.word array,
                  counter  : Word64.word,
                  blockLen : Word32.word,
                  flags    : Word32.word)
                 : Word32.word vector =
    let val out = compress (cv, msgWords, counter, blockLen, flags)
    in Vector.tabulate (8, fn i => Array.sub (out, i)) end

  (* Parse a 64-byte block from string (zero-pad beyond slen) *)
  fun parseBlock (s : string, off : int, slen : int) : Word32.word array =
    let fun b i =
          let val idx = off + i
          in if idx < slen then Word32.fromInt (Char.ord (String.sub (s, idx)))
             else 0w0
          end
        fun readLE32 wi =
          Word32.orb (Word32.orb (Word32.orb (b (wi * 4),
            Word32.<< (b (wi * 4 + 1), 0w8)),
            Word32.<< (b (wi * 4 + 2), 0w16)),
            Word32.<< (b (wi * 4 + 3), 0w24))
    in Array.tabulate (16, readLE32)
    end

  (* Read a little-endian Word32 from a string at offset off *)
  fun readLE32 (s : string, off : int) : Word32.word =
    let fun b i = Word32.fromInt (Char.ord (String.sub (s, off + i)))
    in Word32.orb (Word32.orb (Word32.orb (b 0,
         Word32.<< (b 1, 0w8)),
         Word32.<< (b 2, 0w16)),
         Word32.<< (b 3, 0w24))
    end

  (* An Output captures state just before choosing chaining_value vs root_output *)
  datatype Output = Output of
    { inputCV  : Word32.word vector
    , blockWds : Word32.word array
    , counter  : Word64.word
    , blockLen : Word32.word
    , flags    : Word32.word
    }

  fun outputCV (Output { inputCV, blockWds, counter, blockLen, flags }) : Word32.word vector =
    compressCV (inputCV, blockWds, counter, blockLen, flags)

  fun rootOutputBytes (Output { inputCV, blockWds, counter = _, blockLen, flags }, outLen : int)
                      : Word8Array.array =
    let
      val rootFlags = Word32.orb (flags, flag_ROOT)
      val outBuf    = Word8Array.array (outLen, 0w0 : Word8.word)
      fun writeBlock (blockIdx : int) : unit =
        if blockIdx * 64 >= outLen then ()
        else
          let
            val out16  = compress (inputCV, blockWds, Word64.fromInt blockIdx,
                                    blockLen, rootFlags)
            val startB = blockIdx * 64
            val mask   = 0wxFF : Word32.word
          in
            Array.appi (fn (wi, w) =>
              let val bo = startB + wi * 4
              in if bo >= outLen then ()
                 else (
                   if bo     < outLen then Word8Array.update (outBuf, bo,     Word8.fromInt (Word32.toInt (Word32.andb (w, mask)))) else ();
                   if bo + 1 < outLen then Word8Array.update (outBuf, bo + 1, Word8.fromInt (Word32.toInt (Word32.andb (Word32.>> (w, 0w8),  mask)))) else ();
                   if bo + 2 < outLen then Word8Array.update (outBuf, bo + 2, Word8.fromInt (Word32.toInt (Word32.andb (Word32.>> (w, 0w16), mask)))) else ();
                   if bo + 3 < outLen then Word8Array.update (outBuf, bo + 3, Word8.fromInt (Word32.toInt (Word32.andb (Word32.>> (w, 0w24), mask)))) else ()
                 )
              end) out16;
            writeBlock (blockIdx + 1)
          end
    in writeBlock 0; outBuf
    end

  fun parentOutput (leftCV  : Word32.word vector,
                    rightCV : Word32.word vector,
                    kw      : Word32.word vector,
                    flags   : Word32.word) : Output =
    let val bw = Array.fromList
          [ Vector.sub (leftCV,  0), Vector.sub (leftCV,  1)
          , Vector.sub (leftCV,  2), Vector.sub (leftCV,  3)
          , Vector.sub (leftCV,  4), Vector.sub (leftCV,  5)
          , Vector.sub (leftCV,  6), Vector.sub (leftCV,  7)
          , Vector.sub (rightCV, 0), Vector.sub (rightCV, 1)
          , Vector.sub (rightCV, 2), Vector.sub (rightCV, 3)
          , Vector.sub (rightCV, 4), Vector.sub (rightCV, 5)
          , Vector.sub (rightCV, 6), Vector.sub (rightCV, 7)
          ]
    in Output { inputCV  = kw
               , blockWds = bw
               , counter  = 0w0
               , blockLen = Word32.fromInt 64
               , flags    = Word32.orb (flag_PARENT, flags)
               }
    end

  fun parentCV (leftCV : Word32.word vector, rightCV : Word32.word vector,
                kw : Word32.word vector, flags : Word32.word) : Word32.word vector =
    outputCV (parentOutput (leftCV, rightCV, kw, flags))

  (* Process a single chunk. Returns the Output struct for the last block. *)
  fun chunkOutput (msg : string, off : int, slen : int,
                   chunkCounter : Word64.word,
                   kw : Word32.word vector,
                   baseFlags : Word32.word) : Output =
    let
      val chunkEnd   = Int.min (off + chunkSize, slen)
      val chunkBytes = chunkEnd - off
      val numBlocks  = if chunkBytes = 0 then 1
                       else (chunkBytes + blockSize - 1) div blockSize

      fun loop (blkIdx : int, cv : Word32.word vector) : Output =
        let
          val bOff    = off + blkIdx * blockSize
          val bEnd    = Int.min (bOff + blockSize, chunkEnd)
          val bLen    = bEnd - bOff
          val isFirst = (blkIdx = 0)
          val isLast  = (blkIdx = numBlocks - 1)
          val flags   = Word32.orb (baseFlags,
                        Word32.orb (if isFirst then flag_CHUNK_START else 0w0,
                                    if isLast  then flag_CHUNK_END   else 0w0))
          val mw = parseBlock (msg, bOff, slen)
        in
          if isLast then
            Output { inputCV = cv, blockWds = mw, counter = chunkCounter,
                     blockLen = Word32.fromInt bLen, flags = flags }
          else
            let val newCV = compressCV (cv, mw, chunkCounter,
                                         Word32.fromInt bLen, flags)
            in loop (blkIdx + 1, newCV)
            end
        end
    in loop (0, kw)
    end

  (* The stack holds CVs of completed subtrees, with the bottom of the stack
     being the leftmost completed subtree. *)
  fun addChunkCV (stack    : Word32.word vector list ref,
                  newCV    : Word32.word vector,
                  totalCks : Word64.word,
                  kw       : Word32.word vector,
                  flags    : Word32.word) : unit =
    let
      fun mergeLoop (cv : Word32.word vector, n : Word64.word) : Word32.word vector =
        if Word64.andb (n, 0w1) = 0w0 then
          let val left = hd (!stack)
              val _    = stack := tl (!stack)
          in mergeLoop (parentCV (left, cv, kw, flags), Word64.>> (n, 0w1))
          end
        else cv
      val finalCV = mergeLoop (newCV, totalCks)
    in stack := finalCV :: (!stack)
    end

  fun hashInternal (msg : string, kw : Word32.word vector,
                    flags : Word32.word, outLen : int) : Word8Array.array =
    let
      val slen      = String.size msg
      val numChunks = if slen = 0 then 1
                      else (slen + chunkSize - 1) div chunkSize
      val stack     = ref ([] : Word32.word vector list)

      (* Process each chunk, pushing its CV onto the stack *)
      fun loopChunks (idx : int) : unit =
        if idx >= numChunks then ()
        else
          let
            val off   = idx * chunkSize
            val outO  = chunkOutput (msg, off, slen, Word64.fromInt idx, kw, flags)
            val cv    = outputCV outO
            val total = Word64.fromInt (idx + 1)
          in
            addChunkCV (stack, cv, total, kw, flags);
            loopChunks (idx + 1)
          end

      val _ = loopChunks 0

      (* Now finalize: rebuild the Output starting from the last chunk,
         then wrap in parent nodes for each remaining stack entry. *)
      val lastChunkIdx = numChunks - 1
      val rootOutput0  = chunkOutput (msg, lastChunkIdx * chunkSize, slen,
                                       Word64.fromInt lastChunkIdx, kw, flags)

      (* The last chunk's CV is on the stack; we need the Output, not the CV.
         But after addChunkCV merged, the stack may have a different CV than
         what chunkOutput returned directly. We must redo finalize properly:
         pop the stack items below the last chunk and build parent outputs. *)

      (* Reset: recompute stack without the last chunk merged in *)
      val stack2 = ref ([] : Word32.word vector list)
      val _ = if numChunks = 1 then ()
              else
                let fun loop2 (idx : int) : unit =
                      if idx >= numChunks - 1 then ()
                      else
                        let val off = idx * chunkSize
                            val out = chunkOutput (msg, off, slen, Word64.fromInt idx, kw, flags)
                            val cv  = outputCV out
                            val tot = Word64.fromInt (idx + 1)
                        in addChunkCV (stack2, cv, tot, kw, flags);
                           loop2 (idx + 1)
                        end
                in loop2 0
                end

      (* Now build the root output, wrapping parent nodes for each remaining stack item *)
      fun buildRoot (outO : Output,
                     stk  : Word32.word vector list) : Output =
        case stk of
          []       => outO
        | leftCV :: rest =>
            buildRoot (parentOutput (leftCV, outputCV outO, kw, flags), rest)

      val rootStk = List.rev (!stack2)
      val rootOut = buildRoot (rootOutput0, rootStk)
    in
      rootOutputBytes (rootOut, outLen)
    end

  fun word8ArrayToString (arr : Word8Array.array) : string =
    String.implode (List.tabulate (Word8Array.length arr,
      fn i => Char.chr (Word8.toInt (Word8Array.sub (arr, i)))))

  val hexChars = "0123456789abcdef"

  fun toHex (s : string) : string =
    String.implode (List.concat (List.map (fn c =>
      let val v = Char.ord c
      in [ String.sub (hexChars, v div 16)
         , String.sub (hexChars, v mod 16) ]
      end) (String.explode s)))

  fun bytesToKeyWords (key : string) : Word32.word vector =
    Vector.tabulate (8, fn i => readLE32 (key, i * 4))

  fun hash (msg : string) : string =
    word8ArrayToString (hashInternal (msg, ivVec, 0w0, outSize))

  fun hashHex (msg : string) : string =
    toHex (hash msg)

  fun hashKeyed (key : string) (msg : string) : string =
    let val kw = bytesToKeyWords key
    in word8ArrayToString (hashInternal (msg, kw, flag_KEYED_HASH, outSize))
    end

  fun hashKeyedHex (key : string) (msg : string) : string =
    toHex (hashKeyed key msg)

  fun deriveKey (context : string) (material : string) : string =
    let
      val ctxKeyArr = hashInternal (context, ivVec, flag_DERIVE_KEY_CONTEXT, outSize)
      val ckWords   = bytesToKeyWords (word8ArrayToString ctxKeyArr)
    in word8ArrayToString (hashInternal (material, ckWords, flag_DERIVE_KEY_MATERIAL, outSize))
    end

  fun deriveKeyHex (context : string) (material : string) : string =
    toHex (deriveKey context material)

  fun hashLen (outLen : int) (msg : string) : string =
    word8ArrayToString (hashInternal (msg, ivVec, 0w0, outLen))

  fun hashLenHex (outLen : int) (msg : string) : string =
    toHex (hashLen outLen msg)

end
