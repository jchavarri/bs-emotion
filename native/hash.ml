let to_base b v =
  let rec to_base' a v = if v = 0l then a else to_base' (Int32.rem v b :: a) (Int32.div v b) in
  to_base' [] v

let to_alpha_digit n = if n < 10 then char_of_int (n + int_of_char '0') else char_of_int (n + int_of_char 'a' - 10)

let to_alpha_digits ds =
  let buf = Buffer.create (List.length ds) in
  List.iter (fun i -> Buffer.add_char buf (to_alpha_digit (Int32.to_int i))) ds;
  Buffer.contents buf

(* Adapted from: https://github.com/emotion-js/emotion/blob/fa977675e1df05f210005ccd3461d6cdaf941b42/packages/hash/src/index.js *)

let murmur2 str =
  (* 'm' and 'r' are mixing constants generated offline.
  They're not really 'magic', they just happen to work well.

  const m = 0x5bd1e995;
  const r = 24; *)

  (* Initialize the hash *)
  let h = ref 0l in

  (* Mix 4 bytes at a time into the hash *)
  let i = ref 0 in
  let len = ref (String.length str) in
  let k = ref 0l in
  let chr_and chr const =
    let open Int32 in
    logand (of_int @@ Char.code @@ chr) (of_int const)
  in
  let ( || ) = Int32.logor in
  let ( && ) = Int32.logand in
  let ( ++ ) = Int32.add in
  let ( << ) = Int32.shift_left in
  let ( >>> ) = Int32.shift_right_logical in
  let ( ** ) = Int32.mul in
  let ( ^^ ) = Int32.logxor in
  while !len >= 4 do
    k :=
      chr_and str.[!i] 0xff
      || chr_and str.[!i + 1] 0xff << 8
      || chr_and str.[!i + 2] 0xff << 16
      || chr_and str.[!i + 3] 0xff << 24;

    k := (* Math.imul(k, m): *)
         ((!k && 0xffffl) ** 0x5bd1e995l) ++ ((!k >>> 16) ** 0xe995l << 16);

    k := !k ^^ (* k >>> r: *) (!k >>> 24);

    h :=
      (* Math.imul(k, m): *)
      (((!k && 0xffffl) ** 0x5bd1e995l) ++ ((!k >>> 16) ** 0xe995l << 16))
      ^^ (* Math.imul(h, m): *)
      (((!h && 0xffffl) ** 0x5bd1e995l) ++ ((!h >>> 16) ** 0xe995l << 16));

    len := !len - 4;
    i := !i + 4
  done;

  (* Handle the last few bytes of the input array *)
  let () =
    match !len with
    | 3 -> h := !h ^^ (chr_and str.[!i + 2] 0xff << 16)
    | 2 -> h := !h ^^ (chr_and str.[!i + 1] 0xff << 8)
    | 1 ->
      h := !h ^^ chr_and str.[!i] 0xff;
      h := (* Math.imul(h, m): *)
           ((!h && 0xffffl) ** 0x5bd1e995l) ++ ((!h >>> 16) ** 0xe995l << 16)
    | _ -> ()
  in

  (* Do a few final mixes of the hash to ensure the last few
   bytes are well-incorporated. *)
  h := !h ^^ (!h >>> 13);
  h := (* Math.imul(h, m): *)
       ((!h && 0xffffl) ** 0x5bd1e995l) ++ ((!h >>> 16) ** 0xe995l << 16);

  to_alpha_digits (to_base 36l (!h ^^ (!h >>> 15) >>> 0))