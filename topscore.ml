(*
  Accumulator of the top N values.

*)

type ('a, 'b) t = {
  cmp : 'a -> 'a -> int;
  req_count : int;
  max_count : int; (* length of the array *)
  accu : ('a * 'b) array;
  mutable pos : int;
  mutable last : 'a;
  mutable add_count : int;
}

let get_max_count req_count =
  2 * req_count


let create cmp req_count mini mini_data =
  if req_count <= 0 then
    invalid_arg "Ii_top.create";
  let max_count = get_max_count req_count in
  let accu = Array.make max_count (mini, mini_data) in
  { cmp = cmp;
    req_count = req_count;
    max_count = max_count;
    accu = accu;
    pos = 0;
    last = mini;
    add_count = 0 }


let compact r =
  let cmp = r.cmp in
  Array.fast_sort (fun (a, _) (b, _) -> cmp b a) r.accu;
  r.pos <- (min r.req_count r.pos);
  if r.pos > 0 then
    r.last <- fst r.accu.(r.pos - 1)

let add r k v =
  r.add_count <- r.add_count + 1;
  let c = r.cmp k r.last in
  if c < 0 then
    (* most frequent case: do nothing. *)
    ()
  else if (c = 0 && r.pos < r.req_count) || c > 0 then
    (* add *)
    (if r.pos = r.max_count then
       compact r;
     r.accu.(r.pos) <- (k, v);
     r.pos <- r.pos + 1)
  else
    (* same score as last, but already full *)
    ()

let get r =
  compact r;
  r.add_count, Array.sub r.accu 0 (min r.req_count r.pos)


module Int =
struct

  type 'a t = {
    req_count : int;
    max_count : int; (* length of the array *)
    accu : (int * 'a) array;
    mutable pos : int;
    mutable last : int;
    mutable add_count : int;
  }

  let create req_count mini mini_data =
    if req_count <= 0 then
      invalid_arg "Ii_top.create";
    let max_count = get_max_count req_count in
    let accu = Array.make max_count (mini, mini_data) in
    { req_count = req_count;
      max_count = max_count;
      accu = accu;
      pos = 0;
      last = mini;
      add_count = 0 }

  let compact r =
    Array.fast_sort (fun (a, _) (b, _) -> compare b a) r.accu;
    r.pos <- (min r.req_count r.pos);
    if r.pos > 0 then
      r.last <- fst r.accu.(r.pos - 1)
	
  let add r k v =
    r.add_count <- r.add_count + 1;
    let c = compare k r.last in
    if c < 0 then
      (* most frequent case: do nothing. *)
      ()
    else 
      if (k = 0 && r.pos < r.req_count) || c > 0 then
	(* add *)
	(if r.pos = r.max_count then
	   compact r;
	 r.accu.(r.pos) <- (k, v);
	 r.pos <- r.pos + 1)
      else
	(* same score as last, but already full *)
	()

  let get r =
    compact r;
    r.add_count, Array.sub r.accu 0 (min r.req_count r.pos)

end

module Float =
struct

  type 'a t = {
    req_count : int;
    max_count : int; (* length of the array *)
    accu : (float * 'a) array;
    mutable pos : int;
    mutable last : float;
    mutable add_count : int
  }

  let create req_count mini mini_data =
    if req_count <= 0 then
      invalid_arg "Ii_top.create";
    let max_count = get_max_count req_count in
    let accu = Array.make max_count (mini, mini_data) in
    { req_count = req_count;
      max_count = max_count;
      accu = accu;
      pos = 0;
      last = mini;
      add_count = 0 }

  let compact r =
    Array.fast_sort (fun (a, _) (b, _) -> compare (b : float) a) r.accu;
    r.pos <- (min r.req_count r.pos);
    if r.pos > 0 then
      r.last <- fst r.accu.(r.pos - 1)
	
  let add r k v =
    r.add_count <- r.add_count + 1;
    let c = compare k r.last in
    if c < 0 then
      (* most frequent case: do nothing. *)
      ()
    else if (c = 0 && r.pos < r.req_count) || c > 0 then
      (* add *)
      (if r.pos = r.max_count then
	 compact r;
       r.accu.(r.pos) <- (k, v);
       r.pos <- r.pos + 1)
    else
      (* same score as last, but already full *)
      ()

  let get r =
    compact r;
    r.add_count, Array.sub r.accu 0 (min r.req_count r.pos)

end



let int_data =
  lazy (Array.init 1_000_000 (fun _ -> Random.int 65536 + 1), 1000)

let float_data =
  lazy (let a, n = Lazy.force int_data in
	Array.map float a, n)

let gen_int_test () =
  let a, req_count = Lazy.force int_data in
  let t1 = Unix.gettimeofday () in
  let n = ref 0 in
  let cmp x y = 
    incr n;
    compare x y in
  let r = create cmp req_count 0 0 in
  for i = 0 to Array.length a - 1 do
    let x = a.(i) in
    add r x x
  done;
  let result = get r in
  let t2 = Unix.gettimeofday () in
  !n, t2 -. t1, result

let int_test () =
  let a, req_count = Lazy.force int_data in
  let t1 = Unix.gettimeofday () in
  let r = Int.create req_count 0 0 in
  for i = 0 to Array.length a - 1 do
    let x = a.(i) in
    Int.add r x x
  done;
  let result = Int.get r in
  let t2 = Unix.gettimeofday () in
  t2 -. t1, result


let gen_float_test () =
  let a, req_count = Lazy.force float_data in
  let t1 = Unix.gettimeofday () in
  let n = ref 0 in
  let cmp x y = 
    incr n;
    compare x y in
  let r = create cmp req_count 0. 0. in
  for i = 0 to Array.length a - 1 do
    let x = a.(i) in
    add r x x
  done;
  let result = get r in
  let t2 = Unix.gettimeofday () in
  !n, t2 -. t1, result

let float_test () =
  let a, req_count = Lazy.force float_data in
  let t1 = Unix.gettimeofday () in
  let r = Float.create req_count 0. 0. in
  for i = 0 to Array.length a - 1 do
    let x = a.(i) in
    Float.add r x x
  done;
  let result = Float.get r in
  let t2 = Unix.gettimeofday () in
  t2 -. t1, result

(*
let () =
  let print s (nc, t, (n, a)) =
    Printf.printf
      "top %i/%i %s: %i comparisons in %g seconds\n" 
      (Array.length a) n s nc t
  in

  let print2 s (t, (n, a)) =
    Printf.printf
      "top %i/%i %s: %g seconds\n" 
      (Array.length a) n s t
  in

  print "gen_int_test" (gen_int_test ());
  print2 "int_test" (int_test ());
  print "gen_float_test" (gen_float_test ());
  print2 "float_test" (float_test ())
*)
(*
  Output on Martin's laptop:

top 1000/1000000 gen_int_test: 1158479 comparisons in 0.0551751 seconds
top 1000/1000000 int_test: 0.0258029 seconds
top 1000/1000000 gen_float_test: 1158479 comparisons in 0.0965939 seconds
top 1000/1000000 float_test: 0.0469511 seconds
*)
