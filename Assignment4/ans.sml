(* Q1 *)
fun fromTo (a,b) c =
  if a <= b then
    [c(a)] @ fromTo (a+1,b) c
  else
    [];

(* Q2 a *)
fun halve [] = ([],[])
  | halve (x::xs) = let
	fun alternate [] = []
	  | alternate (a::[]) = [a]
	  | alternate (a::b::t) = a::alternate t
  in
	  (alternate (x::xs), alternate xs)
  end;

(* Q2 b *)
fun merge (a, []) = a
  | merge ([], b) = b
  | merge (a::axs, b::bxs) =
    (a::b::merge (axs, bxs));

(* Q3 a *)
fun splitAtLast _ [] = ([],[])
  | splitAtLast el xs =
	let
		fun findLast i res [] = res
		  | findLast i res (h::t) = findLast (i+1) (if h = el then i else res) t;
		val spliti = findLast 1 0 xs
	in
		(List.take (xs, spliti), List.drop (xs, spliti))
	end;

(* Q3 b *)
fun splitAtFirst n xs =
let
  fun splt (_, axs, []) = (axs, [])
    | splt (n, axs, b::bxs) =
    if n = b then
      (axs, [b] @ bxs)
    else
      splt (n, axs @ [b], bxs);
in
  splt (n, [], xs)
end;

(* Q4 i *)

(* Q4 ii *)
fun join xs =
let
  fun j ([],_) = raise Fail "Empty Segment"
    | j (a,[]) = a
    | j (a, b) =
    if List.last a = List.hd b then
      a @ List.tl b
    else
      raise Fail "Disconnected Segments";
in
  foldr j [] xs 
end;

(* Q4 iii *)
fun valid xs =
let
  fun m [] = true
    | m (a::[]) = true
    | m (a::b::xs) =
    if List.last a = List.hd b then
      m (b::xs)
    else
      false;
  fun cons (f, []) = true
    | cons (f, [x]) = true
    | cons (f, x::y::xs) =
    if f (x, y) then
      cons (f, y::xs)
    else
      false;
  fun dir [] = 0
    | dir xs =
    if cons (op>, xs) then
      1
    else if cons (op=, xs) then
      2
    else if cons (op<, xs) then
      3
    else
      0;
  val dirs = List.map dir xs 
in
  List.all (fn(x) => not (x = [])) xs
  andalso m xs
  andalso List.all (fn(x) => not (x = 0)) dirs
  andalso cons ((fn(x,y) => not (x=y)), dirs)
end;
