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
  fun splt _ axs [] = (axs, [])
    | splt n axs (b::bxs) =
    if n = b then
      (axs, [b] @ bxs)
    else
      splt n (axs @ [b]) bxs;
in
  splt n [] xs
end;


(* Q4 i *)
fun break [] = []
  | break [(x : int)] = [[x]]
  | break (x::y::xs) =
	let
		val cmp = if x > y then op> else if x = y then op= else op<;
		fun ordseg seg [] = (seg, [])
		  | ordseg seg (h::[]) = (seg @ [h], [])
		  | ordseg seg (h::s::t) =
			if cmp (h, s) then ordseg (seg @ [h]) (s::t) else (seg @ [h], h::s::t);
		val (seg, rest) = ordseg [] (x::y::xs);
	in
		[seg] @ break rest
	end;


(* Q4 ii *)
fun join xs =
let
  fun j ([],_) = raise Fail "Empty Segment"
    | j (a,[]) = a
    | j (a, (b::bs)) =
    if List.last a = b then
      a @ bs
    else
      raise Fail "Disconnected Segments";
in
  foldr j [] xs
end;

(* Q4 iii *)
