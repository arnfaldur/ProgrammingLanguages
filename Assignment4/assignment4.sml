(*
* Assignment 4 in FMAL
*
* Authors:
*       Arnaldur Bjarnason    (arnaldur15@ru.is)
*       Jökull Máni Reynisson (jokull16@ru.is)       
*)

(* Q1 *)
fun fromTo (a,b) c =
  if a <= b then
    [c(a)] @ fromTo (a+1,b) c
  else
    [];

(* Q2 i *)
fun halve [] = ([],[])
  | halve (x::xs) = 
  let
    fun alternate [] = []
      | alternate (a::[]) = [a]
      | alternate (a::b::t) = a::alternate t
  in
    (alternate (x::xs), alternate xs)
  end;

(* Q2 ii *)
fun merge (a, []) = a
  | merge ([], b) = b
  | merge (a::axs, b::bxs) =
    (a::b::merge (axs, bxs));

(* Q3 i *)
fun splitAtLast _ [] = ([],[])
  | splitAtLast (el : int) (xs : int list) =
  let
    fun findLast i res [] = res
      | findLast i res (h::t) = findLast (i+1) (if h = el then i else res) t;
    val spliti = findLast 1 0 xs
  in
    (List.take (xs, spliti), List.drop (xs, spliti))
  end;

(* Q3 ii *)
fun splitAtFirst (el : int) (xs : int list) =
  let
    fun splt axs [] = (axs, [])
      | splt axs (b::bxs) =
      if el = b then
        (axs, [b] @ bxs)
      else
        splt (axs @ [b]) bxs;
  in
    splt [] xs
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
      | j (a : int list, (b::bs) : int list) =
      if List.last a = b then
        a @ bs
      else
        raise Fail "Disconnected Segments";
  in
    foldr j [] xs
  end;

(* Q4 iii *)
fun valid [] = true
  | valid [x] = true
  | valid ([]::y::re) = false
  | valid (_::[]::re) = false
  | valid ((x::[])::y::re) = x = List.hd y andalso valid (y::re)
  | valid ((x::xp::xs)::y::re) =
  let
    val cmp = if x > xp then op> else if x = xp then op= else op<;
    fun cons [] = true
      | cons [h] = true
      | cons (h::s::t) =
      if cmp (h, s) then
        cons (s::t)
      else
        false;
  in
    cons (x::xp::xs)
    andalso not (cons y)
    andalso List.last (xp::xs) = List.hd y
    andalso valid (y::re)
  end;
