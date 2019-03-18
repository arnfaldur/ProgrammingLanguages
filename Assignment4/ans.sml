(* Q1 *)
fun fromTo (a,b) c =
  if a <= b then
    [c(a)] @ fromTo (a+1,b) c
  else
    [];

(* Q2 a *)
(* fun halve a =
  let
    fun atn ([], _) = []
    |   atn (x::xs, b) =
      if b then
        [x]
      else
        atn(xs, not b);
  in
    (atn (a, true), atn (a, false))
  end; *)
fun halve a = ListPair.unzip a

(* Q2 b *)
fun merge (a, []) = a
  | merge ([], b) = b
  | merge (a::axs, b::bxs) =
      [a] @ [b] @ merge (axs, bxs);


(* Q3 a *)

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
