(* Q1 *)
fun fromTo (a,b) c =
  if a <= b then
    [c(a)] @ fromTo (a+1,b) c
  else
    [];

(* Q2 a *)
fun halve a =
  let
    fun atn ([], _) = []
    |   atn (x::xs, b) =
      if b then
        [x]
      else
        atn(xs, not b);
  in
    (atn (a, true), atn (a, false))
  end;

(* Q2 b *)
fun merge (a, []) = a
  | merge ([], b) = b
  | merge (a::axs, b::bxs) =
      [a] @ [b] @ merge (axs, bxs);


(* Q3 a *)

(* Q3 b *)
fun splitAtFirst at xs =
let
  fun splt _ (as, []) = (as, [])
    | splt at (as, b::bs) =
    if at = b then
      (as, [b] @ bs)
    else
      splt at (as @ b, bs)
in
  splt at ([], xs)
end;
