(* autor: Artur Matyjasek *)
(* code review: Michał Borowski *)

type point = float * float

type kartka = point -> int

let prostokat (p1:point) (p2:point) = fun (a:point) ->
  match a, p1, p2 with
  | (a, b), (x1, y1), (x2, y2) ->
    if x1 <= a && a <= x2 && y1 <= b && b <= y2 then 1 else 0

(* kwadrat liczby *)
let sq x = x *. x

let kolko (p:point) r = fun (a:point) ->
  match a, p with
  | (a, b), (x1, y1) ->
    if sq(x1 -. a) +. sq (y1 -. b) <= sq r then 1 else 0

(* sprawdza, czy punkt (ax, ay) jest na lewo od prostej przechodzącej przez
   punkty (bx, by), (cx, cy) patrząc od b w stronę c.
   zwraca liczbę > 0 gdy a jest na lewo,
   = 0 gdy a jest na prostej i
   < 0 gdy a jest na prawo *)
let na_lewo (bx, by) (cx, cy) (ax, ay) =
  (cx -. bx) *. (ay -. by) -. (ax -. bx) *. (cy -. by)

(* zwraca punkt symetryczny do (cx, cy) względem prostej przechodzącej przez
   punkty p1 i p2 *)
let symetria (p1x, p1y) (p2x, p2y) (cx,cy) =
  let (cwx, cwy) = (cx -. p1x, cy -. p1y) in
  let (pwx, pwy) = (p2x -. p1x, p2y -. p1y) in
  let dot = (cwx *. pwx) +. (cwy *. pwy) in
  let pmagsq = (pwx *. pwx) +. (pwy *. pwy) in
  let (ox, oy) = (pwx /. pmagsq *. dot +. p1x, pwy /. pmagsq *. dot +. p1y) in
  (cx +. 2.0 *. (ox -. cx), cy +. 2.0 *. (oy -. cy))

let zloz (p1:point) (p2:point) (k:kartka) = fun (x,y) -> 
  let l = na_lewo p1 p2 (x, y) in
  if l = 0. then k (x, y) 
  else if l < 0. then 0
  else k (x, y) + k (symetria p1 p2 (x, y))

let skladaj lista kartka =
  let f k (p1, p2) = zloz p1 p2 k in
  List.fold_left f kartka lista
    
  
