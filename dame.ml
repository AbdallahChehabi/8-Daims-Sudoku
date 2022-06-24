type coord = { x: int ; y: int };;

type value = { crd: coord ; v: int };;

type grid = { vl: value list ; l: int };;

(* val cmp : coord -> coord -> bool = <fun> *)
let cmp c1 c2 = c1.x == c2.x && c1.y == c2.y;;

(* val get_value_at : grid -> coord -> int = <fun> *)
let get_value_at g c =
	let rec aux ls c = match ls with
		| [] -> failwith "Grid vide"
		| q:: t -> if cmp q.crd c then q.v else aux t c
	in aux g.vl c;;

(* val set_value_at : grid -> coord -> int -> grid = <fun> *)
let set_value_at g c v =
	let rec aux vl c v acc = match vl with
		| [] -> { crd = c; v = v }:: acc
		| q:: t ->
				if (q.crd = c) then List.rev_append ({ crd = c; v = v }:: acc) t
				else aux t c v (q:: acc)
	in { vl = aux g.vl c v []; l = g.l };;

(* val possible_to_fill : grid -> coord -> bool = <fun> *)
let possible_to_fill g c =
	let rec aux vl c = match vl with
		| [] -> true
		| q:: t ->
				if q.crd.x = c.x
				|| q.crd.y = c.y
				|| abs(q.crd.x - c.x) = abs(q.crd.y - c.y) then false
				else aux t c
	in aux g.vl c;;

(* val empty_value : grid -> coord list = <fun> *)
let empty_value g =
	let c = { x = 0; y = 0 } in
	let rec aux g c acc =
		if c.x >= g.l then
			if c.y >= g.l - 1 then acc
			else aux g { x = 0; y = c.y +1 } acc
		else
		if possible_to_fill g c then
			aux g { x = c.x + 1; y = c.y } (c:: acc)
		else
			aux g { x = c.x + 1; y = c.y } acc
	in aux g c [];;

(* val possible_values_at : grid -> coord -> int list = <fun> *)
let possible_values_at g c = if possible_to_fill g c then [1] else [];;

(* val fill_at : coord -> grid -> grid list = <fun> *)
let fill_at c g =
	let v = possible_values_at g c in
	let rec aux c g acc = function
		| [] -> acc
		| q:: t -> aux c g ((set_value_at g c q) :: acc) t
	in aux c g [g] v;;

(* val have_n_queens : int -> grid list -> grid list = <fun> *)
let have_n_queens n list =
	let rec aux acc = function
		| [] -> acc
		| q:: t ->	if List.length q.vl = n then aux (q :: acc) t
				else aux acc t
	in aux [] list;;

(* val ele_pre_case : coord list -> coord -> coord list = <fun> *)
let ele_pre_case case c =
	let rec aux case c acc = match case with
		| [] -> acc
		| h :: t ->
				if (h.x < c.x && h.y <= c.y)
				|| h.y < c.y then
					aux t c acc
				else
					aux t c (h :: acc)
	in aux case c [];;

(* val concat : 'a list list -> 'a list = <fun> *)
let concat l =
	let rec aux acc = function
		| [] -> acc
		| q:: t -> aux (List.rev_append acc q) t
	in aux [] l;;

let solve f g =
	let rec aux g c =
		if c.x = g.l then
			if c.y = g.l - 1 then
				[g]
			else aux g { x = 0; y = c.y + 1 }
		else
			let case = (empty_value g) in
			if (List.exists (fun c2 -> c2 = c) case) = false then	aux g { x = c.x +1; y = c.y }
			else
				let grd = (fill_at c g) in
				concat (List.map (function f -> (aux f { x = (c.x +1); y = c.y })) grd)
	in f (aux g { x = 0; y = 0 });;

let complete z = List.filter(function z -> (List.length z.vl) = z.l) z ;;

let solutions = List.length(solve complete { vl =[] ; l = 8 });;

let iin g c =
	let rec aux vl c = match vl with
		| [] -> false
		| q:: t -> if (cmp q.crd c) then true else aux t c
	in aux g.vl c;;

let display g =
	let rec aux c =
		if (c.x == g.l) then
			if (c.y == g.l - 1) then print_char '\n'
			else let f = print_char '\n' in aux { x = 0 ; y = c.y +1 };
				print_char '\n'
		else
			let f = print_char ' ' in
			if (iin g c) then
				let f = print_char 'q' in aux { x = c.x +1 ; y = c.y }
			else let f = print_char '-' in aux { x = c.x +1 ; y = c.y }
	in aux { x =0 ; y =0 };;

let print = List.map display (solve complete { vl =[] ; l = 8 });;