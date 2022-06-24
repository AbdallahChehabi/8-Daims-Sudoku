type coord = { x: int ; y: int };;

type value = { crd: coord ; v: int };;

type grid = { vl: value list ; l: int };;

let cmp c1 c2 = c1.x == c2.x && c1.y == c2.y;;

let get_value_at g c =
	let rec aux ls c = match ls with
		| [] -> failwith "Grid vide"
		| q:: t -> if cmp q.crd c then q.v else aux t c
	in aux g.vl c;;

let set_value_at g c v =
	let rec aux vl c v acc = match vl with
		| [] -> { crd = c; v = v }:: acc
		| q:: t ->
				if (q.crd = c) then List.rev_append ({ crd = c; v = v }:: acc) t
				else aux t c v (q:: acc)
	in { vl = aux g.vl c v []; l = g.l };;

let iin g c =
	let rec aux vl c = match vl with
		| [] -> false
		| q:: t -> if (cmp q.crd c) then true else aux t c
	in aux g.vl c;;

let empty_value g =
	let rec aux g c acc =
		if c.x >= g.l then
			if c.y >= g.l - 1 then acc
			else aux g { x = 0; y = c.y +1 } acc
		else
		if not (iin g c) then
			aux g { x = c.x + 1; y = c.y } (c:: acc)
		else
			aux g { x = c.x + 1; y = c.y } acc
	in aux g { x = 0; y = 0 } [];;


let possible_values_at g c = g;;

let fill_at c g =
	let v = possible_values_at g c in
	let rec aux c g acc v = match v with
		| [] -> acc
		| q:: t -> aux c g ((set_value_at g c q) :: acc) t
	in aux c g [] v;;


let value_x g c =
	let rec aux acc = function
	| [] -> acc
	| q::t -> if (q.crd.x = c.x && q.crd != c) then aux (q.v::acc) t else aux acc t
	in aux [] g.vl;;


let value_y g c =
	let rec aux acc = function
	| [] -> acc
	| q::t -> if (q.crd.y = c.y && q.crd != c) then aux (q.v::acc) t else aux acc t
	in aux [] g.vl;;


let complete list =
	let rec aux acc = function
		| [] -> acc
		| q:: t ->	if List.length q.vl = (q.l * q.l) then aux (q:: acc) t else aux acc t
	in aux [] list;;


let sudoku = {
		vl = [ 
			{crd={x=1;y=1};v=1};{crd={x=1;y=2};v=2};{crd={x=1;y=3};v=3};{crd={x=1;y=4};v=4};{crd={x=1;y=5};v=5};{crd={x=1;y=6};v=6};{crd={x=1;y=7};v=7};{crd={x=1;y=8};v=8};{crd={x=1;y=9};v=9};
			{crd={x=2;y=1};v=2};{crd={x=2;y=2};v=0};{crd={x=2;y=3};v=0};{crd={x=2;y=4};v=0};{crd={x=2;y=5};v=0};{crd={x=2;y=6};v=0};{crd={x=2;y=7};v=0};{crd={x=2;y=8};v=0};{crd={x=2;y=9};v=0};
			{crd={x=3;y=1};v=3};{crd={x=3;y=2};v=0};{crd={x=3;y=3};v=0};{crd={x=3;y=4};v=0};{crd={x=3;y=5};v=0};{crd={x=3;y=6};v=0};{crd={x=3;y=7};v=0};{crd={x=3;y=8};v=0};{crd={x=3;y=9};v=0};
			{crd={x=4;y=1};v=4};{crd={x=4;y=2};v=0};{crd={x=4;y=3};v=0};{crd={x=4;y=4};v=0};{crd={x=4;y=5};v=0};{crd={x=4;y=6};v=0};{crd={x=4;y=7};v=0};{crd={x=4;y=8};v=0};{crd={x=4;y=9};v=0};
			{crd={x=5;y=1};v=5};{crd={x=5;y=2};v=0};{crd={x=5;y=3};v=0};{crd={x=5;y=4};v=0};{crd={x=5;y=5};v=0};{crd={x=5;y=6};v=0};{crd={x=5;y=7};v=0};{crd={x=5;y=8};v=0};{crd={x=5;y=9};v=0};
			{crd={x=6;y=1};v=6};{crd={x=6;y=2};v=0};{crd={x=6;y=3};v=0};{crd={x=6;y=4};v=0};{crd={x=6;y=5};v=0};{crd={x=6;y=6};v=0};{crd={x=6;y=7};v=0};{crd={x=6;y=8};v=0};{crd={x=6;y=9};v=0};
			{crd={x=7;y=1};v=7};{crd={x=7;y=2};v=0};{crd={x=7;y=3};v=0};{crd={x=7;y=4};v=0};{crd={x=7;y=5};v=0};{crd={x=7;y=6};v=0};{crd={x=7;y=7};v=0};{crd={x=7;y=8};v=0};{crd={x=7;y=9};v=0};
			{crd={x=8;y=1};v=8};{crd={x=8;y=2};v=0};{crd={x=8;y=3};v=0};{crd={x=8;y=4};v=0};{crd={x=8;y=5};v=0};{crd={x=8;y=6};v=0};{crd={x=8;y=7};v=0};{crd={x=8;y=8};v=0};{crd={x=8;y=9};v=0};
			{crd={x=9;y=1};v=9};{crd={x=9;y=2};v=0};{crd={x=9;y=3};v=0};{crd={x=9;y=4};v=0};{crd={x=9;y=5};v=0};{crd={x=9;y=6};v=0};{crd={x=9;y=7};v=0};{crd={x=9;y=8};v=0};{crd={x=9;y=9};v=0};
			];
			l = 9;
						};;

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


let display g =
	let rec aux c =
		if (c.x == g.l) then
			if (c.y == g.l - 1) then print_char '\n'
			else let f = print_char '\n' in aux { x = 0 ; y = c.y +1 };
				print_char '\n'
		else
			let f = print_char ' ' in
			if (iin g c) then
				let f = print_int (get_value_at g c) in aux { x = c.x +1 ; y = c.y }
			else let f = print_char '-' in aux { x = c.x +1 ; y = c.y }
	in aux { x =0 ; y =0 };;


let ex = solve complete sudoku;;

let print = List.map display ex;;