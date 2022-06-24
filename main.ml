type coord = { x:int ; y:int };;

type 'a element = { crd:coord ; vl:'a };;

type 'a grid = { el:'a element list ; l:int };;

(* val get_value_at : 'a grid -> coord -> 'a = <fun> *)
let get_value_at g c =
	let rec aux ls c = match ls with
		| [] -> failwith "Grid vide ou coord introuvee"
		| q:: _ when q.crd = c -> q.vl
		| _:: t -> aux t c 
	in aux (g.el) c;;


(* val set_value_at : 'a grid -> coord -> 'a -> 'a grid = <fun> *)
let set_value_at g c v =
	let rec aux ls c v acc = match ls with
	| [] -> {crd = c ; vl = v} :: acc;
	| h :: q -> 
		if h.crd = c then
			 ({crd = c; vl = v} :: acc) @ q
		else aux q c v (h::acc)
	in {el = aux g.el c v []; l = g.l};;


(* val possible_rempli : coord -> 'a grid -> bool = <fun> *)
let possible_rempli c g =
	let rec aux ls c = match ls with
	| [] -> true
	| h :: t -> 
		if h.crd.x = c.x 
			 || h.crd.y = c.y
			 || abs (h.crd.x - c.x) = abs (h.crd.y - c.y) then
				 false
		 else aux t c 
	in aux g.el c;;


(* val empty_value : 'a grid -> coord list = <fun> *)
let empty_value g =
		let c = {x = 0; y = 0} in
		let rec aux g c acc =
			if c.x >= g.l then
				if c.y >= g.l - 1 then acc
				else aux g {x = 0; y = c.y +1} acc
			else
			if possible_rempli c g then
				aux g {x = c.x + 1; y = c.y} (c:: acc)
			else
				aux g {x = c.x + 1; y = c.y} acc
		in aux g c [];;


(* val possible_values_at : 'a grid -> coord -> int list = <fun> *)
let possible_values_at g c = if possible_rempli c g then [1] else [];;


(* val fill_at : coord -> int grid -> int grid list = <fun> *)
let fill_at c g =
	let v = possible_values_at g c in
	 let rec aux c g acc = function
		| [] -> acc 
		| h :: t -> aux c g ((set_value_at g c h) :: acc) t in
			 aux c g [] v;;
	

let have_n_dames n listt =
	let rec aux acc = function
		| [] -> acc
		| h :: t ->
				if List.length h.el = n then
					aux (h :: acc) t
				else
					aux acc t
	in aux [] listt;;

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


(* val solve : (int grid list -> 'a) -> int grid -> 'a = <fun> *)
let solve f g = 
	let rec aux g c =
		if c.x = g.l then
			if c.y = g.l - 1 then
				[g]
			else aux g {x = 0; y = c.y + 1}
		else 
			let case = ele_pre_case (empty_value g) c in 
			if List.exists (fun c2 -> c2 = c) case = false then
				aux g {x = c.x+1; y = c.y}
			else 
				let listt = List.flatten(List.map (function x -> (fill_at c g)) case) in
					List.flatten (List.map (function gr -> (aux gr {x = (c.x+1); y = c.y})) listt)
	in f (aux g {x = 0; y = 0});;

(* essaie *)
let solutions n =
 
  let show board =
    let pr v =
      for i = 1 to n do
        print_string (if i=v then " q" else " _");
      done;
      print_newline() in
    List.iter pr board;
    print_newline() in
 
  let rec safe i j k = function
    | [] -> true
    | h::t -> h<>i && h<>j && h<>k && safe i (j+1) (k-1) t in
 
  let rec loop col p =
    for i = 1 to n
    do
      if safe i (i+1) (i-1) p then
        let p' = i::p in
        if col = n then show p'
        else loop (col+1) p'
    done in
 
  loop 1 [] in
 
let n =
  if Array.length Sys.argv > 1
  then int_of_string Sys.argv.(1)
  else 8 in
 
solutions n