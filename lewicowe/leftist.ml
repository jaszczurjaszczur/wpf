(* 'Drzewa lewicowe' *)
(* Sebastian Jaszczur *)
(* 2014-11-01 *)

module Leftist : sig
	type 'a leftist
	val empty : 'a leftist
	val push : 'a leftist -> 'a -> 'a leftist
	val join : 'a leftist -> 'a leftist -> 'a leftist
	val min : 'a leftist -> 'a
	val pop : 'a leftist -> 'a leftist
	exception Empty
end =
struct
	type 'a leftist = Leaf | Node of ('a leftist * 'a leftist * 'a * int)
	
	let rec make_tree root_val first second =
		(* it makes tree from desired value in root and two subtrees *)
		(* it checks if heights are in order *)
		(* it DOESN'T CHECK if values of tree1 and tree2 are smaller than root_val !! *)
		match first with
		| Leaf ->
			if second = Leaf
				then Node (Leaf, Leaf, root_val, 1)
				else make_tree root_val second first
		| Node (_, _, _, first_height) ->
			match second with
			| Leaf -> Node (first, second, root_val, 1)
			| Node (_, _, _, second_height) ->
				if first_height < second_height
					then Node (second, first, root_val, first_height+1)
					else Node (first, second, root_val, second_height+1)
	
	let make_sapling root_val =
		make_tree root_val Leaf Leaf
	
	let rec join d1 d2 =
		match d1 with
		| Leaf -> d2
		| Node (d1_left, d1_right, d1_val, _) ->
			match d2 with
			| Leaf -> d1
			| Node (_, _, d2_val, _) ->
				if d1_val > d2_val then join d2 d1
				else (* d1_val <= d2_val *)
					let d3 = join d1_right d2 in
					make_tree d1_val d1_left d3
	
	let push tree value =
		join tree (make_sapling value)
	
	exception Empty
	
	let pop tree =
		match tree with
		| Leaf -> raise Empty
		| Node (left, right, _, _) -> join left right
	
	let min tree =
		match tree with
		| Leaf -> raise Empty
		| Node (_, _, value, _) -> value
	
	let empty = Leaf
		
end

(* Priority queue implementation, using above leftist tree module *)

type 'a queue = 'a Leftist.leftist

let empty = Leftist.empty

let add value queue =
	Leftist.push queue value

exception Empty = Leftist.Empty

let delete_min queue =
	(Leftist.min queue, Leftist.pop queue)

let join first_queue second_queue =
	Leftist.join first_queue second_queue

let is_empty queue =
	(Leftist.empty == queue)

