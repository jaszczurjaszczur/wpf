(* wartosc bedzie lista przedzialow obustronnie domknietych;
 * zbior wartosci to jest suma tych przedzialow
 *)
type wartosc = (float * float) list
(* WAZNE OGRANICZENIA:
 * 1) przedzialy moga zaczynac sie na liczba float lub neg_infinity i konczyc liczba float lub infinity
 * 2) przedzialy powinny byc rozlaczne (chyba ze punkt 3) - dzieki temu zyskujemy na szybkosci
 * 3) przedzialy nie moga przecinac 0. - dzieki temu latwiej zapisuje sie operacje
 *)


(* FUNKCJE POMOCNICZE *)
let validate w =
	(* validate sluzy do zamienienia dowolnej listy przedzialow na liste spelniajaca 1) oraz 2) *)
	let rec merge_subsets w =
		match w with
			| [] -> []
			(* ten warunek sprawdza ograniczenie 3) *)
			| (a, b)::t -> if a < 0. && 0. < b then merge_subsets ((a,0.)::(0.,b)::t) else
				match merge_subsets t with
				| [] -> [(a, b)]
				| ((c, d)::inner_t as merged_tail) ->
					(* ten warunek sprawdza ograniczenie 2) chyba ze zachodzi 3) *)
					if b < c || (b=0. && c=0.) then
						(a, b)::merged_tail
					else
						merge_subsets ((a, max b d)::inner_t)
	(* lista przedzialow musi byc posortowana po poczatkach przedzialow *)
	in merge_subsets (List.sort compare w)

let apply_and_validate f wx wy =
	(* apply_and_merge przyjmuje funkcje i dwie wartoci i zwraca wartosc
	 * funkcja musi przyjmowac dwa przedzialy i zwracac przedzial
	 * apply_and_merge dla kazdego przedzialu z wx i dla kazdego przedzialu z wy
	 * generuje przedzial zwracany przez funkcje f i tworzy z nich wszystkich wartosc
	 * a nastepnie przy pomocy merge_and_split robi wartosc spelniajaca warunki 2) i 3)
	 *)
    let rec loop inner_wx acc =
        let rec inner_loop x inner_wy acc =
            match inner_wy with
                | [] -> acc
                | h::t -> inner_loop x t ((f x h)::acc)
        in
        match inner_wx with
            | [] -> acc
            | h::t -> loop t (inner_loop h wy acc)
    in validate (loop wx [])


(* KONSTRUKTORY *)
let wartosc_od_do x y =
	(* podstawowy konstruktor; jest niewrazliwy na kolejnosc x oraz y *)
	if x<=y then
		validate [(x, y)]
	else
		validate [(y, x)]

let wartosc_dokladnosc x p =
	wartosc_od_do (x*.(1.0 +. p /. 100.)) (x*.(1.0 -. p /. 100.))

let wartosc_dokladna x =
	wartosc_od_do x x


(* SELEKTORY *)
let rec in_wartosc w x =
	match w with
		| [] -> false
		| (a, b)::t -> if a<=x && x<=b then true
			else in_wartosc t x

let rec min_wartosc w =
	match w with
		| [] -> nan
		| (a, _)::_ -> a

let rec max_wartosc w =
	match w with
		| [] -> nan
		| [(_, b)] -> b
		| _::t -> max_wartosc t
		
let sr_wartosc w =
	let srednia = ((min_wartosc w) +. (max_wartosc w))/.2. in
	if srednia = infinity || srednia = neg_infinity then
		nan
	else
		srednia


(* MODYFIKATORY *)
let plus wx wy =
	let plus_basic (a, b) (c, d) =
		(* najmniejszy z najmniejszym, najwiekszy z najwiekszym *)
		(a+.c, b+.d)
	in apply_and_validate plus_basic wx wy

let minus wx wy =
	let minus_basic (a, b) (c, d) =
		(a-.d, b-.c)
	in apply_and_validate minus_basic wx wy

let razy_basic (a, b) (c, d) =
	(* funkcja uzywana zarowno w mnozeniu jak i w dzieleniu *)
	if a >= 0. && c >= 0. then
		(a*.c, b*.d)
	else if b <= 0. && c >= 0. then
		(a*.d, b*.c)
	else if a >= 0. && d <= 0. then
		(b*.c, a*.d)
	else (* b <= 0. && d <= 0. *)
		(b*.d, a*.c)

let razy wx wy =
	apply_and_validate razy_basic wx wy

let podzielic wx wy =
	let podzielic_basic x (a, b) =
		(* x/y = (x*(1/y)) ; obliczamy wiec odwrotnosc (a, b) *)
		if a < 0. && b = 0. then
			razy_basic x (neg_infinity, 1./.a)
		else if a = 0. && b > 0. then
			razy_basic x (1./.b, infinity)
		else if a = 0. && b = 0. then
			razy_basic x (neg_infinity, infinity)
		else
			razy_basic x (1./.b, 1./.a)
	in apply_and_validate podzielic_basic wx wy
