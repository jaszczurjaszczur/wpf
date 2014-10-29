open List

type wielomian = (float) list

let oblicz w x =
    (** [oblicz w x] oblicza wartość wielomianu [w] w punkcie [x] *)
    let rec loop inner_w acc =
        match inner_w with
            | [] -> acc
            | h::t -> loop t (acc*.x +. h)
    in loop w 0.0

let suma w1 w2 =
    (** Suma wielomianów *)
    let loop w1 w2 =
        match w1 w2 with
            

let iloczyn w1 w2 =
    let loop w1 

(*
val iloczyn : wielomian -> wielomian -> wielomian (** Iloczyn wielomianów *)
val pochodna : wielomian -> wielomian (** Pochodna wielomianu *)
val stopien : wielomian -> int (** Zwraca stopień wielomianu (czyli najwyższą potęgę argumentu, przy której wykładnik jest różny od 0.) *)
val calka : wielomian -> wielomian (** Calka wielomianu. [calka w] ma wyraz wolny równy 0. *) 
*)
