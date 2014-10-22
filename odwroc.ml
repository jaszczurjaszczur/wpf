let odwroc n =
    let rec loop n x =
        if n = 0 then x else
            loop (n/10) ((n mod 10)+x*10)
    in
        loop n 0;;

let rec silnia n =
    if n = 0 then 1 else
        n * (silnia (n-1));;

let silnia_o n =
    let rec loop n akr =
        if n = 0 then akr else
            loop (n-1) (akr*n)
    in
        loop n 1;;
