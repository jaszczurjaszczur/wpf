open Printf

let rec fib n =
  match n with
  | 0 -> -1
  | 1 -> 2
  | _ -> (fib(n-1) + fib(n-2))
;;

let rec fib_list n =
  match n with
  | 0 -> [fib 0]
  | _ -> fib(n)::fib_list(n-1)
  ;;

List.iter (printf "%d ") (fib_list 30)
(* print_int fib(1);; *)
