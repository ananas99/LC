open Array
open Printf
open Scanf


let n = scanf " %d" (fun n -> n)
let i,j = scanf " %d  %d" (fun a b -> (a,b))
let board = make_matrix n n (-1)

let set matrix (x,y) value = matrix.(x).(y) <- value 

let get matrix (x,y) = matrix.(x).(y) 

let () = set board (i,j) 0

let k = scanf " %d" (fun k -> k)


let printMatrix board n = 
        for i = 0 to n - 1 do
                for j = 0 to n - 1 do
                        print_int board.(i).(j); 
                        printf " "
                done;           
                Printf.printf "\n"
        done


let kPositions k = 
    for i = 0 to (k - 1) do
        scanf " %d %d" (fun x y -> set board (x,y) 0)
    done

let moves (a,b) = [(a+1,b-2);(a-1,b-2) ;
                         (a+2,b-1) ; (a-2,b-1) ;
                         (a+2,b+1) ; (a-2,b-1) ;
                         (a+1,b+2) ; (a-1,b+2) ;]
 

 


let boundary (x,y) board =  (x >= 0 && x < n && y >= 0 && y < n && board.(x).(y) = -1)

let safeMove pos board =
        let rec sM l = match l with
        |[] -> []
        |el::li -> if (boundary el board) then (el::(sM li)) else sM li

    in sM (moves pos)



let rec knightsTour pos i =
    if i = (n*n) then true else
        let rec aux l = match l with
            | [] -> false 
            | el::li ->
              
              let () = (set board el i) in
              if (knightsTour el (i+1)) then true
              else
                let () = (set board el (-1)) in
                aux li
        in aux (safeMove pos board)


let () = if k != 0 then (kPositions k)
let () = if (knightsTour (i,j) 1) then printf "YES\n" else printf "NO\n"


let () = printMatrix board n

