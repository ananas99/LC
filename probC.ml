type suffix_tree = 
    |Empty
    |Node of int * suffix_tree array;;


let sublist s =
    let rec aux i l =
        if i < 0 then l else aux (i - 1) (l@[(String.sub s i ((String.length s) - i))])
    in (aux (String.length s) []) ;;


let createNode () = Node (1,Array.make 26 Empty);;

let getIndex c = (Char.code c) - (Char.code 'a');;

let rec build_tree l root nstring = match l with
    |[] -> ()
    |el::li -> 
        let rec aux n st = match st with
            |Empty -> ()
            |Node (_, vec) -> if  (n < String.length el) then
                                let index = getIndex (el.[n]) in
                                match (vec.(index)) with 
                                    |Empty -> let () = vec.(index) <- createNode () in aux (n+1) vec.(index) 
                                    |Node (a,b) -> if (nstring < (a+1)) then 
                                                       aux (n+1) vec.(index) 
                                                   else (if (nstring = (a+1)) then 
                                                            let () = vec.(index) <- Node(a+1,b) in
                                                            aux (n+1) vec.(index)
                                                        else build_tree li root nstring) 
                              else build_tree li root nstring  
        in (aux 0 root)                  
;; 

let rec string_iter l cnt = match l with
    |[] -> ()
    |el::li -> let () = build_tree el r cnt in 
               string_iter li (cnt+1)
;;


let rec depth  nstr node acc l = match node with 
                        |Empty -> () 
                        |Node (a,b) -> Array.iter (fun x -> match x with 
                                            |Empty -> ()                 
                                            |Node (w,z) ->  if w = nstr then (
                                                                let () = Printf.printf "%d\n" acc in
                                                                depth nstr (Node(w,z)) (acc+1) (acc::l) )) b      
                                                                 
                                                           

;;







(*let () = build_tree ["banana";"anana";"nana";"ana";"na";"a"] r 1 *);;

(*let () = string_iter [["lllaa";"llaa";"laa";"aa";"a"];["llaa";"laa";"aa";"a"]] 1;;*)

let r = createNode();;
let () = string_iter [["lll";"ll";"l"];["ll";"l"]] 1;;



let l = [];;

let () = depth 2 r 1 l;;

(*let () = Printf.printf "%d\n" !z*);;





let () = List.iter (Printf.printf "%d ") l