open String;;
open Char;;


let sublist s =
    let rec aux i l =
        if i < 0 then l else aux (i - 1) (l@[(String.sub s i ((String.length s) - i))])
    in (aux (String.length s) []) ;;


type suffix_tree = 
    |Empty
    |Node of int * suffix_tree array;;


let createNode () = Node (0,Array.make 26 Empty);;
let getIndex c = (code c) - (code 'a');;


let rec build_tree l root nstring = match l with
    |[] -> ()
    |el::li -> 
        let rec aux n st = match st with
            |Empty -> ()
            |Node (_, vec) -> if  (n < length el) then
                                let index = getIndex (el.[n]) in
                                match (vec.(index)) with 
                                    |Empty -> let () = vec.(index) <- createNode () in aux (n+1) vec.(index) 
                                    |Node (a,b) -> if (nstring < (a+1)) then 
                                                       build_tree li root nstring  
                                                   else (if (nstring = (a+1)) then 
                                                            let () = vec.(index) <- Node(a+1,b) in
                                                            aux (n+1) vec.(index))
                              else build_tree li root nstring  
        in (aux 0 root)                  
;; 

let r = createNode()



let rec abc l cnt = match l with
    |[] -> ()
    |el::li -> let () = build_tree el r cnt in 
               abc li (cnt+1)
;;

let () = build_tree ["banana";"anana";"nana";"ana";"na";"a"] r 1 

