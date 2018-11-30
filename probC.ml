
type suffix_tree = 
  |Empty
  |Node of int * suffix_tree array


let sublist s =
  let rec aux i l =
    if i < 0 then l else aux (i - 1) (l@[(String.sub s i ((String.length s) - i))])
  in (aux ((String.length s) - 1) []) 




let createNode () = Node (1,Array.make 26 Empty)

let getIndex c = (Char.code c) - (Char.code 'a')

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


let rec string_iter root l cnt = match l with
  |[] -> ()
  |el::li -> let () = build_tree el root cnt in 
    string_iter root li (cnt+1)



let rec depth  nstr node cnt = match node with 
  |Empty -> cnt
  |Node (a,b) -> Array.fold_left (fun acc x -> match x with 
      |Empty -> acc                
      |Node (w,z) ->  if w = nstr then 
          Pervasives.max acc (depth nstr (Node(w,z)) (cnt+1))  else acc) cnt b      




let read () = 
    let rec aux_read l =
      let aux = input_line stdin in
      try aux_read (aux::l)
      with End_of_file -> (aux::l) 
    in aux_read []






let r = createNode()
let aux_wlist = (read ())


let wlist = List.fold_left (fun l x -> (sublist x)::l) [] aux_wlist
let () = string_iter r wlist 1
let res = depth (List.length aux_wlist) r 0 ;;
let () = Printf.printf "%d\n" res







 

