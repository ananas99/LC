open Stream
open Genlex


(* Ã© necessÃ¡rio instalar previamente o camlp4 e o camlp5 
   (via o comando: opam install camlp4 camlp5)        *)
(* ocamlc str.cma -pp camlp4o ....... *)
(* ou  melhor ainda:  ocamlopt str.cmxa -pp camlp4o ....... *)

type variavel = string
type formula =
  | Implica of formula*formula
  | Equivale of formula*formula
  | Ou of formula*formula
  | E of formula*formula
  | Nao of formula
  | Var of variavel
  | Verdade
  | Falso


type nand =
  |V of variavel
  |Nand of nand*nand
 





 (*

E  ::= T E'
E' ::= -> T E'
E' ::= <-> T E'
E' ::= \epsilon
T  ::= F T'
T' ::= & F T'
T' ::= | F T'
T' ::= \epsilon
F  ::= N
F  ::= V
F  ::= ! E
F  ::= ( E )
*)


let lexer = Genlex.make_lexer ["("; ")"; "<->"; "->"; "|"; "&" ; "!"; "TRUE"; "FALSE"]

let rec parse_expr = parser (* corresponde a entrada E da gramatica *)
    [< e1 = parse_conj; e = parse_more_imps e1 >] -> e
and parse_more_imps e1 = parser (* corresponde a entrada E' da gramatica *)
    [< 'Kwd "->"; e2 = parse_conj; e = parse_more_imps (Implica(e1, e2)) >] -> e
  | [< 'Kwd "<->"; e2 = parse_conj; e = parse_more_imps (Equivale(e1, e2)) >] -> e
  | [< >] -> e1
and parse_conj = parser (* corresponde a entrada T da gramatica *)
    [< e1 = parse_simple; e = parse_more_conjs e1 >] -> e
and parse_more_conjs e1 = parser (* corresponde a entrada T' da gramatica *)
    [< 'Kwd "&"; e2 = parse_simple; e = parse_more_conjs (E(e1, e2)) >] -> e
  | [< 'Kwd "|"; e2 = parse_simple; e = parse_more_conjs (Ou(e1, e2)) >] -> e
  | [< >] -> e1
and parse_simple = parser (* corresponde a entrada F da gramatica *)
    [< 'Ident s >] -> Var s
  | [< 'Kwd "TRUE" >] -> Verdade
  | [< 'Kwd "FALSE" >] -> Falso
  | [< 'Kwd "!";  'Kwd "("; e = parse_expr; 'Kwd ")" >] -> Nao e
  | [< 'Kwd "("; e = parse_expr; 'Kwd ")" >] -> e;;




let parse_expression = parser [< e = parse_expr; _ = Stream.empty >] -> e;;

let formula_of_string s =
  parse_expression
    (lexer
       (Stream.of_string (Str.global_replace (Str.regexp "!") "! " s)
       ))

let rec string_of_nand form =
match form with
  | V v          ->  v
  | Nand(f,g)    ->  "(" ^ string_of_nand f ^ " % " ^ string_of_nand g ^ ")"

let read_formula () =  formula_of_string (read_line ())




let input =  read_formula ()

let rec transform form = match form with 
  | Var v          ->  V v
  | Verdade        ->  transform (Nao(Falso))
  | Falso          ->  transform (E(Var "Z",Nao(Var "Z")))
  | Implica(f, g)  ->  transform (Ou(Nao(f),g))                                                   
  | Equivale(f, g) ->  transform (E((Implica (f,g)),(Implica (g,f))))
  | E(f, g)        ->  Nand(Nand(transform f,transform g),Nand(transform f, transform g))
  | Ou(f, g)       ->  Nand(Nand(transform f,transform f),Nand(transform g, transform g))
  | Nao f          ->  Nand(transform f,transform f)




let res = string_of_nand(transform input)
let () = Printf.printf "%s\n" res

(*
 * Local Variables:
 * compile-command: "ocamlopt str.cmxa -pp camlp4o -o probD esqueletoD.ml"
 * End:
 *)
