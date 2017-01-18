(*My tests*)
type opsys_nonterminals =
   | Ios | Linux | Unix | Windows 


let accept_all rules string = Some (rules, string)
let accept_empty_suffix rules = function
  | [] -> Some (rules, [])
  | _ -> None

let small_rules =
[
  Ios, [ N Linux; N Unix; ];
  Ios, [T "I"; N Windows; T "S"];
  Ios, [ N Windows];
  Linux, [ T "I"];
  Unix, [ T"U"];
  Windows, [T "P"];
  Windows, [T "U"];
  Linux, [N Windows; T "L"];
  Unix, [T"("; N Linux; T")"];
  

];;

let my_gram= Ios, small_rules ;; 
let frag = [ "I"; "U"; "S"];;
let convert0=
  (convert_grammar my_gram);;

(*
This example shows that our parser produces the first acceptable derivation 
and not neccessairily the most complete one. For example if we swapped 
the ordering of the rules:  
Ios, [N Windows; N Unix]; 
Ios, [T "I"; N Unix; T "S"];
then our parser would produce a  derivation for all of the  fragment [ "I"; "U"; "S"]
*)
let test_1= 
  parse_prefix (convert_grammar my_gram) accept_all frag =
Some ([
(Ios, [N Linux; N Unix]); 
(Linux, [T "I"]); 
(Unix, [T "U"])], 
["S"]);;

(*Implementing a different acceptor shows how a complete 
derivation can be attained. *)
let test_2=
  parse_prefix (convert_grammar my_gram) accept_empty_suffix frag =
 Some ([
(Ios, [T "I"; N Windows; T "S"]);
(Windows, [T "U"])],
 []);;
