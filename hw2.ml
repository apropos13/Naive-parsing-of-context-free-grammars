(*Hw 2*)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(*------------------Convert grammar--------------*)

(*Helper functions*)


(*
nt_symbol can be each terminal symbol of the grammar
The variable "rules" contains the rules in the old style of gram.
 
Note that a much nicer (visually) implementation of this function would be to reverse 
the two variables, but this is not possible since in the convert_grammar function
get_rules is called with the second argument being nt_symbol
*)

(*Production function*)
let rec get_rules rules nt_symbol= match rules with
  |[]->[] 
  |hd :: tl -> match hd with 
    |(a,b)-> if (a=nt_symbol) 
             then  b :: (get_rules tl nt_symbol ) 
             else get_rules  tl nt_symbol   ;;

let convert_grammar = function
  |(sp,old_rules)-> ( (sp), (get_rules old_rules));;


(* PARSER AND HELPER FUNCTIONS *)

(*Examine the rhs of a rule; if the rhs contains a temrinal check if                                                                                                                                                                                                         
that terminal is equal to the hd of the frag. If it is  proceed to                                                                                                                                                                                                           
the next element of the rhs. If you find a non terminal symbol in a rule                                                                                                                                                                                                     
then call parser wich in turn calls examine_rule*)

let rec examine_rhs p_f rhs accept derivation frag = match rhs with 
  |[]->accept derivation frag (*Try the acceptor only at the top level*)
  |rhs_h :: rhs_t -> ( match rhs_h with
    |N nt -> init_parser p_f nt (examine_rhs p_f rhs_t accept ) derivation  frag 
    |T t-> (match frag with
      |f_h::f_t -> if t=f_h then examine_rhs p_f rhs_t accept derivation f_t else None
      |[]->None (*Helps in the blind alley situation *)
    )
  )

(*Examine each rule for the "nt_symbol" variable . To examine each rule call examine_rhs.                                                                                                                                                                                    
In the case of a success return the rule otherwise proceed until you find the empty list in                                                                                                                                                                                  
which case return none*) 
and examine_rules p_f symbol rhs accept derivation frag = match rhs with 
  |[]->None 
  |rhs_h::rhs_t -> ( match examine_rhs p_f rhs_h accept  (derivation@[(symbol, rhs_h)]) frag  with 
    |Some (d,s)-> Some (d,s)
    |None -> examine_rules p_f symbol rhs_t accept derivation frag 
  )


and init_parser p_f symbol accept derivation frag = 
if ( p_f symbol ) <> []  
then examine_rules p_f symbol (p_f symbol) accept derivation frag 
else None ;;


let parse_prefix p_f accept frag = match p_f with 
  |(start, p_f)->init_parser p_f start accept [] frag ;;
		  



