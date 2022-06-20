(*--------------------------------------- TEST --------------------------------------------*)

(*--- COSTRUTTORE ---*)

(* INT *)

(* TEST: dizionario vuoto *)
eval (Dictionary(Empty)) (emptyenv Unbound);;

(* TEST: dizionario interpretato correttamente *)
eval (Dictionary(Item("a", Eint(1), Item("b", Eint(2), Item("c", Eint(3), Empty))))) (emptyenv Unbound);;

(* TEST: dizionario con chiave duplicata, errore *)
eval (Dictionary(Item("a", Eint(1), Item("a", Eint(2), Empty)))) (emptyenv Unbound);;

(* TEST: dizionario non omogeneo, errore*)
eval (Dictionary(Item("a", Eint(1), Item("b", Ebool(true), Item("c", Eint(3), Empty))))) (emptyenv Unbound);;

(* BOOL *)

(* TEST: dizionario vuoto *)
eval (Dictionary(Empty)) (emptyenv Unbound);;

(* TEST: dizionario interpretato correttamente *)
eval (Dictionary(Item("a", Ebool(true), Item("b", Ebool(false), Item("c", Ebool(true), Empty))))) (emptyenv Unbound);;

(* TEST: dizionario con chiave duplicata, errore *)
eval (Dictionary(Item("a", Ebool(true), Item("a", Ebool(false), Empty)))) (emptyenv Unbound);;

(* TEST: dizionario non omogeneo, errore*)
eval (Dictionary(Item("a", Ebool(true), Item("b", Eint(2), Item("c", Ebool(true), Empty))))) (emptyenv Unbound);;


(*--- INSERT ---*)

(* INT *)

(* TEST: Insert su dizionario vuoto *)
eval (Insert(Dictionary(Empty), "a", Eint(1))) (emptyenv Unbound);;

(* TEST: Insert interpretata correttamente *)
eval (Insert(Dictionary(Item("a", Eint(1), Item("b", Eint(2), Item("c", Eint(3), Empty)))), "d", Eint(4))) (emptyenv Unbound);;

(* TEST: Insert di chiave già presente nel dizionario *)
eval (Insert(Dictionary(Item("a", Eint(1), Item("b", Eint(2), Item("c", Eint(3), Empty)))), "a", Eint(4))) (emptyenv Unbound);;

(* TEST: Insert di valore il cui tipo non corrisponde a quello dei valori presenti nel dizionario, errore *)
eval (Insert(Dictionary(Item("a", Eint(1), Item("b", Eint(2), Item("c", Eint(3), Empty)))), "d", Ebool(true))) (emptyenv Unbound);;

(* TEST: Insert su dizionario non omogeneo, errore *)
eval (Insert(Dictionary(Item("a", Eint(1), Item("b", Ebool(false), Item("c", Eint(3), Empty)))), "d", Eint(4))) (emptyenv Unbound);;

(* BOOL *)

(* TEST: Insert su dizionario vuoto *)
eval (Insert(Dictionary(Empty), "a", Ebool(true))) (emptyenv Unbound);;

(* TEST: Insert interpretata correttamente *)
eval (Insert(Dictionary(Item("a", Ebool(true), Item("b", Ebool(false), Item("c", Ebool(true), Empty)))), "d", Ebool(false))) (emptyenv Unbound);;

(* TEST: Insert di chiave già presente nel dizionario *)
eval (Insert(Dictionary(Item("a", Ebool(true), Item("b", Ebool(false), Item("c", Ebool(true), Empty)))), "a", Ebool(false))) (emptyenv Unbound);;

(* TEST: Insert di valore il cui tipo non corrisponde a quello dei valori presenti nel dizionario, errore *)
eval (Insert(Dictionary(Item("a", Ebool(true), Item("b", Ebool(false), Item("c", Ebool(true), Empty)))), "d", Eint(4))) (emptyenv Unbound);;

(* TEST: Insert su dizionario non omogeneo, errore *)
eval (Insert(Dictionary(Item("a", Ebool(true), Item("b", Eint(2), Item("c", Ebool(true), Empty)))), "d", Ebool(false))) (emptyenv Unbound);;



(*--- DELETE ---*)

(* INT *)

(* TEST: Delete su dizionario vuoto, errore *)
eval (Delete(Dictionary(Empty), "a")) (emptyenv Unbound);;

(* TEST: Delete su chiave non presente, errore *)
eval (Delete(Dictionary(Item("a", Eint(1), Item("b", Eint(2), Item("c", Eint(3), Empty)))), "d")) (emptyenv Unbound);;

(* TEST: Delete interpretata correttamente *)
eval (Delete(Dictionary(Item("a", Eint(1), Item("b", Eint(2), Item("c", Eint(3), Empty)))), "a")) (emptyenv Unbound);;

(* TEST: Delete su dizionario non omogeneo, errore*)
eval (Delete(Dictionary(Item("a", Eint(1), Item("b", Ebool(false), Item("c", Eint(3), Empty)))), "a")) (emptyenv Unbound);;

(* BOOL *)

(* TEST: Delete su dizionario vuoto, errore *)
eval (Delete(Dictionary(Empty), "a")) (emptyenv Unbound);;

(* TEST: Delete su chiave non presente, errore *)
eval (Delete(Dictionary(Item("a", Ebool(true), Item("b", Ebool(false), Item("c", Ebool(true), Empty)))), "d")) (emptyenv Unbound);;

(* TEST: Delete interpretata correttamente *)
eval (Delete(Dictionary(Item("a", Ebool(true), Item("b", Ebool(false), Item("c", Ebool(true), Empty)))), "a")) (emptyenv Unbound);;

(* TEST: Delete su dizionario non omogeneo, errore*)
eval (Delete(Dictionary(Item("a", Ebool(true), Item("b", Eint(2), Item("c", Ebool(true), Empty)))), "a")) (emptyenv Unbound);;


(*--- HASKEY ---*)

(* INT *)

(* TEST: HasKey su dizionario vuoto *)
eval (HasKey(Dictionary(Empty), "a")) (emptyenv Unbound);;

(* TEST: HasKey su chiave non presente *)
eval (HasKey(Dictionary(Item("a", Eint(1), Item("b", Eint(2), Item("c", Eint(3), Empty)))), "d")) (emptyenv Unbound);;

(* TEST: HasKey su chiave presente *)
eval (HasKey(Dictionary(Item("a", Eint(1), Item("b", Eint(2), Item("c", Eint(3), Empty)))), "a")) (emptyenv Unbound);;

(* TEST: HasKey su dizionario non omogeneo, errore *)
eval (HasKey(Dictionary(Item("a", Eint(1), Item("b", Ebool(false), Item("c", Eint(3), Empty)))), "a")) (emptyenv Unbound);;

(* BOOL *)

(* TEST: HasKey su dizionario vuoto *)
eval (HasKey(Dictionary(Empty), "a")) (emptyenv Unbound);;

(* TEST: HasKey su chiave non presente *)
eval (HasKey(Dictionary(Item("a", Ebool(true), Item("b", Ebool(false), Item("c", Ebool(true), Empty)))), "d")) (emptyenv Unbound);;

(* TEST: HasKey su chiave presente *)
eval (HasKey(Dictionary(Item("a", Ebool(true), Item("b", Ebool(false), Item("c", Ebool(true), Empty)))), "a")) (emptyenv Unbound);;

(* TEST: HasKey su dizionario non omogeneo, errore *)
eval (HasKey(Dictionary(Item("a", Ebool(true), Item("b", Eint(2), Item("c", Ebool(true), Empty)))), "a")) (emptyenv Unbound);;


(*--- ITERATE ---*)

(* INT *)

(* TEST: Iterate interpretata correttamente *)
eval (Iterate(Fun("x", Sum(Den "x", Eint 1)), (Dictionary(Item("a", Eint(1), Item("b", Eint(2), Item("c", Eint(3), Empty))))))) (emptyenv Unbound);;

(* TEST: Iterate su dizionario il cui tipo dei valori presenti non corrisponde al tipo della funzione passata come parametro, errore *)
eval (Iterate(Fun("x", Sum(Den "x", Eint 1)), (Dictionary(Item("a", Ebool(true), Item("b", Ebool(false), Item("c", Ebool(true), Empty))))))) (emptyenv Unbound);;

(* TEST: Iterate su dizionario non omogeneo, errore *)
eval (Iterate(Fun("x", Sum(Den "x", Eint 1)), (Dictionary(Item("a", Eint(1), Item("b", Ebool(false), Item("c", Eint(3), Empty))))))) (emptyenv Unbound);;

(* TEST: Iterate su dizionario vuoto *)
eval (Iterate(Fun("x", Sum(Den "x", Eint 1)), (Dictionary(Empty)))) (emptyenv Unbound);;

(* BOOL *)

(* TEST: Iterate interpretata correttamente *)
eval (Iterate(Fun("x", And(Den "x", Ebool true)), (Dictionary(Item("a", Ebool(true), Item("b", Ebool(false), Item("c", Ebool(true), Empty))))))) (emptyenv Unbound);;

(* TEST: Iterate su dizionario il cui tipo dei valori presenti non corrisponde al tipo della funzione passata come parametro, errore *)
eval (Iterate(Fun("x", And(Den "x", Ebool true)), (Dictionary(Item("a", Eint(1), Item("b", Eint(2), Item("c", Eint(3), Empty))))))) (emptyenv Unbound);;

(* TEST: Iterate su dizionario non omogeneo, errore *)
eval (Iterate(Fun("x", And(Den "x", Ebool true)), (Dictionary(Item("a", Ebool(true), Item("b", Eint(2), Item("c", Ebool(true), Empty))))))) (emptyenv Unbound);;

(* TEST: Iterate su dizionario vuoto *)
eval (Iterate(Fun("x", And(Den "x", Ebool true)), (Dictionary(Empty)))) (emptyenv Unbound);;


(*--- FOLD ---*)

(* INT *)

(* TEST: Fold interpretata correttamente *)
eval (Fold(Fun("x", Sum(Den "x", Eint 1)), (Dictionary(Item("a", Eint(1), Item("b", Eint(2), Item("c", Eint(3), Empty))))))) (emptyenv Unbound);;

(* TEST: Fold su dizionario i cui valori sono di tipo Bool, errore *)
eval (Fold(Fun("x", Sum(Den "x", Eint 1)), (Dictionary(Item("a", Ebool(true), Item("b", Ebool(false), Item("c", Ebool(true), Empty))))))) (emptyenv Unbound);;

(* TEST: Fold su dizionario non omogeneo, errore *)
eval (Fold(Fun("x", Sum(Den "x", Eint 1)), (Dictionary(Item("a", Eint(1), Item("b", Ebool(false), Item("c", Eint(3), Empty))))))) (emptyenv Unbound);;

(* TEST: Fold su dizionario vuoto *)
eval (Fold(Fun("x", Sum(Den "x", Eint 1)), (Dictionary(Empty)))) (emptyenv Unbound);;

(* TEST: Fold con funzione operante su valori di tipo Bool, errore *)
eval (Fold(Fun("x", And(Den "x", Ebool true)), (Dictionary(Item("a", Eint(1), Item("b", Eint(2), Item("c", Eint(3), Empty))))))) (emptyenv Unbound);;

(*--- FILTER ---*)

(* INT *)

(* TEST: Filter interpretata correttamente *)
eval (Filter(["a"; "b"], (Dictionary(Item("a", Eint(1), Item("b", Eint(2), Item("c", Eint(3), Empty))))))) (emptyenv Unbound);;

(* TEST: Filter con lista di chiavi vuota *)
eval (Filter([], (Dictionary(Item("a", Eint(1), Item("b", Eint(2), Item("c", Eint(3), Empty))))))) (emptyenv Unbound);;

(* TEST: Filter su dizionario non omogeneo, errore *)
eval (Filter(["a"; "b"], (Dictionary(Item("a", Eint(1), Item("b", Ebool(false), Item("c", Eint(3), Empty))))))) (emptyenv Unbound);;

(* TEST: Filter su dizionario vuoto *)
eval (Filter(["a"; "b"], (Dictionary(Empty)))) (emptyenv Unbound);;

(* BOOL *)

(* TEST: Filter interpretata correttamente *)
eval (Filter(["a"; "b"], (Dictionary(Item("a", Ebool(true), Item("b", Ebool(false), Item("c", Ebool(true), Empty))))))) (emptyenv Unbound);;

(* TEST: Filter con lista di chiavi vuota *)
eval (Filter([], (Dictionary(Item("a", Ebool(true), Item("b", Ebool(false), Item("c", Ebool(true), Empty))))))) (emptyenv Unbound);;

(* TEST: Filter su dizionario non omogeneo, errore *)
eval (Filter(["a"; "b"], (Dictionary(Item("a", Ebool(true), Item("b", Eint(2), Item("c", Ebool(true), Empty))))))) (emptyenv Unbound);;

(* TEST: Filter su dizionario vuoto *)
eval (Filter(["a"; "b"], (Dictionary(Empty)))) (emptyenv Unbound);;





