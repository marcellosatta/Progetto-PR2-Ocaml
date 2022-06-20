
(* INTERPRETE OCAML CON ESTENSIONE CHE PERMETTE DI LAVORARE SU DIZIONARI *)

(* Marcello Satta - # matricola: 580495 - corso B *)

type ide = string;;
(* Tipo chiave, utilizzato nei dizionari *)
type key = string;;
(* Tipi espressione da valutare *)
type exp = Eint of int | 
		   Ebool of bool | 
		   Den of ide | 
		   Prod of exp * exp | 
		   Sum of exp * exp | 
		   Diff of exp * exp |
		   Eq of exp * exp | 
		   Minus of exp | 
	       IsZero of exp | 
	       Or of exp * exp | 
	       And of exp * exp | 
	       Not of exp |
		   Ifthenelse of exp * exp * exp | 
		   Let of ide * exp * exp | 
		   Fun of ide * exp | 
		   FunCall of exp * exp |
		   Letrec of ide * exp * exp |

		   (* Estensione richiesta, dizionari: *) 
		   Dictionary of dict | 
		   Insert of exp * key * exp | 
		   Delete of exp * key | 
		   HasKey of exp * key |
		   Iterate of exp * exp | 
		   Fold of exp * exp | 
		   Filter of (key list) * exp

		   (* definizione ricorsiva *)
and dict = Empty | 
		   Item of key * exp * dict;;

(* ambiente polimorfo *)
type 't env = ide -> 't;;
let emptyenv (v : 't) = function x -> v;;
let applyenv (r : 't env) (i : ide) = r i;;
let bind (r : 't env) (i : ide) (v : 't) = function x -> if x = i then v else applyenv r x;;

(* tipi esprimibili *)
type evT = Int of int | 
		   Bool of bool | 
		   Unbound | 
		   FunVal of evFun | 
		   RecFunVal of ide * evFun | 

		   (* Tipo esprimibile dei dizionari, rappresentati come una lista 
		   	di coppie chiave (tipo key) e valore (tipo esprimibile) *)
		   DictVal of (key * evT) list
and evFun = ide * exp * evT env

(* Run Time Support *)

(* Type checking int e bool *)
let typecheck (s : string) (v : evT) : bool = match s with
	"int" -> (match v with
		Int(_) -> true |
		_ -> false) |
	"bool" -> (match v with
		Bool(_) -> true |
		_ -> false) |
	_ -> failwith("not a valid type");;

(* Type checking dizionari (DictVal) *)
let rec dictTypeCheck (s: string) (d: (key * evT) list) : bool = 
		(match d with 
			[] -> true |
			(k1, v1)::tl -> if isIn tl k1 
								then failwith("Error: duplicate key!")
				    		else 
							   	(match s with 
									"int" -> (match v1 with
										Int(_) -> dictTypeCheck s tl |
										_ -> false) |
									"bool" -> (match v1 with 
										Bool(_) -> dictTypeCheck s tl |
										_ -> false) |
									_ -> failwith("Type error (string s does not represent a type)")) |
			_ -> failwith("Error: d does not represent a Dictionary!"));;


(*funzioni primitive*)
let prod x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Int(n*u))
	else failwith("Type error");;

let sum x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Int(n+u))
	else failwith("Type error");;

let diff x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Int(n-u))
	else failwith("Type error");;

let eq x y = if (typecheck "int" x) && (typecheck "int" y)
	then (match (x,y) with
		(Int(n),Int(u)) -> Bool(n=u))
	else failwith("Type error");;

let minus x = if (typecheck "int" x) 
	then (match x with
	   	Int(n) -> Int(-n))
	else failwith("Type error");;

let iszero x = if (typecheck "int" x)
	then (match x with
		Int(n) -> Bool(n=0))
	else failwith("Type error");;

let vel x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then (match (x,y) with
		(Bool(b),Bool(e)) -> (Bool(b||e)))
	else failwith("Type error");;

let et x y = if (typecheck "bool" x) && (typecheck "bool" y)
	then (match (x,y) with
		(Bool(b),Bool(e)) -> Bool(b&&e))
	else failwith("Type error");;

let non x = if (typecheck "bool" x)
	then (match x with
		Bool(true) -> Bool(false) |
		Bool(false) -> Bool(true))
	else failwith("Type error");;

(* Funzioni ausiliarie *)

(* Controlla se dato un dizionario (lista di coppie chiave-valore)
e una chiave, essa è presente all'interno del dizionario *)
let rec isIn lst k = match lst with 
	[] -> false |
	(k1, v1)::tl -> if k1 = k then true else isIn tl k |
	_ -> failwith("Function isIn not called on a Dictionary");;

(* Controlla se data una lista di chiavi, la chiave k passata come parametro
è presente nella lista (utilizzata nell'operazione primitiva Filter) *)
let rec isInAux lst k = match lst with 
	[] -> false |
	k1::tl -> if k1 = k then true else isInAux tl k |
	_ -> failwith("Function isInAux not called on a list of keys (string)");;

(* Rimuove la chiave k passata come parametro dal dizionario rappresentato dalla lista lst
se essa è presente (utilizzata nell'operazione primitiva Delete) *)
let rec removeKey lst k = match lst with 
	[] -> [] |
	(k1, v1)::tl -> if k1 = k then tl else (k1, v1)::(removeKey tl k) |
	_ -> failwith("Function removeKey not called on a Dictionary");;

(* Interprete *)
let rec eval (e : exp) (r : evT env) : evT = match e with
	Eint n -> Int n |
	Ebool b -> Bool b |
	IsZero a -> iszero (eval a r) |
	Den i -> applyenv r i |
	Eq(a, b) -> eq (eval a r) (eval b r) |
	Prod(a, b) -> prod (eval a r) (eval b r) |
	Sum(a, b) -> sum (eval a r) (eval b r) |
	Diff(a, b) -> diff (eval a r) (eval b r) |
	Minus a -> minus (eval a r) |
	And(a, b) -> et (eval a r) (eval b r) |
	Or(a, b) -> vel (eval a r) (eval b r) |
	Not a -> non (eval a r) |
	Ifthenelse(a, b, c) -> 
		let g = (eval a r) in
			if (typecheck "bool" g) 
				then (if g = Bool(true) then (eval b r) else (eval c r))
				else failwith ("nonboolean guard") |
	Let(i, e1, e2) -> eval e2 (bind r i (eval e1 r)) |
	Fun(i, a) -> FunVal(i, a, r) |
	FunCall(f, eArg) -> 
		let fClosure = (eval f r) in
			(match fClosure with
				FunVal(arg, fBody, fDecEnv) -> 
					eval fBody (bind fDecEnv arg (eval eArg r)) |
				RecFunVal(g, (arg, fBody, fDecEnv)) -> 
					let aVal = (eval eArg r) in
						let rEnv = (bind fDecEnv g fClosure) in
							let aEnv = (bind rEnv arg aVal) in
								eval fBody aEnv |
				_ -> failwith("non functional value")) |
        Letrec(f, funDef, letBody) ->
        		(match funDef with
            		Fun(i, fBody) -> let r1 = (bind r f (RecFunVal(f, (i, fBody, r)))) in
                         			                eval letBody r1 |
            		_ -> failwith("non functional def")) |

    (* ESTENSIONE DIZIONARI *)

    (* Costruttore: valuta l'espresione che rappresenta un dizionario tramite la funzione ausiliaria evalList, controlla
    che il dizionario sia omogeneo (i valori presenti all'interno siano dello stesso tipo) e che non ci siano chiavi duplicate *)

    Dictionary(d) -> (let lstV = evalList d r in (match lstV with
    												[] -> DictVal(lstV) |
    												(k1, v1)::tl -> (match v1 with
    																	Int(_) -> if (dictTypeCheck "int" lstV) then DictVal(lstV) 
    																			  else failwith("Type error") |
    																	Bool(_) -> if (dictTypeCheck "bool" lstV) then DictVal(lstV)
    																			   else failwith("Type error") |
    																	_ -> failwith("Type error")) | 
    												_ -> failwith("Type error"))) |
    
    (* Insert: inserisce all'interno del dizionario una coppia chiave-valore se vengono rispettati i controlli di tipo (il tipo 
    del valore da inserire corrisponde al tipo dei valori del dizionario) e se la chiave non è già presente all'interno del dizionario *)
    Insert(d, key, value) -> (match eval d r with
   								DictVal(lstV) -> (match lstV with 
    								    			[] -> DictVal((key, (eval value r))::lstV) |
    												(k1, v1)::tl -> (match v1 with 
					    												Int(_) -> (if isIn lstV key 
					    															then failwith("Key already present in the Dictionary!")
						    				   						   			   else 
								    												let valueV = eval value r in 
								    													if typecheck "int" valueV 
								    														then DictVal(lstV @ [(key, valueV)]) 
																						else 
				    																		failwith("Type error")) |
					    												Bool(_) -> (if isIn lstV key 
					    																then failwith("Key already present in the Dictionary!")
						    				    						   			else 
								    													let valueV = eval value r in 
								    														if typecheck "bool" valueV 
								    															then DictVal(lstV @ [(key, valueV)]) 
								    														else 
								   																failwith("Type error")) |
					    												_ -> failwith("Type error")) |
					    							_ -> failwith("Type error")) |
    							_ -> failwith("Error: d does not represent a Dictionary!")) |

    (* Delete: elimina dal dizionario la coppia chiave-valore identificata dalla chiave key se vengono rispettati i controlli di tipo
    e se la chiave è presente all'interno del dizionario *)
    Delete(d, key) -> (match eval d r with 
    					DictVal(lstV) -> (match lstV with 
    						    			[] -> failwith("No such key present in the Dictionary!") |
    										(k1, v1)::tl -> (match v1 with 
					    										Int(_) -> (if isIn lstV key 
																			then let newLstV = removeKey lstV key in DictVal(newLstV)
					    				    						 	   else 
					    				    					 			failwith("No such key present in the Dictionary!")) |
					    										Bool(_) -> (if isIn lstV key 
					   															then let newLstV = removeKey lstV key in DictVal(newLstV)
				    											 		    else 
					    				    						 			failwith("No such key present in the Dictionary!")) |
					    										_ -> failwith("Type error")) |
					    					_ -> failwith("Type error")) |
    					_ -> failwith("Error: d does not represent a Dictionary!")) |

    (* HasKey: controlla se la chiave key è presente all'interno del dizionario *)
    HasKey(d, key) -> (match eval d r with 
    					DictVal(lstV) -> (match lstV with 
    										[] -> Bool false |
    										(k1, v1)::tl -> (match v1 with 
					    										Int(_) -> (if isIn lstV key 
																			then Bool true
					    				    						 	   else 
					    				    					 			Bool false) |
					    										Bool(_) -> (if isIn lstV key 
					    														then Bool true
							     									        else 
					    				    						 			Bool false) |
					    										_ -> failwith("Type error")) |
					    					_ -> failwith("Type error")) |
    					_ -> failwith("Error: d does not represent a Dictionary!")) |

    (* Iterate: applica la funzione definita da f ad ogni valore presente all'interno del dizionario se vengono rispettati i controlli
    di tipo *)
    Iterate(f, d) -> (match d with 
    					Dictionary(eD) -> (let lstV = evalList eD r in (match lstV with
					    												[] -> DictVal(lstV) |
					    												(k1, v1)::tl -> (match v1 with
					    																	Int(_) -> if (dictTypeCheck "int" lstV) 
					    																				then DictVal(applyIterate f eD r) 
					    																			  else 
					    																			  	failwith("Type error") |
					    																	Bool(_) -> if (dictTypeCheck "bool" lstV) 
					    																				then DictVal(applyIterate f eD r)
					    																			   else 
					    																			   	failwith("Type error") |
					    																	_ -> failwith("Type error")) | 
					    												_ -> failwith("Type error"))) |
    					_ -> failwith("Error: d does not represent a Dictionary!")) |

    (* Fold: applica la funzione definita da f ad ogni valore presente all'interno del dizionario se vengono rispettati i controlli
    di tipo e calcola la somma di tutti i valori ottenuti (questa operazione primitiva accetta solo dizionari i cui valori sono 
    di tipo Int)*)
    Fold(f, d) -> (match d with 
    					Dictionary(eD) -> (let lstV = evalList eD r in (match lstV with
					    												[] -> Int(0) |
					    												(k1, v1)::tl -> (match v1 with
					    																	Int(_) -> if (dictTypeCheck "int" lstV) 
					    																				then (applyFold f eD (Int(0)) r)
					    																			  else 
					    																			  	failwith("Type error!") |
					    																	_ -> failwith("Type error")) | 
					    												_ -> failwith("Type error"))) |
    					_ -> failwith("Error: d does not represent a Dictionary!")) |

   	(* Filter: filtra il dizionario eliminando tutte le coppie chiave-valore le cui chiavi non sono presenti nella lista di chiavi passata 
   	come parametro *)
    Filter(keyList, d) -> (match eval d r with 
    						DictVal(lstV) -> (match lstV with 
    						    				[] -> DictVal(lstV) |
    											(k1, v1)::tl -> (match v1 with 
					    											Int(_) -> (let filteredList = filter keyList lstV in DictVal(filteredList)) |
					    											Bool(_) -> (let filteredList = filter keyList lstV in DictVal(filteredList)) |
					    											_ -> failwith("Type error")) |
					    						_ -> failwith("Type error")) |
    						_ -> failwith("Error: d does not represent a Dictionary!"))

    (* Funzioni ausiliarie *)

    (* evalList: valuta i valori presenti all'interno del dizionario *)
    and evalList (d: dict) (r: evT env): (key * evT) list = 
    		match d with
    			Empty -> [] |
    			Item(key, value, tl) -> let valueV = eval value r in
    										(key, valueV)::(evalList tl r) |
    			_ -> failwith("Error: d does not represent an element of a Dictionary!")

    (* applyIterate: valuta i valori presenti all'interno del dizionario applicandogli direttamente la funzione f passata
    come parametro *)
    and applyIterate (f: exp) (d: dict) (r: evT env): (key * evT) list = 
    		match d with 
				Empty -> [] |
				Item(key, value, tl) -> let valueV = eval (FunCall(f, value)) r in
											(key, valueV)::(applyIterate f tl r) |
				_ -> failwith("Error: d does not represent an element of a Dictionary!")

	(* applyFold: valuta i valori presenti all'interno del dizionario applicandogli direttamente la funzione f passata
    come parametro e li somma, restituendo l'accumulatore *)
	and applyFold (f: exp) (d: dict) (acc: evT) (r: evT env): evT = 
			match d with 
				Empty -> acc |
				Item(key, value, tl) -> let valueV = eval (FunCall(f, value)) r in 
											match (acc, valueV) with 
												(Int(x), Int(y)) -> (applyFold f tl (Int(x + y)) r) |
												(_, _) -> failwith("Type error") |
				_ -> failwith("Error: d does not represent an element of a Dictionary!")

    (* filter: rimuove dal dizionario tutte le coppie chiave-valore la cui chiave non è presente nella lista di chiavi passate come
    parametro *)
	and filter (keyList: key list) (d: (key * evT) list): (key * evT) list = 
			match d with 
				[] -> [] |
				(k1,v1)::tl -> if isInAux keyList k1 
								then (k1, v1)::(filter keyList tl) 
						  	   else 
						  	   	filter keyList tl |
				_ -> failwith("Error: d does not represent an element of a Dictionary!");;