type atom = private int
;;

external from : atom -> int = "%identity"
;;

val make : string -> atom
;;

val name : atom -> string
;;

val compare : atom -> atom -> int
;;

external eq : atom -> atom -> bool = "%eq"
;;
