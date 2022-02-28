open Lambda_sigma;;


let beta = function App(Lambda(a),b) -> subst(a,inst(b,id));;


let rec lam_to_db t = vartodb(t);;
