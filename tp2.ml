

let t = [|22;28;33;55;128;128;128;255;255|];;


let meilleur_gris tableau premier dernier = 
	let somme_poid = ref 0
	and somme_poid_gris = ref 0 in
		for i = premier to dernier do
		somme_poid_gris := !somme_poid_gris + tableau.(i);
		somme_poid := !somme_poid + 1;
		done;
	!somme_poid_gris / !somme_poid;;

let au_carre valeur = 
	valeur * valeur;;


let _ = 
	let c = ref 0 in
	c := meilleur_gris t 0 8;
	Printf.printf("%d\n") !c;;


(*
------------------------------------------------------------------------
			Partie Test

*)


let t = [|22;28;33;55;128;128;128;255;255|];;

let t1 = [|0;0;0;0;0;20;20;20;20;20|];;

let t2 = [|100;132;164;164;180|];;

let t3 = [|255;255;255;255;255;255;255;255;255;255|];;


let test_meilleur_gris1 = 
	10 == meilleur_gris t1 0 9;;

let test_meilleur_gris2 = 
	148 == meilleur_gris t2 0 4;;

let test_meilleur_gris3 = 
	255 == meilleur_gris t3 0 9;;






