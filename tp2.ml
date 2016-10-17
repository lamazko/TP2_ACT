

let t1bis = [|(0,5);(20,5);(100,1);(132,1);(164,2);(180,1);(255,10)|];;


let meilleur_gris tableau premier dernier = 
	let somme_poid = ref 0
	and somme_poid_gris = ref 0 in
		for i = premier to dernier do
		somme_poid_gris := !somme_poid_gris + (fst(tableau.(i)))*(snd(tableau.(i)));
		somme_poid := !somme_poid + snd(tableau.(i));
		done;
	Printf.printf("%d\n") !somme_poid;
	!somme_poid_gris / !somme_poid;;

let au_carre valeur = 
	valeur * valeur;;

let distance_min tableau deb fin = 
	let moyenne = (meilleur_gris tableau deb fin)
	and score = ref 0 in
	for i = deb to fin do
	score := !score + (au_carre ((fst(tableau.(i)) - moyenne))) * snd(tableau.(i)); 
	done;
	!score;;


let min a b = 
if a > b then
	b 
	else 
a;;


let rec algo palette k debut fin= 
if k = 1 then
	distance_min palette debut fin
else 	
	begin
	let res = ref (-1) in
	for i = 0 to (fin-k-debut+1) do
		if !res = -1 then
		res := (distance_min palette debut (debut+i)) + (algo palette (k-1) (debut+i+1) fin)
		else 
		res := min (!res) (distance_min palette debut (debut+i)) + (algo palette (k-1) (debut+i+1) fin);
	done;
	!res;
	end;;





let _ = 
	let c = ref 0 in
	c := distance_min t1bis 0 1;
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
	0 == distance_min t1bis 6 6;;


(*
let test_meilleur_gris2 = 
	148 == meilleur_gris t2 0 4;;

let test_meilleur_gris3 = 
	255 == meilleur_gris t3 0 9;;


let test_distance_min = 
	let t = ref (distance_min t1 0 9) in
	t := !t + (distance_min t2 0 4);
	t := !t + (distance_min t3 0 9);
	5096 == !t;;
*)



