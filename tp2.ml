
open List;;
open Baboon_gray;;

let cpt = ref 0;;



let cmp a b =
  cpt := !cpt + 1;
  if a = b then
    0
  else if a < b then
    -1
  else
    1 ;; 


let test12345 = [|0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;
0;0;0;0;0;0;3;3;3;3;0;0;7;7;7;7;0;0;11;11;
11;11;0;0;15;15;15;15;0;0;3;0;0;0;0;0;7;0;0;0;
0;0;11;0;0;0;0;0;15;0;0;15;0;0;3;3;3;0;0;0;
7;7;7;0;0;0;11;11;11;0;0;0;15;15;15;15;0;0;3;0;
0;0;0;0;7;0;0;0;0;0;11;0;0;0;0;0;15;0;0;0;
0;0;3;0;0;0;0;0;7;7;7;7;0;0;11;11;11;11;0;0;
15;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;
0;0;0;0;0;0;0;0;0|];;

let t0bis = [|(0, 96); (3, 10); (7, 13); (11, 13); (15, 12)|];;

let t1bis = [|(0,5);(20,5);(100,1);(132,1);(164,2);(180,1);(255,10)|];;

let t2bis = [|(0,3);(10,2);(30,4);(40,1);(100,1)|];;

let meilleur_gris tableau premier dernier = 
	let somme_poid = ref 0
	and somme_poid_gris = ref 0 in
		for i = premier to dernier do
		somme_poid_gris := !somme_poid_gris + (fst(tableau.(i)))*(snd(tableau.(i)));
		somme_poid := !somme_poid + snd(tableau.(i));
		done;
	!somme_poid_gris / !somme_poid;;

let poid tableau premier dernier =
let somme_poid = ref 0 in
	for i = premier to dernier do
		somme_poid := !somme_poid + snd(tableau.(i));
	done;
	!somme_poid;;

let au_carre valeur = 
	valeur * valeur;;

let distance_min tableau deb fin = 
	cpt := !cpt+1;
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
		res := min (!res) ((distance_min palette debut (debut+i)) + (algo palette (k-1) (debut+i+1) fin));
	done;
	!res;
	end;;


let algo1cpt palette k debut fin = 
	cpt := 0;
	algo palette k debut fin;;




let rec algo2 palette tableau k debut fin = 
if k = 1 then
	distance_min palette debut fin
else 	
	begin
	let res = ref (-1) in
	for i = 0 to (fin-k-debut+1) do
		if !res = -1 then
		res := tableau.(debut).(debut+i) + (algo2 palette tableau (k-1) (debut+i+1) fin)
		else 
		res := min (!res) (tableau.(debut).(debut+i) + (algo2 palette tableau (k-1) (debut+i+1) fin));
	done;
	!res;
	end;;




let algo2cpt palette k debut fin = 
	cpt := 0;
	let t = Array.make_matrix (fin+1) (fin+1) 0 in
	for i = 0 to fin-1 do
		for j = 0 to fin do
		if i < j then 
		begin
		t.(i).(j) <- (distance_min palette i j); 
		end;
		done;
	done;
	algo2 palette t k debut fin;;





let rec algo3 palette tableau k debut fin l= 
if k = 1 then
	(distance_min palette debut fin, (fin :: debut:: l))
else 	
	begin
	let res = ref ((-1),l) in
	for i = 0 to (fin-k-debut+1) do
		let newl = ((debut+i) :: debut ::l) in
		if fst(!res) = -1 then
		res := (tableau.(debut).(debut+i) + fst((algo3 palette tableau (k-1) (debut+i+1) fin newl)),
		append newl (snd((algo3 palette tableau (k-1) (debut+i+1) fin newl))))
		else 
		if (min (fst(!res)) (tableau.(debut).(debut+i) + (fst(algo3 palette tableau (k-1) (debut+i+1) fin l))) <> (fst(!res))) then
			res :=  ((tableau.(debut).(debut+i) + fst((algo3 palette tableau (k-1) (debut+i+1) fin newl))),
			append newl (snd((algo3 palette tableau (k-1) (debut+i+1) fin newl))));
	done;
	!res;
	end;;






let al palette k debut fin = 
	let t = Array.make_matrix (fin+1) (fin+1) 0 and
	l = [] in
	for i = 0 to fin-1 do
		for j = 0 to fin do
		if i < j then 
		begin
		t.(i).(j) <- (distance_min palette i j); 
		end;
		done;
	done;
	algo3 palette t k debut fin l;;





let intervalle l k = 
let l2 = (rev l) in
let t = Array.make k (0,0) in
let i = ref 0 in
while !i < ((2*k)) do 
t.(!i/2) <- (nth l2 !i, nth l2 (!i+1));
i := !i+2;
done;
t;;


let result palette k debut fin = 
	let res = (al palette k debut fin) in
	(fst(res),intervalle (snd(res)) k);;








let distance tableau tabindice = 
let t = Array.make (Array.length tabindice) 0 in
for i = 0 to (Array.length tabindice)-1 do
t.(i) <- meilleur_gris tableau (fst(tabindice.(i))) (snd(tabindice.(i)));
done;
t;;


let nouvelle_valeur tableau valeur = 
	let i = ref 0 and
	rep = ref false in
	while !rep = false do
	if (valeur >= fst(snd(tableau.(!i)))) && (valeur <= snd(snd(tableau.(!i)))) then
	rep := true;
	i := !i+1;
	done;
	fst(tableau.(!i-1));;
	
		
	

let nouvelle_palette palette k debut fin = 
let tabinterv = (snd(result palette k debut fin)) in
	let newpalet = (Array.make k (0,(0,0))) in
	for i = 0 to k-1 do
	newpalet.(i) <- (meilleur_gris palette (fst(tabinterv.(i))) (snd(tabinterv.(i))),
	((fst(palette.(fst(tabinterv.(i))))), (fst(palette.(snd(tabinterv.(i)))))));
	done;
	newpalet;;




let tableau_to_palette tableau size max = 
 let a = ref 0 
 and palette = Array.make (max+1) 0 in
	for i = 0 to size-1 do 
        begin
	 a := tableau.(i);
         palette.(!a) <- palette.(!a)+ 1;
        end;
	done;
 palette;;


	


let palette_reduction palette =
 let lpalette = ref ([]) in 
 for i = 0 to (Array.length palette-1) do
  if (palette.(i) <> 0) then
   let color = [(i,palette.(i))] in
   lpalette :=  append !lpalette color;
  done;
 let size = length !lpalette in
 let newpal = Array.make size (0,0) in
 for j = 0 to size-1 do
 newpal.(j) <- (nth (!lpalette) j);
 done;
 newpal;;


let algofinal tableaupixel tailletab couleurmax k = 	
	let palette = tableau_to_palette tableaupixel tailletab couleurmax in
	let palette_reduite = palette_reduction palette in
	let rep = (nouvelle_palette palette_reduite k 0 ((Array.length palette_reduite)-1)) in
	let t = Array.make (Array.length tableaupixel) 0 in
	for i = 0 to (Array.length tableaupixel) -1 do
		t.(i) <- nouvelle_valeur rep (tableaupixel.(i));
	done;
	t;;





let _ = 
	let t = algofinal baboon_gray (Array.length baboon_gray) 255 3 in
        Printf.printf("let longeurtab = 512 ;;\n");
        Printf.printf("let hauteurtab = 512 ;;\n\n");
	Printf.printf("let t = [|");
	for i = 0 to Array.length t-2 do
	Printf.printf("%d;") t.(i);
	done;
	Printf.printf("%d|];;") t.((Array.length t)-1);;	


	


(*
------------------------------------------------------------------------
			Partie Test

*)


(*let t = [|22;28;33;55;128;128;128;255;255|];; *)

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


(*
------------------------------------------------------------------------
			Partie Complexite

*)



Random.self_init();;



let tableau_aleatoire n taille =
  let f i =
    (Random.int taille)
  in
  Array.init n f;;

let palette_aleatoire n taille = 
	let t = tableau_aleatoire n (taille+1) in
	let t1 = tableau_to_palette t n taille in
	palette_reduction t1;;

let echanger tableau i j=
  let memoire=tableau.(i) in
  tableau.(i)<-tableau.(j);
  tableau.(j)<-memoire;;

let partition tableau deb fin=
  let pivot=tableau.(deb) and compteur=ref(deb) in
  for i=(deb+1) to fin do
    if(tableau.(i)<pivot) then
      begin
        compteur:=(!compteur)+1;
        echanger tableau i (!compteur);
      end;
  done;
  echanger tableau (!compteur) deb;
  (!compteur);;
	


let tri_rapide tableau=
  let rec tri_rapide_bis tableau deb fin=
    if deb<fin then
      begin
        let position=(partition tableau deb fin) in
        tri_rapide_bis tableau deb (position-1);
        tri_rapide_bis tableau (position+1) fin;
      end;
  in
    tri_rapide_bis tableau 0 (Array.length(tableau)-1);;	



let complexite algo n taille = 
	let proba = ref 0 in
		for i = 1 to 25 do
			let t = palette_aleatoire n taille in
				algo t 4 0 ((Array.length t)-1); 
				proba := !proba + !cpt; 
		done;
	proba := !proba/25;
	!proba;;


(*

let _ =

		for j = 1 to 7 do
				Printf.printf("%3d %8d %8d \n") (j*10) (complexite algo1cpt (j*10) 255)
				 (complexite algo2cpt (j*10) 255);
		done;;


*)

