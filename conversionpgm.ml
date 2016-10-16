open String;;

let string_of_char a = 
 String.make 1 a;;


let lecture_couleur channel = 
let t = ref "" in
let caract = ref (input_char channel) in
if !caract = ' ' then
begin
while !caract = ' ' do
	caract := input_char channel;
done;
end;
while !caract <> ' ' do
	t :=  !t ^ (string_of_char !caract);
	caract := input_char channel;
done;
 !t;;


let lecture_last channel = 
let t = ref "" in
let caract = ref (input_char channel) in
while !caract <> '\n' do
if !caract <> ' ' then
	t :=  !t ^ (string_of_char !caract);
	caract := input_char channel;
done;
 !t;;


let _ = 
let lecture = open_in((Sys.argv.(1))) in
input_line lecture;
let longeur = lecture_couleur lecture in
Printf.printf("let longeurtab = %d;;\n") (int_of_string longeur);
let hauteur = lecture_last lecture in
Printf.printf("let hauteurtab = %d;;\n") (int_of_string hauteur);
input_line lecture;
Printf.printf("\n");
let tableauname = sub Sys.argv.(1) 0 ((length Sys.argv.(1))-4) in
Printf.printf("let %s = [|") tableauname;
for i = 1 to (int_of_string hauteur)-1 do
Printf.printf("[|");
for j = 1 to (int_of_string longeur)-1 do
let caract = lecture_couleur lecture in
Printf.printf("%s;") caract;
done;
let caract2 = lecture_last lecture in
Printf.printf("%s|]\n") caract2;
done;
Printf.printf("|];;");;



