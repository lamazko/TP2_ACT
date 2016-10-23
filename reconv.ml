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

let lecture_mot channel = 
let t = ref "" in
let caract = ref (input_char channel) in
while (!caract = ' ') || (!caract = '\n') do
	caract := input_char channel;
done;
while (!caract <> ' ') && (!caract <> '\n') do
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


let lecture_soustab channel =
let caract = ref (input_char channel) in
while !caract <> '|' do 
if !caract <> ';' then
Printf.printf("%c\n") !caract;
caract := input_char channel;
done;;


let lect channel = 
let caract = ref (input_char channel) in
caract := input_char channel;
caract := input_char channel;
while !caract <> ']' do
let str = ref "" in
while (!caract <> ';') && (!caract <> '|') do
str := !str ^ (string_of_char !caract);
caract := input_char channel;
done;
Printf.printf("%s \n") !str;
caract := input_char channel;
done;;



let _ = 
let lecture = open_in((Sys.argv.(1))) in
Printf.printf("P2 \n") ;
for i = 0 to 2 do
 (lecture_mot lecture);
done;
Printf.printf("%s ") (lecture_mot lecture);
for i = 0 to 3 do
(lecture_mot lecture);
done;
Printf.printf("%s ") (lecture_mot lecture);
for i = 0 to 3 do
lecture_mot lecture;
done;
Printf.printf("\n%s ") (Sys.argv.(2));
lect lecture;;



