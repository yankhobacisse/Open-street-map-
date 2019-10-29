#directory "+xml-light";;
#load "xml-light.cma";;
#load "graphics.cma";; 
open Graphics;;
Random.self_init;;
open Xml;;
let a=parse_file "06_new_york_downtown.osm";;
let p= parse_file "09_palm_jumeirah_detail.osm";;
let k=parse_file "10_dubai_detail.osm";;
let l=parse_file "test.osm";;


type expr=
  |Bounds of (string*string) list
  |Tag of (string*string) list
  |Nd of (string*string) list
  |Way of ((string*string) list * expr list)
  |Node of ((string*string) list * expr list)
  |Osm of expr list
;;

Tag [("k","k'") ];;
 (* transformation xml en expr*)
exception Incorrecte;;
(* |  [ ]  *)
(* Copy des element   *)
let rec copy_tag=function
  |[]->[]
  |[Element ("tag",l',[])]->[Element ("tag",l',[])] 
  |Element ("tag",l',[])::l -> Element ("tag",l',[]):: copy_tag l
 (* |Element (_,l',l'')::l ->copy_tag l''*)
  |_::reste-> copy_tag reste   
;;
let rec copy_nd=function
  |[]->[]
  |[Element ("nd",l',[])]->[Element ("nd",l',[])] 
  |Element ("nd",l',[])::l -> Element ("nd",l',[]):: copy_nd l
  (*|Element (_,l',l'')::l ->copy_nd l''*)
  |_::reste->copy_nd reste
;;
let rec copy_bounds=function
  |[]->[]
  |[Element ("bounds",l',[])]->[Element ("bounds",l',[])] 
  |Element ("bounds",l',[])::l -> Element ("bounds",l',[]):: copy_bounds l
     (*|Element (_,l',l'')::l ->copy_nd l''*)
  |_::reste->copy_bounds reste
;;

(*filtrage de principaux elements  *)
   
let rec filtrage_node =function
  |[]->[]
  |Element("node",l,t)::reste-> let a=copy_tag t in
      (Element("node",l,a))::filtrage_node reste
  |Element(_,l,t)::[]-> filtrage_node t	
  |_::reste-> filtrage_node reste
;; 
let rec filtrage_way=function
  |[]->[]
  |Element("way",l,t)::reste-> let a=(copy_tag t) and b=(copy_nd t) in
      (Element("way",l,a@b))::filtrage_way reste
  |Element(_,l,t)::[]-> filtrage_way t	
  |_::reste-> filtrage_way reste
;;
let rec filtrage_way_sans_nd=function
  |[]->[]
  |Element("way",l,t)::reste-> let a=(copy_tag t) in
      (Element("way",l,a))::filtrage_way reste
  |Element(_,l,t)::[]-> filtrage_way t	
  |_::reste-> filtrage_way reste
;;

let  tag_of_xml=function
  |Element("tag ",l, l')->Tag  l 
  |Element(_,l, l')->Tag  l 
  |_->raise Incorrecte
;;
let  nd_of_xml x=
  match x with
    |Element("nd", l,[ ])->Nd l
    |Element(_, l,l' )->Nd l
    |_->raise Incorrecte
;;

(* convertion de xml vers notre type que nous avons defini*)
 
let rec bounds_of_xml=function
  |Element("bounds ",l, l')->Bounds  l 
  |Element(_,l, l')->Bounds  l 
  |_->raise Incorrecte
;;
let rec node_of_xml=function
  |[]->[]
  |Element("node", attrib,l)::reste-> (let a=copy_tag l in Node (attrib, List.map tag_of_xml a) ):: node_of_xml reste
  |Element(_,l,t)::[]-> node_of_xml t
  |_::reste-> node_of_xml reste
;;
let rec way_of_xml=function
  |[]->[]
  |Element("way", attrib,l)::reste-> (let a= (copy_tag l) and b= (copy_nd l) in 
					Way (attrib,(List.map tag_of_xml a)@(List.map nd_of_xml b) )):: way_of_xml reste
  |Element(_,l,t)::[]-> way_of_xml t
  |_::reste-> way_of_xml reste
;;
let rec wayref_of_xml=function
  |[]->[]
  |Element("way", attrib,l)::reste-> (let b= (copy_nd l) in 
					Way (attrib,(List.map nd_of_xml b) )):: wayref_of_xml reste
  |Element(_,l,t)::[]-> wayref_of_xml t
  |_::reste-> wayref_of_xml reste
;;

let rec waytag_of_xml=function
  |[]->[]
  |Element("way", attrib,l)::reste-> (let a= (copy_tag l)   in 
					Way (attrib,(List.map tag_of_xml a) )):: waytag_of_xml reste
  |Element(_,l,t)::[]-> waytag_of_xml t
  |_::reste-> waytag_of_xml reste
;;
let rec recupere_bounds=function
  |Element("osm",_,l)->List.map bounds_of_xml (copy_bounds l)
  |Element(_,_,l)->List.map bounds_of_xml (copy_bounds l)
  |_-> raise Incorrecte
;;
     
let expr_of_xml=function
  |Element("tag ",l,[])->Tag  l
  |Element("bounds ",l,[_])->Bounds  l
  |Element("nd", [("k","k'")],[_])->Nd  [("k","k'")]
  |Element("node", attrib,l)-> let a=copy_tag l in Node (attrib,List.map tag_of_xml a)
  |Element("way", attrib,l)-> let a= copy_tag l and b= copy_nd l in 
      Way (attrib,(List.map tag_of_xml a)@(List.map nd_of_xml b) )
  |Element("osm", attrib,l)-> let a=(filtrage_node l) and b=(filtrage_way l) in
      Osm  ((way_of_xml b)@( node_of_xml a))
  |_->raise Incorrecte
;;

(*traitement des noeuds*)

let attribut_bounds bounds=
  let a=recupere_bounds bounds in
  let b=List.hd a in
    begin
      match b with
	| Bounds l->(float_of_string(List.assoc "minlat" l),float_of_string( List.assoc "minlon" l),float_of_string( List.assoc "maxlat" l),float_of_string(List.assoc "maxlon" l))
	| _->raise Incorrecte
    end
;;
let recupere_node=function
  |Element("osm",_,l)-> node_of_xml (filtrage_node l)
  |Element(_,_,l)-> node_of_xml (filtrage_node l)
  |_-> raise Incorrecte
;;

let rec lat_long k=
  match k with
    |[]->[]
    | (Node (l,l'))::l''->(float_of_string(List.assoc "lat" l), float_of_string(List.assoc "lon" l))::( lat_long l'') 
    | _->raise Incorrecte
;;

(* convertion en pixel *)
let coord width hight lat lon expr =
  let (minlat,minlon,maxlat,maxlon)=attribut_bounds expr in
 (int_of_float (0.5 +. (float_of_int (width-1)) *. (lat-.minlat) /. (maxlat-.minlat)),int_of_float (0.5 +. (float_of_int (hight-1)) *. (lon-.minlon) /. (maxlon-.minlon)))
;;
let rec convertlat_long width hight k expr= 
  let rec aux lis width hight exp=
      match lis with
	|[]->[]
	| (l,l')::l''->(coord width hight l l' exp)::( aux l'' width hight exp) 
	| _->raise Incorrecte
  in 
  let liste=lat_long expr in  
    aux liste width hight k
;;
let rec dessine_node list=
 set_color (rgb (Random.int 254) (Random.int 254) (Random.int 254)) ;
  match list with
    |[]->plot 0 0
    |(lat,lon)::l->
       moveto lon lat;
	plot lon lat ;
	dessine_node l	  
;;

(*traitement des aire*)

let recupere_way=function
  |Element("osm",_,l)-> way_of_xml (filtrage_way l)
  |Element(_,_,l)-> way_of_xml (filtrage_way l)
  |_-> raise Incorrecte
;;   
let recupere_wayref=function
  |Element("osm",_,l)-> wayref_of_xml (filtrage_way l)
  |Element(_,_,l)-> wayref_of_xml (filtrage_way l)
  |_-> raise Incorrecte
;;
   
let rec id_node =function
  |[]->[]
  | (Node (l,l'))::l''->float_of_string(List.assoc "id" l) ::( id_node l'') 
  | _->raise Incorrecte
;;
let rec idlat_long k=
  match k with
    |[]->[]
    | (Node (l,l'))::l''->(float_of_string(List.assoc "id" l),float_of_string(List.assoc "lat" l), float_of_string(List.assoc "lon" l))::( idlat_long l'') 
    | _->raise Incorrecte
;;
let idcoord width hight id lat lon expr =
  let (minlat,minlon,maxlat,maxlon)=attribut_bounds expr in
 (id,int_of_float (0.5 +. (float_of_int (width-1)) *. (lat-.minlat) /. (maxlat-.minlat)),int_of_float (0.5 +. (float_of_int (hight-1)) *. (lon-.minlon) /. (maxlon-.minlon)))
;;
let rec idconvertlat_long width hight k expr= 
  let rec aux lis width hight exp=
    match lis with
      |[]->[]
      | (id,l,l')::l''->(idcoord width hight id l l' exp)::( aux l'' width hight exp) 
 
  in 
  let liste=idlat_long expr in  
    aux liste width hight k
;;


let css1=function
  |"highway","motorway"->set_color( rgb 90 94 107) 
  |"highway","service"->set_color( rgb 90 94 107) 
  |"highway","road" ->set_color (rgb 254 254 254) 
  |"highway","pedestrian" ->set_color (rgb 206 206 206) 
  |"building","school"->set_color(rgb 139 108 106)
  |"building","hotel"->set_color(rgb 139 108 106)
  |"bilding","church"->set_color(rgb 139 108 106)
  |"building","yes"->set_color(rgb 200 173 127)
  |"waterway","river"->set_color(rgb 119 181 254)
  |"waterway","stream"->set_color(rgb 119 181 254)
  |"waterway","canal"->set_color(rgb 119 181 254)
  |"natural","water"->set_color(rgb 119 181 254)
  |"natural","coastline"->set_color(rgb 119 181 254)
  |"landuse","grass"->set_color(rgb 130 196 108)
  |"landuse","landouse"->set_color(rgb 130 196 108)
  |"landuse","forest"->set_color(rgb 130 196 108)
  |"leisure","park"->set_color(rgb 130 196 108) 
  |_,_->set_color background
;;

let cssrue=function
  |"highway","motorway"->set_color( rgb 90 94 107) ;set_line_width 20
  |"highway","service"->set_color( rgb 90 94 107) ;set_line_width 10
  |"highway","road" ->set_color (rgb 254 254 254) ;set_line_width 15 
  |"highway","pedestrian" ->set_color (rgb 206 206 206) ;set_line_width 5
  |"building","school"->set_color(rgb 139 108 106) ;set_line_width 20
  |"building","hotel"->set_color(rgb 139 108 106) ;set_line_width 20
  |"bilding","church"->set_color(rgb 139 108 106) ;set_line_width 20
  |"building","yes"->set_color(rgb 200 173 127) ;set_line_width 20
  |"waterway","river"->set_color(rgb 119 181 254) ;set_line_width 20
  |"waterway","stream"->set_color(rgb 119 181 254) ;set_line_width 20
  |"waterway","canal"->set_color(rgb 119 181 254) ;set_line_width 20
  |"natural","water"->set_color(rgb 119 181 254) ;set_line_width 20
  |"natural","coastline"->set_color(rgb 119 181 254) ;set_line_width 1000
  |"landuse","grass"->set_color(rgb 130 196 108) ;set_line_width 20
  |"landuse","landouse"->set_color(rgb 130 196 108) ;set_line_width 20
  |"landuse","forest"->set_color(rgb 130 196 108) ;set_line_width 20
  |"leisure","park"->set_color(rgb 130 196 108)  ;set_line_width 20
  |_,_->set_color background  ;set_line_width 0
;;

let css2=function
  |"highway","motorway"->draw_string "-->"
  |"building","school"->set_text_size 15 ;draw_string  "School"
  |"building","hotel"->draw_string "Hotel"
  |"bilding","church"->draw_string "Eglise"
  |"landuse","forest"->draw_string "Forest"
  |"leisure","park"->set_text_size 90  ;draw_string "Park"
  |_,_->draw_string " "
;;
let  recupere_ref=function
 | Nd [(a,b)]->float_of_string (b)
 |_->raise Incorrecte     
;;

let rec tous_nd nd  =
  match nd with
    |[]->[]
    |n::l-> (recupere_ref n )::(tous_nd l )
       
;;

let rec chemin lis m=
  match lis with
    |[]->[]
    |(a,b,c)::l-> if m=a then (b,c)::(chemin l m)
      else chemin l m
;;

let rec synthese lis refliste=
  match refliste with
    |[]->failwith " n'est pas mature pour être tracé"
    |[a]-> ( chemin lis a)
    |a'::l'->  ( chemin lis a')@ (synthese lis l') 
;;

let rec nth var liste =
  match (var,liste) with
    |0,x::l->x
    |n,x::l->nth (n-1) l
    |_, []-> failwith " position imprecis ou liste vide"
;;
let rec trace_chemin liste (a',b') =
  set_color black;
  match liste with
    |[]->moveto b' a';
	lineto  b' a'
    |(a,b)::l->
       begin
	 moveto b' a';
	 lineto b a;
	 trace_chemin l (a,b)

       end
 (* | (a, b)::[]->lineto a b*)
;;


let rec trace_way lis wayref=
  match wayref with
    |Way (l,l')::reste->
       let a= (tous_nd  l') in let b=(synthese lis a) in 
	 trace_chemin b (List.hd b);
	 trace_way lis reste	   
    |_->raise Incorrecte
;;


let rec clef_valeur=function
  |Tag  [(a,b);(a',b') ] ->(b,b');
  |_->raise Incorrecte
;;
let rec copy_Tag=function
  |[ ]->[ ]
  | (Tag  [(a,b);(a',b') ])::reste->(Tag  [(a,b);(a',b')])::(copy_Tag reste)
  |_::reste->copy_Tag reste
;;
let rec copy_Nd=function
  |[ ]->[ ]
  |(Nd [(a,b) ] )::reste->(Nd [(a,b) ])::(copy_Nd reste)
  |_::reste-> copy_Nd reste
;;

let rec valeur_tagway1  tableau col=
  match col with
  | []-> failwith " pas de tag pour ce chemin"
  | [a]->css1 a ;css2 a;
      fill_poly tableau
  |a::l-> css1 a ; css2 a;
      fill_poly tableau;
      valeur_tagway1 tableau l
;;

let rec rever_latlon = function
| [] ->  []
| (a,b)::reste -> (b,a)::(rever_latlon reste)
;;

let rec aire lis way=
  match way with
    |Way (l,l')->
	 let a= ((tous_nd (copy_Nd l'))) in let b=(rever_latlon(synthese lis a)) and   k=List.map clef_valeur (copy_Tag l') in
	   if ((nth 0 b)=nth ((List.length b)-1) b) then
	     begin
	       
	       let tableau=(Array.of_list b) in
		 valeur_tagway1 tableau k
	     end
    |_-> raise Incorrecte
;;




 let rec dessine_tagway lis way=
  match way with
      | []-> failwith " il n'y pas de chemin"
      |[Way (l,l')]->
	 aire lis (Way (l,l') )   
      |Way (l,l')::reste->
	 dessine_tagway lis reste;
	  aire lis (Way (l,l'));
	   
    |_::reste-> dessine_tagway lis reste
 ;;

(* |  [ ]  *)
 

let mainWindow a=
  open_graph" ";
  set_window_title "le plan";
let _=read_key() in close_graph()
;;
open_graph " 2500*2500";;
set_window_title "le plan";;
let temoin=expr_of_xml b;;
let liste= recupere_node a;;
let latlong=lat_long liste;;
(* attribut_bounds  a;;
 recupere_bounds a;;
clear_graph();;*)
let _x=size_x() and _y=size_y() in
  dessine_node( convertlat_long _x _y a liste);;
(*let refway=recupere_wayref a;;*)
let _x=size_x() ;;
let _y=size_y() ;;
let idlatlon=idconvertlat_long _x _y a liste;;
let way= recupere_way a;;
trace_way idlatlon ( recupere_wayref a);;
dessine_tagway idlatlon way;;

let  _=read_key() in close_graph()
