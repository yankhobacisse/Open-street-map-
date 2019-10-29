# Open-street-map-
Rapport sur le projet OpenStreetMap
Nous allons d’abors vous presenter le doc de notre programme.
Ps: Lancer le mode tuareg puis evaluer le buffering et observer le
resultat.
(* c’est notre type de base nous permettant de manipler le type xml en
Ocmal*)
type expr =
Bounds of (string * string) list
| Tag of (string * string) list
| Nd of (string * string) list
| Way of ((string * string) list * expr list)
| Node of ((string * string) list * expr list)
| Osm of expr list
# exception Incorrecte
(*copy tout les tag dans un way*)
#
val copy_tag : Xml.xml list -> Xml.xml list = <fun>
(*copy tout les nd dans un way*)
#
val copy_nd : Xml.xml list -> Xml.xml list = <fun>
(* copy tout les bounds dans un osm*)
#
val copy_bounds : Xml.xml list -> Xml.xml list = <fun>
(*copy tout les nodes dans un osm*)
#
val filtrage_node : Xml.xml list -> Xml.xml list = <fun>
(*copy tout les ways dans un osm*)
#
val filtrage_way : Xml.xml list -> Xml.xml list = <fun>#
val filtrage_way_sans_nd : Xml.xml list -> Xml.xml list = <fun>
(*ce qui suit nous permet de traduire le osm en notre type expr *)
# val tag_of_xml : Xml.xml -> expr = <fun>
# val nd_of_xml : Xml.xml -> expr = <fun>
# val bounds_of_xml : Xml.xml -> expr = <fun>
# val node_of_xml : Xml.xml list -> expr list = <fun>
# val way_of_xml : Xml.xml list -> expr list = <fun>
# val wayref_of_xml : Xml.xml list -> expr list = <fun>
#
#
val waytag_of_xml : Xml.xml list -> expr list = <fun>
val recupere_bounds : Xml.xml -> expr list = <fun>
# val expr_of_xml : Xml.xml -> expr = <fun>
# val attribut_bounds : Xml.xml -> float * float * float * float = <fun>
#
val recupere_node : Xml.xml -> expr list = <fun>
#
val lat_long : expr list -> (float * float) list = <fun>
#
val coord : int -> int -> float -> float -> Xml.xml -> int * int = <fun>
#
Characters 187-188:
| _->raise Incorrecte
(*pour le reste on a utliser des noms symboliques pour les fonctions*)
val convertlat_long : int -> int -> Xml.xml -> expr list -> (int * int) list =
<fun>
# val dessine_node : (int * int) list -> unit = <fun>
# val recupere_way : Xml.xml -> expr list = <fun>
#
#
val recupere_wayref : Xml.xml -> expr list = <fun>
val id_node : expr list -> float list = <fun>#
#
val idlat_long : expr list -> (float * float * float) list = <fun>
val idcoord : int -> int -> 'a -> float -> float -> Xml.xml -> 'a * int * int =
<fun>
#
val idconvertlat_long :
int -> int -> Xml.xml -> expr list -> (float * int * int) list = <fun>
# val css1 : string * string -> unit = <fun>
# val cssrue : string * string -> unit = <fun>
#
#
val css2 : string * string -> unit = <fun>
val recupere_ref : expr -> float = <fun>
# val tous_nd : expr list -> float list = <fun>
# val chemin : ('a * 'b * 'c) list -> 'a -> ('b * 'c) list = <fun>
# val synthese : ('a * 'b * 'c) list -> 'a list -> ('b * 'c) list = <fun>
# val nth : int -> 'a list -> 'a = <fun>
#
val trace_chemin : (int * int) list -> int * int -> unit = <fun>
#
val trace_way : (float * int * int) list -> expr list -> 'a = <fun>
#
val clef_valeur : expr -> string * string = <fun>
# val copy_Tag : expr list -> expr list = <fun>
# val copy_Nd : expr list -> expr list = <fun>
#
val valeur_tagway1 : (int * int) array -> (string * string) list -> unit =
<fun>
#
#
#
#
val rever_latlon : ('a * 'b) list -> ('b * 'a) list = <fun>
val aire : (float * int * int) list -> expr -> unit = <fun>
val dessine_tagway : (float * int * int) list -> expr list -> unit = <fun>
val mainWindow : 'a -> unit = <fun>Notr
