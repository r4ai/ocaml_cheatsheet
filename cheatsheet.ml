(* ============== *)
(*   Data Types   *)
(* ============== *)

let x = 5                (* 型推論 *)
let x : int = 5          (* 整数, 31 or 61 bits *)
let y : float = 5.0      (* 倍精度浮動小数 *)
let b : bool = true      (* 真偽値 *)
let c : char = 'c'       (* ASCII文字 *)
let s : string = "Hello" (* 文字列 *)
let v : unit = ()        (* void *)

let lst : int list = [ 1; 2; 3 ]                   (* リスト, 不変 *)
let arr : int array = [| 1; 2; 3 |]                (* 配列, 可変 *)
let tuple : int * string * char = (1, "foo", 't')  (* 組 *)

(* レコード: 名前付きデータの集まり *)
type score = {
    mutable genre : string;  (* 可変 *)
    name : string;           (* 不変 *)
    score : int;             (* 不変 *)
  }
let my_score : score = {
    name = "hoge";
    genre = "cs";
    score = 100;
  }

(* ヴァリアント: 「どれかひとつ」を表す型 *)
(* type 型名 = 構成子 of 引数の型 | ... *)
type str_or_num = Str of string | Num of int
type 'a list_t =
  | Nil                     (* 引数なし *)
  | Cell of 'a * 'a list_t  (* 引数あり *)
type 'a tree_t =
  | Nil
  | Node of 'a * 'a tree_t * 'a

(* Option : None かもしれない値を表す型          *)
(* None はほかの言語でいう null などに相当する。 *)
(* type 'a option = None | Some of 'a            *)
let maybe_none1 : int option = Some 3
let maybe_none2 : string option = Some "not null"
let maybe_none3 : string option = None

(* ============== *)
(*     演算子     *)
(* ============== *)

let is_eq = 3 = 2    (* >>> false *)
let is_ne = 3 <> 2   (* >>> true  *)
let is_gt = 3 > 2    (* >>> true  *)
let is_lt = 3 < 2    (* >>> false *)
let is_ge = 3 >= 2   (* >>> true  *)
let is_le = 3 <= 2   (* >>> false *)


(* ============== *)
(*     制御式     *)
(* ============== *)

let for_expr () =
  for i = 0 to 3 do
    print_int i
  done

let while_expr () =
  let i = ref 0 in
  while !i < 3 do
    print_int !i;
    i := !i + 1
  done

let recrusive_fn () =
  let rec loop i =
    if i < 0 then ()
    else (loop (i - 1); print_int i)
  in
  loop 2;;


(* ============== *)
(*      List      *)
(* ============== *)

let lst1 : int list = [ 1; 2; 3 ]  (* 1次元 *)
let lst2 : string list list = [    (* 2次元 *)
    [ "H"; "e"; "l"; "l"; "o" ];
    [ "W"; "o"; "r"; "l"; "d" ]
  ]
let lst3 = 1 :: 2 :: 3 :: []        (* [ 1; 2; 3 ] *)
let lst4 = 1 :: 2 :: [ 3 ]          (* [ 1; 2; 3 ] *)
let lst5 = [1] :: [2] :: [3] :: []  (* [ [1]; [2]; [3] ] *)

(* list は match で分解できる *)
let rec sum lst = match lst with
  | [] -> 0
  | hd :: tl -> hd + sum tl
let sum_of_lst1 = sum lst1  (* >>> 1 + 2 + 3 = 6 *)
let rec multipy num lst = match lst with
  | [] -> []
  | hd :: tl -> hd * num :: multipy num tl
let multipied_by_2 = multipy 2 lst1  (* >>> [ 2; 4; 6 ] *)

(* List.length : 'a list -> int *)
let len = List.length lst1

(* List.map : ('a -> 'b) -> 'a list -> 'b *)
let doubled = List.map (fun x -> x * 2) lst1
let floated = List.map float_of_int lst1

(* List.filter : ('a -> bool) -> 'a list -> 'a list *)
let only_2 = List.filter (fun x -> if x = 2 then true else false) lst1;;

(* List.fold_left : ('b -> 'a -> 'b) -> 'b -> 'a list -> 'b *)
let sum = List.fold_left (fun a x -> a + x) 0 lst1

(* List.fold_right : ('a -> 'b -> 'a) -> 'a list -> 'b -> 'b  *)
let sum = List.fold_right (fun x a -> x + a) lst1 0


(* ============== *)
(*      参照      *)
(* ============== *)

let x = ref 3 ;;  (* 可変参照 *)
x := 4 ;;         (* 代入 *)
print_int !x ;;   (* 参照外し *)

let arr = [| 1; 2; 3 |] ;;  (* 配列 *)
let arr_1 = arr.(1) ;;      (* 要素へのアクセス *)
arr.(0) <- arr_1 ;;         (* 要素への代入 *)
Array.iter (fun x -> print_int x; print_string ", ") arr
(* >>> 2, 2, 3, *)

let s = "Hello" ;;   (* 文字列 *)
let s_0 = s.[0] ;;   (* 要素へのアクセス *)


(* ============== *)
(*    例外処理    *)
(* ============== *)

(* 例外の定義 *)
exception MyError of string
exception Todo

(* 例外の発生 *)
let raise_error code =
  if code = true then raise (MyError "エラーだよ！")
  else "エラーは発生しませんでした"

(* 例外の処理 *)
let handle_error () =
  try
    raise_error true
  with MyError msg -> "エラー: " ^ msg
;;
let res = (handle_error ())  (* >>> エラー: エラーだよ！ *)



(* ============== *)
(*     Module     *)
(* ============== *)

(* Signature, 宣言 *)
module type Tree_t = sig
  type tree
  val sum_tree : tree -> int
  val search : tree -> int -> bool
end

(* Module, 定義 *)
module Tree : Tree_t = struct
  type tree = Empty | Node of tree * int * tree

  exception Todo

  (* 木の中の整数の合計を返す *)
  (* sum_tree : tree -> int *)
  let rec sum_tree tree =
    match tree with
    | Empty -> 0
    | Node (l, n, r) -> sum_tree l + n + sum_tree r

  (* 二分探索木の中に、数字`m`が存在するか判定する *)
  (* search : tree -> int -> bool *)
  let rec search tree m =
    match tree with
    | Empty -> false
    | Node (l, n, r) ->
        if n < m then search r m else if n > m then search l m else true
end

