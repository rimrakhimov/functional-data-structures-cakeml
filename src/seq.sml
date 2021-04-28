structure Seq =
struct
    datatype 'a seq = Nil | Cons 'a (unit -> 'a seq)

    exception Empty

    fun cons x xq = Cons x (fn () => xq)

    fun null xq =
        case xq of
            Nil => True
          | _ => False

    fun hd seq =
        case seq of
            Cons x xf => x
          | Nil => raise Empty

    fun tl seq =
        case seq of
            Cons x xf => xf ()
          | Nil => raise Empty

    fun fromList l = List.foldr cons Nil l

    fun toList xq =
        case xq of
            Nil => []
          | Cons x xf => x :: toList (xf ())

    fun take xq n =
        case (xq, n) of
            (_, 0) => []
          | (Nil, _) => raise Subscript
          | (Cons x xf, n) => x :: take (xf ()) (n-1)

    fun drop xq n =
        case (xq, n) of 
            (_, 0) => xq
          | (Nil, _) => raise Subscript
          | (Cons x xf, n) => drop (xf ()) (n-1)

    fun append xq yq =
        case xq of
            Nil => yq
          | Cons x xf => Cons x (fn () => append (xf ()) yq)

    fun interleave xq yq =
        case xq of
            Nil => yq
          | Cons x xf => Cons x (fn () => interleave yq (xf ()))

    fun map f xq =
        case xq of
            Nil => Nil
          | Cons x xf => Cons (f x) (fn () => map f (xf ()))

    fun filter pred xq =
        case xq of
            Nil => Nil
          | Cons x xf => if pred x then Cons x (fn () => filter pred (xf ())) else filter pred (xf ())

    fun iterates f x = Cons x (fn () => iterates f (f x))

    fun from k = iterates (fn t => t + 1) k

    (*****************************************************************************************)

    fun add xq yq =
        case (xq, yq) of
            (Cons x xf, Cons y yf) => Cons (x + y) (fn () => add (xf ()) (yf ()))
          | _ => Nil

    fun elementwiseRepeat xq k =
        let
            fun internal xq t =
                case (xq, t) of
                    (Nil, _) => Nil
                  | (Cons x xf, 0) => internal (xf ()) k
                  | (Cons x xf, t) => Cons x (fn () => internal xq (t-1))
        in
            internal xq k
        end

    fun addAdjascent xq =
        case xq of
            Nil => Nil
          | Cons x xf =>
            case (xf ()) of 
                Nil => raise Subscript
              | Cons y yf => Cons (x+y) (fn () => addAdjascent (yf ()))

    fun takewhile pred xq =
        case xq of
            Nil => Nil
          | Cons x xf => if pred x then Cons x (fn () => takewhile pred (xf ())) else Nil
end

val x = Seq.map (fn t => t * t) (Seq.from 1)
val y = Seq.from 1
val s = Seq.takewhile (fn t => t > 0) x
val _ = List.app (fn x => print ("\n\n====" ^ Int.toString x ^ "=====\n\n")) (Seq.take s 5)

structure Seq2 =
struct
  datatype 'a seq = Nil | Cons (unit -> 'a * 'a seq)

  exception Empty

  fun hd seq =
      case seq of 
          Cons xf => fst (xf ())
        | Nil => raise Empty

  fun tl seq =
      case seq of
          Cons xf => snd (xf ())
        | Nil => raise Empty

  fun from k = Cons (fn () => (k, from (k+1)))

  fun take xq n =
      case (xq, n) of
          (_, 0) => []
        | (Nil, _) => raise Subscript
        | (Cons xf, n) => fst (xf ()) :: take (snd (xf ())) (n-1)
end

structure Seq3 =
struct
    datatype 'a seqnode = Nil | Cons (unit -> 'a * 'a seq) 
         and 'a seq = Seq (unit -> 'a seqnode)
    
    exception Empty

    fun hd (Seq f) =
        case f () of
            Cons xf => fst (xf ())
          | Nil => raise Empty

    fun tl (Seq f) =
        case f () of
            Cons xf => snd (xf ())
          | Nil => raise Empty
    
    fun from k = Seq (fn () => Cons (fn () => (k, from (k + 1))))

    fun take (Seq f) n =
        case (f (), n) of
            (_, 0) => []
          | (Nil, _) => raise Subscript
          | (Cons xf, n) => fst (xf ()) :: take (snd (xf ())) (n-1)
end

   
(* val s = from (30, print "\n\n===Hello====\n\n"); *)
(* val s = Seq.from 12; *)
(* print ("\n\n====" ^ Int.toString (hd s) ^ "=====\n\n");
val s = tl s;
print ("\n\n====" ^ Int.toString (hd s) ^ "=====\n\n");
val s = tl s;
print ("\n\n====" ^ Int.toString (hd s) ^ "=====\n\n"); *)

(* List.app (fn x => print ("\n\n====" ^ Int.toString x ^ "=====\n\n")) (take s 3); *)



(* val s = Seq3.from 4
val _ = List.app (fn x => print ("\n\n====" ^ Int.toString x ^ "=====\n\n")) (Seq3.take s 3); *)