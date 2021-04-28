fun suffixes xs =
    case xs of
        [] => [ [] ]
      | (x :: xs) => (x :: xs) :: suffixes xs

(* val _ = List.app (fn l => (print "\n[ "; List.app (fn x => print (Int.toString x ^ " ")) l; print "]\n")) (suffixes [1, 2, 3, 4]) *)

structure BinTree =
struct
    datatype 'a tree = E ('a -> 'a -> ordering) | T ('a -> 'a -> ordering) ('a tree) 'a ('a tree)

    exception Exists

    fun empty comp = E comp

    fun member x t =
        case t of
            E _ => False
          | T comp a y b =>
            case comp x y of
                Less => member x a
              | Greater => member x b
              | Equal => True

    fun member2 x t =
        let
            fun search x t c =
                case t of
                    E comp => comp x c = Equal
                  | T comp a y b =>
                        if comp x y = Less then search x a c
                        else search x b y
        in
            case t of
                E _ => False
              | T _ _ y _ => search x t y
        end

    fun insert x t =
        let
            fun internal x t =
                case t of
                    E comp => T comp (empty comp) x (empty comp)
                  | T comp a y b => 
                    case comp x y of
                        Less => T comp (internal x a) y b
                      | Greater => T comp a y (internal x b)
                      | Equal => raise Exists
        in
            internal x t handle Exists => t
        end

    fun insert2 x t =
        let
            fun internal x t c = 
                case t of
                    E comp => if comp x c = Equal then raise Exists else T comp (empty comp) x (empty comp)
                  | T comp a y b =>
                        if comp x y = Less 
                        then T comp (internal x a c) y b
                        else T comp a y (internal x b y)
        in
            (case t of
                E comp => T comp (empty comp) x (empty comp)
              | T comp _ y _ => internal x t y
            ) handle Exists => t
        end

    fun toList t =
        case t of
            E _ => []
          | T _ a y b => y :: (toList a @ toList b)
end

val tree = BinTree.empty Int.compare
val tree = BinTree.insert2 3 tree
val tree = BinTree.insert2 14 tree
val tree = BinTree.insert2 1 tree
val tree = BinTree.insert2 2 tree
val tree = BinTree.insert2 0 tree
val tree = BinTree.insert2 14 tree

val _ = print ("\n\n====" ^ Bool.toString (BinTree.member2 14 tree) ^ "====\n\n")

val _ = List.app (fn x => print ("\n\n====" ^ Int.toString x ^ "=====\n\n")) (BinTree.toList tree)