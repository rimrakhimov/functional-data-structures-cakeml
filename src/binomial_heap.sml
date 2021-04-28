structure BinomialHeap =
struct
    datatype 'a tree = Node int 'a ('a tree list)

    datatype 'a heap = Heap ('a -> 'a -> ordering) ('a tree list)

    exception Empty

    fun empty comp = Heap comp []

    fun isEmpty (Heap _ ts) =
        case ts of
            [] => True
          | _ => False

    fun link comp t1 t2 =
        case (t1, t2) of
            (Node r x1 c1, Node _ x2 c2) =>
                if comp x1 x2 <> Greater
                then Node (r + 1) x1 (t2 :: c1)
                else Node (r + 1) x2 (t1 :: c2)

    fun root (Node _ x _) = x

    fun rank (Node r _ _) = r
    
    local 
        fun insTree comp t ts =
            case (t, ts) of
                (t, []) => [t]
              | (t, t'::ts') =>
                    if rank t < rank t' then (t :: ts) else insTree comp (link comp t t') ts'
    in
        fun insert x (Heap comp ts) = Heap comp (insTree comp (Node 0 x []) ts)

        fun merge (Heap comp ts1) (Heap _ ts2) =
            let
                fun merge_forests comp ts1 ts2 =
                    case (ts1, ts2) of
                        (ts1, []) => ts1
                      | ([], ts2) => ts2
                      | (t1::ts1', t2::ts2') =>
                            if rank t1 < rank t2
                            then t1 :: merge_forests comp ts1' ts2
                            else if rank t1 > rank t2
                            then t2 :: merge_forests comp ts1 ts2'
                            else insTree comp (link comp t1 t2) (merge_forests comp ts1' ts2')
            in
                Heap comp (merge_forests comp ts1 ts2)
            end
    end

    local 
        fun removeMinTree comp ts =
            case ts of
                [] => raise Empty
              | (t::[]) => (t, [])
              | (t::ts') =>
                let
                    val (t', ts'') = removeMinTree comp ts'
                in
                    if comp (root t) (root t') <> Greater
                    then (t, ts')
                    else (t', t::ts'')
                end 
    in
        fun findMin (Heap comp ts) = root (fst (removeMinTree comp ts))

        fun deleteMin (Heap comp ts) =
            let
                val (Node _ _ ts1, ts2) = removeMinTree comp ts
            in
                merge (Heap comp (List.rev ts1)) (Heap comp ts2)
            end
    end

    fun findMin2 (Heap comp ts) =
        let
            fun internal comp ts =
                case ts of
                    [] => raise Empty
                  | (t::[]) => root t
                  | (t::ts') => 
                    let
                        val x = root t
                        val x' = internal comp ts'
                    in
                        if comp x x' <> Greater
                        then x
                        else x'
                    end 
        in
            internal comp ts
        end
end

structure BinomialHeap2 =
struct
    datatype 'a tree = Node 'a ('a tree list)

    datatype 'a heap = Heap ('a -> 'a -> ordering) ((int * 'a tree) list)

    exception Empty

    fun empty comp = Heap comp []

    fun isEmpty (Heap _ ts) =
        case ts of
            [] => True
          | _ => False

    fun link comp t1 t2 =
        case (t1, t2) of
            (Node x1 c1, Node x2 c2) =>
                if comp x1 x2 <> Greater
                then Node x1 (t2 :: c1)
                else Node x2 (t1 :: c2)

    fun root (Node x _) = x

    local 
        fun insTree comp (r, t) ts =
            case ts of
                [] => [(r, t)]
              | ((r', t')::ts') =>
                    if r < r' then ((r, t) :: ts) else insTree comp (r + 1, (link comp t t')) ts'
    in
        fun insert x (Heap comp ts) = Heap comp (insTree comp (0, Node x []) ts)

        fun merge (Heap comp ts1) (Heap _ ts2) =
            let
                fun merge_forests comp ts1 ts2 =
                    case (ts1, ts2) of
                        (ts1, []) => ts1
                      | ([], ts2) => ts2
                      | ((r1, t1)::ts1', (r2, t2)::ts2') =>
                            if r1 < r2
                            then (r1, t1) :: merge_forests comp ts1' ts2
                            else if r1 > r2
                            then (r2, t2) :: merge_forests comp ts1 ts2'
                            else insTree comp (r1 + 1, link comp t1 t2) (merge_forests comp ts1' ts2')
            in
                Heap comp (merge_forests comp ts1 ts2)
            end
    end

    local 
        fun removeMinTree comp ts =
            case ts of
                [] => raise Empty
              | ((r, t)::[]) => ((r, t), [])
              | ((r, t)::ts') =>
                let
                    val ((r', t'), ts'') = removeMinTree comp ts'
                in
                    if comp (root t) (root t') <> Greater
                    then ((r, t), ts')
                    else ((r', t'), (r, t)::ts'')
                end 
    in
        fun findMin (Heap comp ts) = root (snd (fst (removeMinTree comp ts)))

        fun deleteMin (Heap comp ts) =
            let
                val ((r, Node _ ts1), ts2) = removeMinTree comp ts
            in
                merge (Heap comp (List.rev (List.mapi (fn i => fn x => (r - 1 - i, x)) ts1))) (Heap comp ts2)
            end
    end

    fun findMin2 (Heap comp ts) =
        let
            fun internal comp ts =
                case ts of
                    [] => raise Empty
                  | ((_, t)::[]) => root t
                  | ((_, t)::ts') => 
                    let
                        val x = root t
                        val x' = internal comp ts'
                    in
                        if comp x x' <> Greater
                        then x
                        else x'
                    end 
        in
            internal comp ts
        end
end

structure ExplicitBinomialHeap =
struct
    datatype 'a explicit_heap = E ('a -> 'a -> ordering) | NE ('a -> 'a -> ordering) 'a ('a BinomialHeap.heap)

    fun empty comp = E comp

    fun isEmpty eh =
        case eh of
            E _ => True
          | _ => False

    fun insert x eh =
        case eh of
            E comp => NE comp x (BinomialHeap.insert x (BinomialHeap.empty comp))
          | NE comp m h =>
                if comp m x <> Greater
                then NE comp m (BinomialHeap.insert x h)
                else NE comp x (BinomialHeap.insert x h)
    
    fun merge eh1 eh2 =
        case (eh1, eh2) of
            (eh1, E _) => eh1
          | (E _, eh2) => eh2
          | (NE comp m1 h1, NE _ m2 h2) =>
                if comp m1 m2 <> Greater
                then NE comp m1 (BinomialHeap.merge h1 h2)
                else NE comp m2 (BinomialHeap.merge h1 h2)

    fun deleteMin eh =
        case eh of
            E _ => raise BinomialHeap.Empty
          | NE comp m h =>
            let
                val h' = BinomialHeap.deleteMin h
            in
                if BinomialHeap.isEmpty h'
                then E comp
                else NE comp (BinomialHeap.findMin h') h'
            end

    fun findMin exp_heap =
        case exp_heap of
            E _ => raise BinomialHeap.Empty
          | NE _ m _ => m
end

val heap = BinomialHeap.empty Int.compare

val heap = BinomialHeap.insert 4 heap
val heap = BinomialHeap.insert 2 heap
val heap = BinomialHeap.insert 5 heap
val heap = BinomialHeap.insert 1 heap

(* val heap = BinomialHeap.fromList Int.compare [4, 2, 5, 2] *)

val heap = BinomialHeap.deleteMin heap
val heap = BinomialHeap.deleteMin heap

val _ = print ("\n\nMIN_ELEMENT=" ^ Int.toString (BinomialHeap.findMin heap) ^ "========\n\n")