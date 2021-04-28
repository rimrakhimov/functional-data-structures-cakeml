structure HeightBiasedLeftistHeap =
struct
    datatype 'a heap = E ('a -> 'a -> ordering) | T ('a -> 'a -> ordering) int 'a ('a heap) ('a heap)

    exception Empty

    fun empty comp = E comp

    fun singleton comp x = T comp 1 x (empty comp) (empty comp)

    fun getComp h = 
        case h of 
            E comp => comp 
          | T comp _ _ _ _ => comp

    fun rank h =    
        case h of
            E _ => 0
          | T _ r _ _ _ => r
    
    fun makeT x a b = 
        if rank a >= rank b 
        then T (getComp a) (1 + rank b) x a b
        else T (getComp a) (1 + rank a) x b a

    fun merge h1 h2 =
        case (h1, h2) of
            (E _, h2) => h2
          | (h1, E _) => h1
          | (T comp _ x a1 b1, T _ _ y a2 b2) =>
                if comp x y <> Greater
                then makeT x a1 (merge b1 h2)
                else makeT y a2 (merge h1 b2)

    fun insert x h = 
        let
            val comp = getComp h
        in
            merge (singleton comp x) h
        end

    fun insert2 x h =
        case h of
            E comp => singleton comp x
          | T comp _ y a b =>
                if comp y x <> Greater
                then makeT y a (insert2 x b)
                else makeT x a (insert2 y b)

    fun findMin h =
        case h of
            E _ => raise Empty
          | T _ _ x _ _ => x

    fun deleteMin h =   
        case h of
            E _ => raise Empty
          | T _ _ _ a b => merge a b

    fun fromList comp l =
        let
            fun mergeAdjascent l = 
                case l of 
                    [] => []
                  | (x::[]) => [x]
                  | (x::y::ls) => merge x y :: mergeAdjascent ls

            fun internal l =
                case l of
                    [] => empty comp
                  | (x::[]) => x
                  | _ => internal (mergeAdjascent l)
        in
            internal (List.map (fn x => singleton comp x) l)
        end
end

structure WeightBiasedLeftistHeap =
struct
    datatype 'a heap = E ('a -> 'a -> ordering) | T ('a -> 'a -> ordering) int 'a ('a heap) ('a heap)

    exception Empty

    fun empty comp = E comp

    fun singleton comp x = T comp 1 x (empty comp) (empty comp)

    fun getComp h = 
        case h of 
            E comp => comp 
          | T comp _ _ _ _ => comp

    fun weight h =    
        case h of
            E _ => 0
          | T _ w _ _ _ => w

    fun merge h1 h2 =
        case (h1, h2) of
            (E _, h2) => h2
          | (h1, E _) => h1
          | (T comp w1 x a1 b1, T _ w2 y a2 b2) =>
                if 
                    comp x y <> Greater
                then 
                    if weight a1 >= (weight b1 + weight h2)
                    then T comp (w1 + w2 + 1) x a1 (merge b1 h2) 
                    else T comp (w1 + w2 + 1) x (merge b1 h2) a1
                else 
                    if weight a2 >= (weight h1 + weight b2)
                    then T comp (w1 + w2 + 1) y a2 (merge h1 b2) 
                    else T comp (w1 + w2 + 1) y (merge h1 b2) a2

    fun insert x h = 
        let
            val comp = getComp h
        in
            merge (singleton comp x) h
        end

    fun insert2 x h =
        case h of
            E comp => singleton comp x
          | T comp w y a b =>
                if 
                    comp y x <> Greater
                then
                    if weight a >= 1 + weight b 
                    then T comp (w + 1) y a (insert2 x b)
                    else T comp (w + 1) y (insert2 x b) a
                else 
                    if weight a >= 1 + weight b 
                    then T comp (w + 1) x a (insert2 y b)
                    else T comp (w + 1) x (insert2 y b) a

    fun findMin h =
        case h of
            E _ => raise Empty
          | T _ _ x _ _ => x

    fun deleteMin h =   
        case h of
            E _ => raise Empty
          | T _ _ _ a b => merge a b

    fun fromList comp l =
        let
            fun mergeAdjascent l = 
                case l of 
                    [] => []
                  | (x::[]) => [x]
                  | (x::y::ls) => merge x y :: mergeAdjascent ls

            fun internal l =
                case l of
                    [] => empty comp
                  | (x::[]) => x
                  | _ => internal (mergeAdjascent l)
        in
            internal (List.map (fn x => singleton comp x) l)
        end
end

val heap = WeightBiasedLeftistHeap.empty Int.compare

val heap = WeightBiasedLeftistHeap.insert2 4 heap
val heap = WeightBiasedLeftistHeap.insert2 2 heap
val heap = WeightBiasedLeftistHeap.insert2 5 heap
val heap = WeightBiasedLeftistHeap.insert2 1 heap

val heap = WeightBiasedLeftistHeap.fromList Int.compare [4, 2, 5, 2]

val heap = WeightBiasedLeftistHeap.deleteMin heap
val heap = WeightBiasedLeftistHeap.deleteMin heap

val _ = print ("\n\nMIN_ELEMENT=" ^ Int.toString (WeightBiasedLeftistHeap.findMin heap) ^ "========\n\n")