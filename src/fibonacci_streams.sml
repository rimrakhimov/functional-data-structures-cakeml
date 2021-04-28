structure LazyPrivate =
struct
    datatype 'a state = Pure 'a | Except exn | Delay (unit -> 'a)

    fun compute f = Pure (f ()) handle e => Except e
end

structure Lazy =
struct
    type 'a susp = 'a LazyPrivate.state ref

    fun delay f = Ref (LazyPrivate.Delay f)

    fun force r =
        case (!r) of
            LazyPrivate.Pure x => x
          | LazyPrivate.Except e => raise e
          | LazyPrivate.Delay f => (r := LazyPrivate.compute f; force r)
end

structure StreamFibs =
struct
    datatype 'a stream = Cons 'a (unit -> 'a stream)

    fun hd (Cons x _) = x

    fun tl (Cons _ xf) = xf ()

    local
        fun take_aux n (Cons x xf) lst =
            if n = 0
            then lst
            else take_aux (n - 1) (xf ()) (x :: lst)
    in
        fun take n s = List.rev (take_aux n s [])

        fun nth n s = List.hd (take_aux (n + 1) s [])
    end

    fun sum (Cons x xf) (Cons y yf) =
        Cons (x + y) (fn () => sum (xf ()) (yf ()))

    fun fibs () = 
        Cons 0 (fn () => 
            Cons 1 (fn () => 
                sum (tl (fibs ())) (fibs ())
            )
        )

    fun nth_fib n = nth n (fibs ())
end

structure LazyFibs =
struct
    datatype 'a lazystream = Cons 'a ('a lazystream Lazy.susp)

    fun hd (Cons x _) = x

    fun tl (Cons _ xf) = Lazy.force xf

    local
        fun take_aux n (Cons x xf) lst =
            if n = 0
            then lst
            else take_aux (n - 1) (Lazy.force xf) (x :: lst)
    in
        fun take n s = List.rev (take_aux n s [])

        fun nth n s = List.hd (take_aux (n + 1) s [])
    end

    fun sum (Cons x xf) (Cons y yf) =
        Cons (x + y) (Lazy.delay (fn () => sum (Lazy.force xf) (Lazy.force yf)))

    fun fibs () = 
        Cons 0 (Lazy.delay (fn () => 
            Cons 1 (Lazy.delay (fn () => 
                sum (tl (fibs ())) (fibs ())
            ))
        ))

    val fib = fibs ()

    fun nth_fib n = nth n fib
end

val v = StreamFibs.nth_fib 30
(* val v = LazyFibs.nth_fib 30 *)
val _ = print ("\n\n======" ^ Int.toString v ^ "=======\n\n")

val t = StreamFibs.nth_fib 31
(* val t = LazyFibs.nth_fib 31 *)
val _ = print ("\n\n======" ^ Int.toString t ^ "=======\n\n")