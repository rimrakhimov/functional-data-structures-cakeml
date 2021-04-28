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

