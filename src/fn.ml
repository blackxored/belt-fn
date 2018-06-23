(** The identity function. 
    Returns its argument. 

    Also known as the `I` combinator or `Idiot`.
*)
let id x = x

(** Produces a function that always returns its first argument.

    Also known as the `K` or `Kestrel` combinator. 
*)
let const a _b = a
(* const is reserved in Javascript, so we provide and alias other than `$$const` *)
let konst = const

(** Produces a function that always returns its second argument.

    Also known as the `Ki` or `Kite` combinator. 
*)
let kite _a b = b

(** Negates a function. 

    Also known as `complement`, `negate`, `not`.
*)
let non f x = not (f x)

(** Function Composition
    [compose f g x] is [f (g x)].
    Also known as the `B` or `Bluebird` combinator.
*)
let compose f g x = f (g x)

(** Compose 3 functions.

      Also known as the `B3` or `Becard` combinator.
*)
let compose3 f g h x = f (g (h x))

(** Reverse compose 

     Also known as `Q` or `Queer` combinator, pipe.
*)
let pipe f g x = g (f x)

(** Reverses the order of arguments for a binary function.

    Also known as the `C` or `Cardinal` combinator.
*)
let flip f x y = f y x

(** Takes a function and a value and returns a function that applies the 
    value to the function.

    Also known as the `A` or `I*` combinators, `applicator`, `$`.  
*)
let apply f x = f x

(** Takes a value and a function and applies that value to the function.

     Also known as the `T` or `Trush` combinator.
*)
let trush a b = b a 

(** [forever f] runs [f ()] until it throws an exception and returns the
    exception.
*)
let forever f = 
  let rec forever () =
    f ();
    forever ()
  in 
  try forever ()
  with e -> e

(**
    Also known as the `Psi` combinator
*)
let on f g x y = f (g x) (g y) 

(**

     Also known as the `S` or `Starling` combinator, substitution, split, ap, <*>.
*)
let ap f g x = f x (g x) 



(**
     Also known as the `S'` or `Starling prime` combinator, big phi.
*)
let liftA2 f g h x = f (g x) (h x)

(**
     Also known as the `J` or `Jaybird` combinator.
*)
let jay f x y z = f x (f z y) 

(**

      Also known as the `B'` or `BlackBird` combinator.
*)
let blackbird f g x y = f (g x y)

(**

     Also known as the `Y` combinator.
*)
let fix f =
  (fun (`X x)  -> f (x (`X x)))
    (`X (fun (`X x)  -> fun y  -> f (x (`X x)) y))