(** An interface with all of the module system features *)

module type Empty = sig type t end

(** An ambiguous, misnamed module type *)
module type MissingComment = sig type t end

(** A plain, empty module. *)
module Empty = struct end

(** A plain module alias. *)
module EmptyAlias = Empty

(** A plain, empty module signature. *)
module type EmptySig = sig end

(** A plain, empty module signature alias. *)
module type EmptySigAlias = EmptySig

(** A plain module of a signature. *)
module ModuleWithSignature = struct end

(** A plain module with an alias signature. *)
module ModuleWithSignatureAlias = struct end

(** has type "one" *)
module One = struct type one end

(** There's a module in this signature. *)
module type SigForMod = sig
  module Inner : sig
    module type Empty = sig end
  end
end

module type SuperSig = sig
  module type SubSigA = sig
    (** {3:SubSig A Labeled Section Header Inside of a Signature *)

    type t

    module SubSigAMod : sig
      type sub_sig_a_mod
    end
  end
  module type SubSigB = sig
    (** {3:SubSig Another Labeled Section Header Inside of a Signature *)

    type t
  end
  module type EmptySig = sig
    type not_actually_empty
  end
  module type One = sig type two end
  module type SuperSig = sig end
end

(** {!Buffer.t} *)
module Buffer = struct
  let f _ = ()
end

(** Unary exception constructor *)
exception Kaboom of unit

(** Binary exception constructor *)
exception Kablam of unit * unit

(** Unary exception constructor over binary tuple *)
exception Kapow  of (unit * unit)

(** {!EmptySig} is general but {!module:EmptySig} is a module and
    {!exception:EmptySig} is this exception. *)
exception EmptySig

(** {!exception:EmptySigAlias} is this exception. *)
exception EmptySigAlias

module type Mtv = sig
  exception EmptySigAlias
  (** {!EmptySigAlias} is general and {!exception:EmptySigAlias} is an
      exception and {!modtype:EmptySigAlias} is a module type. *)
end

(** {!a_function} is general but {!type:a_function} is this type and
    {!val:a_function} is the value below. *)
type (-'a,+'b) a_function = 'a -> 'b

(**
   @param x the [x] coordinate
   @return the [y] coordinate
*)
let a_function ~x = x

let fun_fun_fun int_fun = (fun () -> ())

let fun_maybe ?yes () = 0

let tuple_pyramid = (0,(1,(2,3),4),5)

let tuple_fun = (~-), 2

let fun_higher _f = ()

let fun_maybe_higher ?maybe = (fun () -> ())

let fun_labeled_higher ~f = ()

let fun_default ?(default=[]) () = List.length default

(** @raise Not_found That's all it does *)
let not_found () = raise Not_found

(** @see < http://ocaml.org/ > The OCaml Web site *)
let ocaml_org = "http://ocaml.org/"

(** @see 'some_file' The file called [some_file] *)
let some_file = "some_file"

(** @see "some_doc" The document called [some_doc] *)
let some_doc = "some_doc"

(**
   This value was introduced in the Mesozoic era.
   @since mesozoic
*)
let since_mesozoic = ()

(**
   This value has had changes in 1.0.0, 1.1.0, and 1.2.0.
   @before 1.0.0 before 1.0.0
   @before 1.1.0 before 1.1.0
   @version 1.2.0
*)
let changing = ()

(** This value has a custom tag [foo].
    @foo the body of the custom [foo] tag
*)
let with_foo = ()

(** {3 Some Operators } *)

let ( ~- ) = ()
let ( ! )  = ()
let ( @ )  = ()
let ( $ )  = ()
let ( % )  = ()
let ( ^ )  = ()
let ( & )  = ()
let ( * )  = ()
let ( - )  = ()
let ( + )  = ()
let ( < )  = ()
let ( > )  = ()
let ( -? ) = ()
let ( / )  = ()
let ( -| ) = ()
let ( := ) = ()
let ( = )  = ()

(** {3 Advanced Module Stuff} *)

(** This comment is for [CollectionModule]. *)
module CollectionModule = struct
  (** This comment is for [collection]. *)
  type collection
  type element
  type other

  (** This comment is for [InnerModuleA]. *)
  module InnerModuleA = struct
    (** This comment is for [t]. *)
    type t = collection
    (** This comment is for [InnerModuleA']. *)
    module InnerModuleA' = struct
      (** This comment is for [t]. *)
      type t = (unit,unit) a_function
    end

    (** This comment is for [InnerModuleTypeA']. *)
    module type InnerModuleTypeA' = sig
      (** This comment is for [t]. *)
      type t = InnerModuleA'.t
    end
  end
  (** This comment is for [InnerModuleTypeA]. *)
  module type InnerModuleTypeA = InnerModuleA.InnerModuleTypeA'
end

(** module type of *)
module type COLLECTION = module type of CollectionModule

module Recollection(C : COLLECTION) :
  COLLECTION with type collection = C.element list and type element = C.collection = struct
  type collection = C.element list
  type element = C.collection
  type other = C.other

  (** This comment is for [InnerModuleA]. *)
  module InnerModuleA = struct
    (** This comment is for [t]. *)
    type t = collection
    (** This comment is for [InnerModuleA']. *)
    module InnerModuleA' = struct
      (** This comment is for [t]. *)
      type t = (unit,unit) a_function
    end

    (** This comment is for [InnerModuleTypeA']. *)
    module type InnerModuleTypeA' = sig
      (** This comment is for [t]. *)
      type t = InnerModuleA'.t
    end
  end
  (** This comment is for [InnerModuleTypeA]. *)
  module type InnerModuleTypeA = InnerModuleA.InnerModuleTypeA'
end

module Generative() = struct end

module type FUNCTOR_TYPE = functor (M : Map.S) -> sig end

module type GENERATIVE_FUNCTOR_TYPE = functor () -> sig end

module type MMM = sig module C : COLLECTION end

module type RECOLLECTION = MMM with module C = Recollection(CollectionModule)

module type PATH_SUBST =
  MMM with type C.element = int and type C.collection = int list

module type MOD_SUBST = MMM with module C := CollectionModule

module type TYPE_SUBST =
  COLLECTION with type element := int and type collection := unit

module type PRIVATE = COLLECTION
  with type element = private int and type collection = private int list

module type RecollectionModule = sig
  include module type of Recollection(CollectionModule)
end

module type A = sig
  type t
  module Q : COLLECTION
end

module type B = sig
  type t
  module Q : COLLECTION
end

module type C = sig
  include A
  include B with type t := t and module Q := Q
end

(*
(** This comment is for [Functor]. *)
module Functor(EmptyAlias : EmptySigAlias) = struct
  (** This comment is for [FunctorInner]. *)
  module FunctorInner = EmptyAlias
end
*)

(** This comment is for [FunctorTypeOf]. *)
module FunctorTypeOf(Collection : module type of CollectionModule) = struct
  (** This comment is for [t]. *)
  type t = Collection.collection
end

(** This comment is for [IncludeModuleType]. *)
module type IncludeModuleType = sig
  (** This comment is for [include EmptySigAlias]. *)
  include EmptySigAlias
end

(** {3 Advanced Type Stuff} *)

type private_i = private int
(** A private integer. *)

(** This comment is for [record]. *)
type record = {
  field1 : int; (** This comment is for [field1]. *)
  field2 : int; (** This comment is for [field2]. *)
}
(** This comment is also for [record]. *)

type mutable_record = {
  mutable a : int; (** [a] is first and mutable *)
  b : unit; (** [b] is second and immutable *)
  mutable c : int; (** [c] is third and mutable *)
}

type universe_record = {
  nihilate : 'a. 'a -> unit;
}

(** This comment is for [variant]. *)
type variant =
| TagA (** This comment is for [TagA]. *)
| ConstrB of int (** This comment is for [ConstrB]. *)
| ConstrC of int * int (** This comment is for binary [ConstrC]. *)
| ConstrD of (int * int)
(** This comment is for unary [ConstrD] of binary tuple. *)
| ConstrE
(** This comment is also for [variant]. *)

(** This comment is for [poly_variant]. *)
type poly_variant = [
| `TagA (** This comment is for [`TagA]. *)
| `ConstrB of int (** This comment is for [`ConstrB]. *)
]
(** Wow! It was a polymorphic variant! *)

(** Unary gadt *)
type _ gadt = Unary_gadt_cons : unit gadt

(** This comment is for [full_gadt]. *)
type (_,_) full_gadt =
| Tag : (unit,unit) full_gadt
| First : 'a -> ('a,unit) full_gadt
| Second : 'a -> (unit,'a) full_gadt
| Exist : 'b -> (unit, unit) full_gadt
(** Wow! It was a GADT! *)

(** This comment is for [partial_gadt]. *)
type 'a partial_gadt =
| AscribeTag : 'a partial_gadt
| OfTag of 'a partial_gadt
| ExistGadtTag : ('a -> 'b) -> 'a partial_gadt
(** Wow! It was a mixed GADT! *)

(** This comment is for [alias]. *)
type alias = variant

(** This comment is for [tuple]. *)
type tuple = (alias * alias) * alias * (alias * alias)

(** This comment is for [variant_alias]. *)
type variant_alias = variant =
| TagA
| ConstrB of int
| ConstrC of int * int
| ConstrD of (int * int)
| ConstrE

(** This comment is for [record_alias]. *)
type record_alias = record = {
  field1 : int;
  field2 : int;
}

(** This comment is for [poly_variant_union]. *)
type poly_variant_union = [
| poly_variant
| `TagC
]

type 'a poly_poly_variant = [
| `TagA of 'a
]

type ('a,'b) bin_poly_poly_variant = [
| `TagA of 'a
| `ConstrB of 'b
]

(* TODO: figure out how to spec a conjunctive type
type amb_poly_variant = [
| unit poly_poly_variant
| (int,unit) bin_poly_poly_variant
| `TagC
]
*)

type 'a open_poly_variant  = [> `TagA ] as 'a

type 'a open_poly_variant2 = [> `ConstrB of int ] as 'a

type 'a open_poly_variant_alias = 'a open_poly_variant open_poly_variant2

type 'a poly_fun = ([> `ConstrB of int ] as 'a) -> 'a

type 'a poly_fun_constraint = 'a -> 'a constraint 'a = [> `TagA ]

type 'a closed_poly_variant = [< `One | `Two ] as 'a

type 'a clopen_poly_variant =
[< `One | `Two of int | `Three > `Two `Three] as 'a

type nested_poly_variant = [
| `A
| `B of [
  | `B1
  | `B2
]
| `C
| `D of [
  | `D1 of [
    `D1a
  ]
]
]

(** This comment is for [full_gadt_alias]. *)
type ('a,'b) full_gadt_alias = ('a,'b) full_gadt =
| Tag : (unit,unit) full_gadt_alias
| First : 'a -> ('a,unit) full_gadt_alias
| Second : 'a -> (unit,'a) full_gadt_alias
| Exist : 'b -> (unit, unit) full_gadt_alias

(** This comment is for [partial_gadt_alias]. *)
type 'a partial_gadt_alias = 'a partial_gadt =
| AscribeTag : 'a partial_gadt_alias
| OfTag of 'a partial_gadt_alias
| ExistGadtTag : ('a -> 'b) -> 'a partial_gadt_alias

(** This comment is for {!exn_arrow}. *)
exception Exn_arrow : unit -> exn

exception Exn_higher : (unit -> unit) -> exn

exception Exn_tuple : (int * int) -> exn

exception Exn_binary : int * int -> exn

(** This comment is for {!mutual_constr_a} and {!mutual_constr_b}. *)
type mutual_constr_a =
| A
| B_ish of mutual_constr_b
and mutual_constr_b =
| B
| A_ish of mutual_constr_a

type rec_obj = < f : int; g : unit -> unit; h : rec_obj >

type 'a open_obj = < f : int; g : unit -> unit; .. > as 'a

type 'a oof = (< a : unit; .. > as 'a) -> 'a

type 'a any_obj = < .. > as 'a

type empty_obj = < >

type one_meth = < meth: unit >

(** A mystery wrapped in an ellipsis *)
type ext = ..

type ext += ExtA
type ext += ExtB
type ext +=
| ExtC of unit
| ExtD of ext
type ext += ExtE

type ext += private ExtF
(** {!ExtF} is private *)

type ext +=
| ExtG of int * int
| ExtH of (int * int)
| ExtI : int * (int -> int) -> ext
| ExtJ : (int -> int) -> ext
| ExtK : (int * int) * (int * int) -> ext

type 'a poly_ext = ..
(** 'a poly_ext *)

type 'b poly_ext += Foo of 'b | Bar of 'b * 'b | Quux of ('b * 'b)
(** 'b poly_ext *)

module ExtMod = struct
  type t = ..

  type t += Leisureforce
end

type ExtMod.t += ZzzTop
(** It's got the rock *)

type ExtMod.t += ZzzTop of unit
(** and it packs a unit. *)

(** Rotate keys on my mark... *)
external launch_missiles : unit -> unit = "tetris"

(** One ping only. *)
external dive : unit -> unit = "deep" "sea"

(** We didn't start the fire. *)
external light : unit -> unit = "byte" "native" "noalloc"

(** A brown paper package tied up with string*)
type my_mod = (module COLLECTION)

type my_mod_with_type = (module COLLECTION with type element = int)
type my_mod_with_type2 =
  (module COLLECTION with type element = int and type collection = int list)
type my_mod_with_type3 =
  (module COLLECTION
    with type element = int
    and type collection = int list
    and type other = unit)

class empty_class = object val x = 0 end

class one_method_class = object
  method go = ()
end

class two_method_class = object
  method one = new one_method_class
  method undo = ()
end
(** A class with two methods. *)

class ['a] param_class (a : 'a) = object
  method v = a
end
(** A class with a type parameter. *)

class ['a,'b] param2_class (a : 'a) (b : 'b) = object
  method p = (a,b)
end
(** A class with two type parameters. *)

type my_unit_object = unit param_class

type 'a my_unit_class = unit #param_class as 'a

class type my_unit_class_type = [unit] param_class

class type empty_class_type = object end
(** An empty class type. *)

class type self_class_type = object ('self)
  val self : 'self
end
(** A class type using self and instance variable. *)

class type private_class_type = object
  method private v : int
end
(** A class type using a private instance variable. *)

class type virtual virtual_class_type = object
  val virtual v : int
end
(** A class type using a virtual instance variable. *)

class type virtual inherit_class_type = object
  inherit virtual_class_type
  inherit self_class_type
  inherit virtual_class_type
end
(** A class type inheriting {{!virtual_class_type}virtual},
    {{!self_class_type}self}, virtual. *)

class virtual virtual_class : virtual_class_type = object
  val virtual v : int
end
(** A class inheriting {!virtual_class_type}. *)

class virtual virtual_class_2 : virtual_class_type = object
  val virtual v : int
end
(** A class of type {!virtual_class_type}. *)

class virtual virtual_mutable_class_with_stop = object
  val mutable mutable_literal = 3
  val mutable virtual mutable_virtual_literal : int
  val virtual virtual_literal : int

(**/*)
  val hidden_literal = 4

(** Hidden comment *)

end

(** Comment after {!virtual_mutable_class_with_stop} *)

module Stop_module = struct

  let value = 0
  (** {!value} is an int *)

  (** Visible comment *)

  (**/**)
  let hidden_value = 1

  (** Hidden comment *)

end

(** Comment after {!Stop_module} *)

module type SIMPLE = sig
  type t
  type u
end

module Simple1 = struct type t type u end
module Simple2 = struct type t = int type u end
module Simple3tu = struct type t = int type u = int end
module Simple3ut = struct type u = int type t = int end
module Simple4 = struct type u end
module Simple5 = struct type t end
module Simple6tu = struct end
module Simple6ut = struct end

type simple_t = Simple1.t
type simple_u = Simple1.u

type package = (module SIMPLE with type t = int)
type package_tu = (module SIMPLE with type t = int and type u = int)
type package_ut = (module SIMPLE with type u = int and type t = int)

module type ALIAS = SIMPLE

module Alias6ut = struct end

module Simple = struct
  type t
  type u
end

module Alias = Simple

type alias_t = Alias.t
type alias_u = Alias.u

(* Test resolution of dependently typed modules *)
module Dep1 = struct
  module type S = sig
    class c : object
      method m : int
    end
  end
  module X = struct
    module Y = struct
      class c = object
        method m = 4
      end
    end
  end
end

module Dep2 (Arg : sig module type S module X : sig module Y : S end end) =
struct
  module A = Arg.X
  module B = A.Y
end

type dep1 = Dep2(Dep1).B.c

module Dep3 = struct type a end

module Dep4 = struct
  module type T = sig type b end
  module type S = sig
    module X : T
    module Y : sig end
  end
  module X = struct type b end
end

module Dep5 (Arg : sig
  module type T
  module type S = sig
    module X : T
    module Y : sig end
  end
  module X : T
end) = struct
  module Z : Arg.S with module Y = Dep3 = struct
    module X = Arg.X
    module Y = Dep3
  end
end

type dep2 = Dep5(Dep4).Z.X.b

type dep3 = Dep5(Dep4).Z.Y.a

module Dep6 = struct
  module type S = sig type d end
  module type T = sig
    module type R = S
    module Y : R
  end
  module X = struct
    module type R = S
    module Y = struct type d end
  end
end

module Dep7 (Arg : sig
  module type S
  module type T = sig
    module type R = S
    module Y : R
  end
  module X : T
end) = struct
  module M = Arg.X
end

type dep4 = Dep7(Dep6).M.Y.d

module Dep8 = struct
  module type T = sig type t end
end

module Dep9(X : sig module type T end) = X

module type Dep10 = Dep9(Dep8).T with type t = int

module Dep11 = struct
  module type S = sig
    class c : object
      method m : int
    end
  end
end

module Dep12 =
  functor (Arg : sig module type S end) -> struct
    module type T = Arg.S
  end

module Dep13 = struct
  class c = object
    method m = 4
  end
end

type dep5 = Dep13.c

module type With1 = sig
  module M : sig
    module type S
  end
  module N : M.S
end

module With2 = struct
  module type S = sig type t end
end

module With3 = struct
  module M = With2
  module N = struct
    type t = int
  end
end

type with1 = With3.N.t

module With4 = struct
  module N = struct
    type t = int
  end
end

type with2 = With4.N.t
