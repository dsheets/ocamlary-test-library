(** An interface with all of the module system features *)

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

module type SuperSig = sig
  module type SubSigA = sig
    (** {3:SubSig A Labeled Section Header Inside of a Signature *)

    type t
  end
  module type SubSigB = sig
    (** {3:SubSig Another Labeled Section Header Inside of a Signature *)

    type t
  end
  module type EmptySig = sig
    type not_actually_empty
  end
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

(** {!a_function} is general but {!type:a_function} is this type and
    {!val:a_function} is the value below. *)
type ('a,'b) a_function = 'a -> 'b

(**
   @param x the [x] coordinate
   @return the [y] coordinate
*)
let a_function ~x = x

let fun_fun_fun int_fun = (fun () -> ())

let fun_maybe ?yes () = 0

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

(** {3 Advanced Module Stuff} *)

(** This comment is for [CollectionModule]. *)
module CollectionModule = struct
  (** This comment is for [collection]. *)
  type collection
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

(** This comment is for [record]. *)
type record = {
  field1 : int; (** This comment is for [field1]. *)
  field2 : int; (** This comment is for [field2]. *)
}
(** This comment is also for [record]. *)

(** This comment is for [variant]. *)
type variant =
| TagA (** This comment is for [TagA]. *)
| ConstrB of int (** This comment is for [ConstrB]. *)
| ConstrC of int * int (** This comment is for binary [ConstrC]. *)
| ConstrD of (int * int)
(** This comment is for unary [ConstrD] of binary tuple. *)
(** This comment is also for [variant]. *)

(** This comment is for [poly_variant]. *)
type poly_variant = [
| `TagA (** This comment is for [`TagA]. *)
| `ConstrB of int (** This comment is for [`ConstrB]. *)
]
(** Wow! It was a polymorphic variant! *)

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
