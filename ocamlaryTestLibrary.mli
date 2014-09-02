(** This is an {i interface} with {b all} of the {e module system} features.
    {C This text is centered. }
    {L This text is left-aligned. }
    {R This text is right-aligned. }
    This documentation demonstrates:
- comment formatting
- unassociated comments
- documentation sections
- module system documentation including {ol
 {- submodules}
 {- module aliases}
 {- module types}
 {- module type aliases}
 {- modules with signatures}
 {- modules with aliased signatures}
}
*)

(**
    You may find more information about this HTML documentation renderer
    at {{:https://github.com/dsheets/ocamlary} github.com/dsheets/ocamlary }.
*)

(**
   This is some verbatim text: {v verbatim v}
*)

(**
    This is some verbatim text: {v [][df[]]}} v}
*)

(**
    Here is some raw LaTeX: {% $e^{i\pi} = -1$ %}
*)

(**
    Here is an index table of [Empty] modules: {!modules:Empty EmptyAlias}
*)

(**
    Here is a table of links to indexes: {!indexlist}
*)

(**
    Here is some superscript: x{^2}
*)

(**
    Here is some subscript: x{_0}
*)

(**
    Here are some escaped brackets: \{ \[ \@ \] \}
*)

(**
   David Sheets is the author.
   @author David Sheets
*)

(** An unassociated comment *)
(******************************************************************************)

(** {0 Level 0 } *)
(** {1 Level 1 } *)
(** {2 Level 2 } *)
(** {3 Level 3 } *)
(** {4 Level 4 } *)
(** {5 Level 5 } *)
(** {6 Level 6 } *)
(** {7 Level 7 } *)
(** {8 Level 8 } *)
(** {9 Level 9 } *)

(** {3 Basic module stuff} *)

(** A plain, empty module *)
module Empty : sig end
(** This module has a signature without any members. *)

(** {9000:s9000 Level 9000 } *)

(** A plain module alias of [Empty] *)
module EmptyAlias = Empty

(** {3:EmptySig EmptySig *)

(** A plain, empty module signature *)
module type EmptySig = sig end

(** A plain, empty module signature alias of {[EmptySig]} (preformatted). *)
module type EmptySigAlias = EmptySig

(** A plain module of a signature of {!EmptySig} (reference) *)
module ModuleWithSignature : EmptySig

(** A plain module with an alias signature
    @deprecated I don't like this element any more.
*)
module ModuleWithSignatureAlias : EmptySigAlias

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

(** For a good time, see {!SubSig} (general) or
    {!SuperSig.SubSigA.SubSig} or {!SuperSig.SubSigB.SubSig} or
    {!SuperSig.EmptySig}. Section {!s9000} is also
    interesting. {!EmptySig} is a general reference but
    {!section:EmptySig} is the section and {!modtype:EmptySig} is the
    module signature. *)

(** Some text before exception title. {3 Basic exception stuff} After exception title. *)

(** Unary exception constructor *)
exception Kaboom of unit

(** Binary exception constructor *)
exception Kablam of unit * unit

(** Unary exception constructor over binary tuple *)
exception Kapow  of (unit * unit)

(** {!EmptySig} is general but {!modtype:EmptySig} is a module and
    {!exception:EmptySig} is this exception. *)
exception EmptySig

(** {!exception:EmptySigAlias} is this exception. *)
exception EmptySigAlias

(** {3 Basic type and value stuff with advanced doc comments } *)

(** {!a_function} is general but {!type:a_function} is this type and
    {!val:a_function} is the value below. *)
type ('a,'b) a_function = 'a -> 'b

(**
   This is [a_function] with param and return type.
   @param x the [x] coordinate
   @return the [y] coordinate
*)
val a_function : x:int -> int

val fun_fun_fun : ((int, int) a_function, (unit, unit) a_function) a_function

val fun_maybe : ?yes:unit -> unit -> int

(** @raise Not_found That's all it does *)
val not_found : unit -> unit

(** @see < http://ocaml.org/ > The OCaml Web site *)
val ocaml_org : string

(** @see 'some_file' The file called [some_file] *)
val some_file : string

(** @see "some_doc" The document called [some_doc] *)
val some_doc : string

(**
   This value was introduced in the Mesozoic era.
   @since mesozoic
*)
val since_mesozoic : unit

(**
   This value has had changes in 1.0.0, 1.1.0, and 1.2.0.
   @before 1.0.0 before 1.0.0
   @before 1.1.0 before 1.1.0
   @version 1.2.0
*)
val changing : unit

(** This value has a custom tag [foo].
    @foo the body of the custom [foo] tag
*)
val with_foo : unit

(** {3 Advanced Module Stuff} *)

(** This comment is for [CollectionModule]. *)
module CollectionModule : sig
  (** This comment is for [collection]. *)
  type collection
  (** This comment is for [InnerModuleA]. *)
  module InnerModuleA : sig
    (** This comment is for [t]. *)
    type t = collection
    (** This comment is for [InnerModuleA']. *)
    module InnerModuleA' : sig
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

(* TODO: figure out why this doesn't work

(** This comment is for [Functor]. *)
module Functor(EmptyAlias : EmptySigAlias) : sig
  (** This comment is for [FunctorInner]. *)
  module FunctorInner = EmptyAlias
end
*)

(** TODO: re-enable functor after
   {{:https://github.com/lpw25/opam-doc-base/issues/25} lpw25/opam-doc-base#25} is fixed

(** This comment is for [FunctorTypeOf]. *)
module FunctorTypeOf(Collection : module type of CollectionModule) : sig
  (** This comment is for [t]. *)
  type t = Collection.collection
end
*)

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

(** TODO: re-enable polymorphic variant after
    {{:https://github.com/lpw25/opam-doc-base/issues/27} lpw25/opam-doc-base#27} is fixed

(** This comment is for [poly_variant]. *)
type poly_variant = [
| `TagA (** This comment is for [`TagA]. *)
| `ConstrB of int (** This comment is for [`ConstrB]. *)
]
(** Wow! It was a polymorphic variant! *)
*)

(** This comment is for [full_gadt]. *)
type (_,_) full_gadt =
| Tag : (unit,unit) full_gadt
| First : 'a -> ('a,unit) full_gadt
| Second : 'a -> (unit,'a) full_gadt
| Exist : 'b -> (unit, unit) full_gadt (** *)
(** Wow! It was a GADT! *)

(** This comment is for [partial_gadt]. *)
type 'a partial_gadt =
| AscribeTag : 'a partial_gadt
| OfTag of 'a partial_gadt
| ExistGadtTag : ('a -> 'b) -> 'a partial_gadt (** *)
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

(** TODO: re-enable polymorphic variant union after
    {{:https://github.com/lpw25/opam-doc-base/issues/27} lpw25/opam-doc-base#27} is fixed

(** This comment is for [poly_variant_union]. *)
type poly_variant_union = [
| poly_variant
| `TagC
]

also check constraints and ambiguities
*)

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

(* TODO: classes, class types, packed modules, open types, ...? *)
