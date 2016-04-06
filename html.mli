type t

val alert: string -> unit

val text: string -> t
val text': string -> t * (string -> unit)
val img: ?class_: string -> ?alt: string -> string -> t
val a: ?class_: string -> ?href: string -> t list -> t
val div: ?class_: string -> t list -> t
val div': ?class_: string -> t list -> t * (t list -> unit)
val span: ?class_: string -> t list -> t
val span': ?class_: string -> t list -> t * (t list -> unit)
val checkbox_input: ?class_: string -> ?on_change: (bool -> unit) -> bool -> t
val checkbox_input':
  ?class_: string -> ?on_change: (bool -> unit) -> bool -> t * (bool -> unit)
val text_input: ?class_: string -> ?on_change: (string -> unit) -> string -> t
val text_input':
  ?class_: string -> ?on_change: (string -> unit) -> string ->
  t * (string -> unit)

val run: (unit -> t) -> unit

val get_hash: unit -> string
val set_hash: string -> unit
val on_hash_change: (unit -> unit) -> unit
