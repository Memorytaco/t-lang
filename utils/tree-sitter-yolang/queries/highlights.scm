;; common
(string) @string
(number) @number
(comment) @comment
(force_id) @string.escape
["," ";;"] @punctuation.delimiter

;; module
[(absolute_id) (qualified_id) (namespace)] @module
(module "module" @keyword)
(export "module" @keyword)
(export "exclude" @keyword)
(module "*"  @keyword.operator)
(export "*"  @keyword.operator)
(use_keyword_star) @keyword.operator
(module_annotation) @keyword

(use "use" @keyword)
(piece ".." @keyword)

[(use_keyword_exclude) (use_keyword_self) (use_keyword_alias) (use_keyword_as)] @keyword
(module "exclude" @keyword)

;; data
[(newtype_evidence) (newtype_reverse_evidence)] @constructor
(data ["data" "of" "deriving" "via"] @keyword)
(data "|" @keyword.operator)
(data ":" @keyword.operator)
(data (unit) @type.builtin)
(data (tuple_type "(" @type.builtin "," ")" @type.builtin))
(data (data_constructor) @constructor)
(data (data_accessor) @variable.member)
(data (region_id) @type.builtin)
(data (field) @keyword.modifier)
(data_repr "=" @keyword)
(data_repr [(field) (value)] @keyword.modifier)
(data_repr (annotation) @keyword.directive)

;; type alias
(type_alias ["type" "="] @keyword)
(type_alias (name) @type)

(primitive_type "#!(" @type.builtin ")" @type.builtin)

(record_field) @variable.member

(type ["forall" ":" "."] @keyword)
(type "=" @type.builtin)
(type "~" @type.builtin)
(type (unit) @type.builtin)
(type (tuple_type "(" @type.builtin "," ")" @type.builtin))
(type_effect_suffix "!{" @keyword.operator "}" @keyword.operator)
(type_eff_name) @type.builtin
(type "forall" "{" @keyword (region_id) @type.builtin "}" @keyword)
(type_var (region_id) @type.builtin)
(type_reserved_operator) @type.builtin
(type (operator) @operator)
(primitive_compound (operator) @operator)
(primitive_compound (integer) @number)
(type_effect_suffix ".." @keyword)
(handler_type [";;" ".."] @keyword)
(handler_type "{" @type.builtin "}" @type.builtin)
(record_type "=" @keyword)
(record_type "{" @type.builtin "}" @type.builtin)

(primitive_reverse_atom_prim) @type.builtin
(primitive_reverse_atom) @keyword.operator
(type_implicit_var) @type.builtin
(type_constructor) @type.definition
(type_param) @variable.parameter

(primitive_reverse_prim "#!(" @keyword.operator ")" @keyword.operator)

(primitive_compound ";" @keyword.operator)

(data "{" @keyword.operator "," @keyword.operator "}" @keyword.operator)

;; top level value definition
(definition ["let" "do" "=" "|" ":"] @keyword)
(definition [(operator_id)] @operator)

;; expression
(expression [(operator) (force_operator_id)] @operator)
(expression "forall" @keyword "." @keyword)
(expression "auto" @keyword)
(expression [":" "|" "[" "]"] @keyword)
(expression "(" @keyword.operator "," ")" @keyword.operator)
(expression (unlift_type "@(" @keyword.operator ")" @keyword.operator))
(expression (unlift_type "@" @keyword.operator))
(expression (unlift_type (unlift_var) @type.builtin))
(expression (unit) @constructor)
(expression ["let" "match" "=" "do" "with" "in" "or" "%null"] @keyword)
(expression (region_id) @type)
(do_block ["->" "=" ";;"] @keyword)
(do_block [","] @keyword.operator)
(string_cons (string_cons_var) @attribute)
(handler ["forall" "with" "auto" "|"] @keyword)

;; operator fixity
(fixity "_" @keyword)
(fixity ["fixity" "postfix" "prefix" "infix"] @keyword)
(fixity [(operator) (operator_id) (force_operator_id)] @operator)

;; pattern
(pattern [(pattern_wildcard) ":" "%null"] @keyword)
(pattern_var ["?"] @keyword.operator) @variable
(pattern (pattern_cons) @constructor)
(pattern (unit) @constructor)
(pattern "(" @keyword.operator "," ")" @keyword.operator)
(pattern "@" @keyword.operator)
(pattern "@" "(" @keyword.operator "," ")" @keyword.operator)
(pattern ["<-"] @keyword)
(pattern (region_id) @type)
(or_pattern ["or" "return" "{" ".." "}" "="] @keyword)

;; binding
(binding ["pattern" "<-" ":" "{" "}"] @keyword)
(binding (cons) @constructor)
(binding (id) @variable.member)

;; effects
(effect ["effect" "auto" "forall"] @keyword)
(effect ["." ":"] @keyword.operator)
(effect "{" @keyword "}" @keyword)
(effect (method) @variable.member @markup.italic)
(effect (named) @variable.member @markup.italic @markup.strong)
(effect (name) @markup.strong)
(effect (region_id) @type.builtin)
(handler ["handler" ":"] @keyword)
(handler (handler_resume) @variable.builtin)
(handler "=" @keyword)
(handler (method) @variable.member)

;; type class
(typeclass ["default" "class" "forall" "=>" "|" ":" "."] @keyword)
(typeclass (field) @variable.member)
(typeclass (classname) @type.definition)

;; class instance
(instance_chain ["instance" "else"] @keyword)
(instance ["forall" "fail" "=>" "=" "|" ":" "."] @keyword)
(instance (name) @label)
(instance (classname) @type @markup.italic @markup.strong)
(instance (field) @variable.member)

;; constraint
(constraint ["rule" "<=>" "==>" "\\" "|" "@" "." "forall"] @keyword)
(constraint (property) @variable.member)
(constraint (rulename) @constructor)
(chr [(operator) (force_operator_id)] @operator)

;; macro
(macro_id) @variable.parameter.builtin
(macro ["#[" "]"] @punctuation.special @markup.strong)
(macro_dict ["{" "}" ":" ".."] @keyword)
(macro_expr "@(" @keyword.operator ")" @keyword.operator)
(macro_expr "@" @keyword.operator)
(macro_block [(name) "}"] @function.macro @markup.italic)
(macro_block (operator) @operator)
(macro_block (name) ["(" ")" "[" "]" "{" "}" "|" "," ":" "="] @keyword.operator @markup.italic "}")

;; foreign FFI
(foreign ["foreign" "import" "export" "|" "=" ":"] @keyword)
(foreign ["[" "]"] @function.macro)
(foreign (rawname) @keyword.function)
(foreign (name) @function)

;; region
(region ["region" "open" "close" "with" "=" "_" "of" "|"] @keyword)
(region [(region_id) (repr)] @type)
(region (from) @constructor)
(region (to) @variable.member)
