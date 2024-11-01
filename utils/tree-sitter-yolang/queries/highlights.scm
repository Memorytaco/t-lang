;; common
(string) @string
(comment) @comment
(force_id) @string.escape
[","] @punctuation.delimiter

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
(data "data" @keyword)
(data "of" @keyword)
(data "|" @keyword.operator)
(data ":" @keyword.operator)
(data (unit) @type.builtin)
(data (tuple_type "(" @type.builtin "," ")" @type.builtin))
(data (data_constructor) @constructor)
(data (type_annotation_var) @type.builtin)

(primitive_type "#!(" @type.builtin ")" @type.builtin)

(record_field) @variable.member

(type ["forall" ":" "."] @keyword)
(type "=" @type.builtin)
(type "~" @type.builtin)
(type (unit) @type.builtin)
(type (tuple_type "(" @type.builtin "," ")" @type.builtin))
(type_effect_suffix "!{" @keyword.operator "}" @keyword.operator)
(type_eff_name) @type.builtin
(type "forall" "{" @keyword (type_annotation_var) @type.builtin "}" @keyword)
(type_var (type_annotation_var) @type.builtin)
(type_reserved_operator) @type.builtin
(type (operator) @operator)
(primitive_compound (operator) @operator)
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

;; binding
(let_binding ["let" "do" "=" "|"] @keyword)

;; expression
(expression (number) @number)
(expression [(operator) (force_operator_id)] @operator)
(expression "forall" @keyword "." @keyword)
(expression "auto" @keyword)
(expression [":" "|" "[" "]"] @keyword)
(expression "(" @keyword.operator "," ")" @keyword.operator)
(expression (unlift_type "@(" @keyword.operator ")" @keyword.operator))
(expression (unlift_type "@" @keyword.operator))
(expression (unlift_type (unlift_var) @type.builtin))
(expression (unit) @constructor)
(expression ["let" "do" "with" "in" "or"] @keyword)
(do_block ["bind" "="] @keyword)
(string_cons (string_cons_var) @attribute)
(handler ["forall" "with" "auto" "|"] @keyword)

;; operator fixity
(fixity "_" @keyword)
(fixity ["fixity" "postfix" "prefix" "infix"] @keyword)
(fixity [(operator) (force_operator_id)] @operator)

;; pattern
(pattern (pattern_wildcard) @keyword)
(pattern_var ["?"] @keyword.operator) @variable
(pattern (pattern_cons) @constructor)
(pattern (unit) @constructor)
(pattern "(" @keyword.operator "," ")" @keyword.operator)
(pattern "@" @keyword.operator)
(pattern "@" "(" @keyword.operator "," ")" @keyword.operator)
(pattern (number) @number)
(pattern ["<-"] @keyword)
(or_pattern "or" @keyword)

;; binding
(binding ["bind" "<-" ":" "{" "}"] @keyword)
(binding (cons) @constructor)
(binding (id) @variable.member)

;; effects
(effect ["effect" "auto" "forall"] @keyword)
(effect ["." ":"] @keyword.operator)
(effect "{" @keyword "}" @keyword)
(effect (method) @variable.member)
(effect (type_annotation_var) @type.builtin)
(handler ["handler" ":"] @keyword)
(handler (handler_resume) @variable.builtin)
(handler "=" @keyword)
(handler (method) @variable.member)

;; constraint
(constraint ["rule" "<=>" "==>" "\\" "|" "@" "." "forall"] @keyword)
(constraint (property) @variable.member)
(constraint (rulename) @constructor)
(chr [(operator) (force_operator_id)] @operator)

;; macro
(macro_id) @variable.parameter.builtin
