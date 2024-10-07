;; common
(string) @string
(comment) @comment
(operator_id) @keyword.operator
(force_id) @string.escape

;; module
[(module_id) (qualified_id) (module_path) (use_path)] @module
[","] @punctuation.delimiter
["use" "data" ] @keyword
(module "module" @keyword)
(module_export "module" @keyword)
(module_export "exclude" @keyword)
(module "*"  @keyword.operator)
(module_export "*"  @keyword.operator)
(use_keyword_star) @keyword.operator
(module_annotation) @keyword

[(use_keyword_exclude) (use_keyword_self) (use_keyword_alias) (use_keyword_as)] @keyword
(module "exclude" @keyword)

;; data
[(newtype_evidence) (newtype_reverse_evidence)] @constructor
(data "of" @keyword)
(data "|" @keyword.operator)
(data ":" @keyword.operator)

(primitive_type
  "#!(" @type.builtin
  ")" @type.builtin)

(record_field) @variable.member

(type ["forall" ":" "."] @keyword)
(type "=" @type.builtin)
(type "~" @type.builtin)
(type_unit) @type.builtin
(type_tuple "(" @type.builtin "," ")" @type.builtin)
(type_effect_suffix "!{" @keyword.operator "}" @keyword.operator)
(type_eff_name) @type.builtin
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
(type_implicit_var) @attribute
(type_constructor) @type.definition
(type_param) @variable.parameter

(primitive_reverse_prim "#!(" @keyword.operator ")" @keyword.operator)

(primitive_compound ";" @keyword.operator)

(data "{" @keyword.operator "," @keyword.operator "}" @keyword.operator)

;; binding
(let_binding ["let" "do" "=" "|"] @keyword)

;; expression
(expression (number) @number)
(expression "forall" @keyword "." @keyword)
(expression [":" "|" "[" "]"] @keyword)
(do_block ["let" "const" "<-" "|" "="] @keyword)
(expression ["let" "do" "with" "in" "or"] @keyword)
(string_cons (string_cons_var) @attribute)
(handler ["forall" "|"] @keyword)

;; operator fixity
(fixity "_" @keyword.operator)
(fixity ["fixity" "postfix" "prefix" "infix"] @keyword)
(fixity (operator) @operator)

;; pattern
(pattern (pattern_wildcard) @keyword)
(pattern_var "?" @keyword.operator) @variable
(pattern (pattern_cons) @constructor)
(pattern (pattern_unit) @constant.builtin)
(pattern "(" @keyword.operator "," ")" @keyword.operator)
(pattern "@" @keyword.operator)
(pattern "@" "(" @keyword.operator "," ")" @keyword.operator)
(pattern (number) @number)
(pattern "!" @keyword.operator "->" @keyword.operator)
(or_pattern "or" @keyword)

;; effects
(effect "effect" @keyword)
(effect ["." ":"] @keyword.operator)
(effect "{" @keyword "}" @keyword)
(effect (method) @variable.member)
(handler "handler" @keyword)
(handler (handler_resume) @variable.builtin)
(handler "=" @keyword)
(handler (method) @variable.member)

;; macro
(macro_id) @variable.parameter.builtin
