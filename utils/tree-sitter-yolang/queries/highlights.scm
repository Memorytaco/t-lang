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

(type "forall" @keyword)
(type "=" @type.builtin)
(type "~" @type.builtin)
(type ":" @keyword.operator)
(type_unit) @type.builtin
(type_tuple "(" @type.builtin "," ")" @type.builtin)
(type_effect_suffix "!{" @keyword.operator "}" @keyword.operator)
(type_eff_name) @type.builtin
(type_reserved_function) @type.builtin
(type (operator) @operator)
(primitive_compound (operator) @operator)

(primitive_reverse_atom_prim) @type.builtin
(primitive_reverse_atom) @keyword.operator
(type_implicit_var) @attribute
(type_constructor) @type.definition
(type_param) @variable.parameter

(primitive_reverse_prim "#!(" @keyword.operator ")" @keyword.operator)

(primitive_compound ";" @keyword.operator)

(data "{" @keyword.operator "," @keyword.operator "}" @keyword.operator)

;; binding
(let_binding "let" @keyword)
(let_binding "=" @keyword.operator)
(let_binding "|" @keyword.operator)

;; expression
(expression (number) @number)
(expression ":" @keyword.operator)
(expression "forall" @keyword "." @keyword.operator)
(expression ["<-" "|" "="] @keyword.operator)
(expression ["let" "do" "with" "in" "or" "const"] @keyword)
(string_cons (string_cons_var) @attribute)

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

;; macro
(macro_id) @variable.parameter.builtin
