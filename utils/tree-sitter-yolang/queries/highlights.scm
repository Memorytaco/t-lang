;; common
(string) @string
(comment) @comment
(operator_id) @keyword.operator

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

(primitive_type_expr
  "#!(" @type.builtin
  ")" @type.builtin)

(type_expr "forall" @keyword)
(type_expr "=" @type.builtin)
(type_expr "~" @type.builtin)
(type_expr ":" @keyword.operator)
(type_unit) @type.builtin
(type_tuple "(" @type.builtin "," ")" @type.builtin)
(type_effect_suffix "!{" @type.builtin "}" @type.builtin)
(type_eff_name) @type.builtin
(type_reserved_function) @type.builtin
(type_expr (operator) @operator)
(primitive_compound (operator) @operator)

(force_id) @string.escape

(primitive_reverse_atom_prim) @type.builtin
(primitive_reverse_atom) @keyword.operator
(type_implicit_var) @attribute
(type_constructor) @type.definition
(type_param) @variable.parameter

(primitive_reverse_prim "#!(" @keyword.operator ")" @keyword.operator)

(primitive_compound ";" @keyword.operator)

(data "{" @keyword.operator "," @keyword.operator "}" @keyword.operator)
