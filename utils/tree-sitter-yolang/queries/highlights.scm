(string) @string
[(module_id) (qualified_id) (module_path) (use_path)] @module

(comment) @comment
[","] @punctuation.delimiter
["use" "data" ] @keyword
(module "module" @keyword)
(module_export "module" @keyword)
(module_export "exclude" @keyword)
(operator_id) @keyword.operator
(module "*"  @keyword.operator)
(module_export "*"  @keyword.operator)
(use_keyword_star) @keyword.operator

[(use_keyword_exclude) (use_keyword_self) (use_keyword_alias) (use_keyword_as)] @keyword
(module "exclude" @keyword)

