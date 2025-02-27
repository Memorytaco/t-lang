/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const PRIORITY = {
  TYPE: 1,
  EXPRESSION: 1,
  PATTERN: 1,
  PRIMITIVE_TYPE: 2,
}

module.exports = grammar({
  name: "yolang",
  // extras: $ => [/\s|\\\r?\n/, $.comment],
  extras: $ => [/\s/, $.comment],
  conflicts: $ => [
    [$.expression, $.pattern_cons],
    [$.expression, $.pattern],
    [$.data_repr, $.type_var],
    [$.data_repr, $.tuple_type],
  ],
  rules: {
    source_file: $ => seq($.module, repeat($._top)),

    // top level items in a file, include all things here.
    _top: $ => choice(
      $.use,
      $.decorated,
    ),

    // module declaration and its exporting list
    module: $ => statement("module", optional($.attribute), $.namespace, optional(choice(
      // inside parenthesis
      wrapsep("(", ")", ",", choice(
        $.qualified_id,
        $.id,
        $.export,
        seq("*", optional(seq("exclude", wrapsep("(", ")", ",", choice(
          $.operator_id,
          $.force_id,
          $.id
        )))))
      )),
      // after everything *
      seq("*", optional(seq("exclude", wrapsep("(", ")", ",", choice(
        $.identifier,
        $.operator_id,
      )))))
    ))),
    module_annotation: _$ => /[A-Za-z_]\w*:/,
    export: $ => seq("module", choice(
      seq($.absolute_id, optional(choice(
        wrap("(", ")", sep(",", $.identifier)),
        seq("*", optional(seq("exclude", wrap("(", ")", sep(",", $.identifier)))))
      ))),
      wrap("(", ")", sep(",", $.absolute_id)),
      seq("*", optional(seq("exclude", wrap("(", ")", sep(",", $.absolute_id)))))
    )),

    // use statement
    use: $ => statement("use", optional($.attribute), optional($.module_annotation), $.piece),
    piece: $ => seq(
      $.namespace,
      optional(choice(
        seq(alias("as", $.use_keyword_as), $.id),
        seq(alias("alias", $.use_keyword_alias), $.id),
        alias("self", $.use_keyword_self))),
      optional(choice(
        wrap("{", "}", sep(",", choice($.piece, $.operator_id, $.force_id))),
        wrap("(", ")", choice(
          sep(",", choice($.id, $.operator_id, $.force_id)),
          ".."
        )),
        seq(
          alias("*", $.use_keyword_star),
          optional(seq(alias("exclude", $.use_keyword_exclude), wrapsep("(", ")", ",", choice(
            $.id, $.operator_id, $.force_id
          ))))
        )
      ))
    ),

    // C comment from https://github.com/tree-sitter/tree-sitter-c/blob/master/grammar.js
    comment: _$ => token(choice(
      seq('//', /(\\+(.|\r?\n)|[^\\\n])*/),
      seq( '/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/',))),

    macro: $ => wrapsep("#[", "]", ",", $.macro_expr),

    macro_expr: $ => prec.right(seq(
      choice(
        $.identifier, $.operator, $.operator_id, $.force_operator_id,
        $.string_cons, $.string, $.number,
        $.macro_dict, field("type", seq("@", token.immediate(/[A-Za-z_]\w*/))), wrap("@(", ")", $.type),
        wrapsep("(", ")", ",", $.macro_expr)
      ),
      repeat($.macro_expr)
    )),
    macro_dict: $ => wrapsep("{", "}", ",", choice(
      seq(alias($.id, $.key), ":", $.macro_expr),
      seq("..", $.macro_expr)
    )),

    declaration: $ => choice(
      $.data,
      $.type_alias,
      $.definition,
      $.fixity,
      $.effect,
      $.binding,
      $.constraint,
      $.typeclass,
      $.instance_chain,
      $.foreign,
      $.region,
    ),

    decorated: $ => seq(repeat($.macro), $.declaration),

    foreign: $ => statement("foreign", optional(choice("import", "export")),
      optional(wrapsep("[", "]", ",", $.macro_expr)),
      sep1(",", field("name", seq(
        alias($.id, $.name),
        optional(alias($.string, $.rawname)),
        optional(wrapsep("[", "]", ",", $.macro_expr))
      ))),
      ":", $.type,
      optional(choice(
        seq("=", $.expression),
        seq(wrapsep("|", "|", ",", $.or_pattern), $.expression),
      ))
    ),

    typeclass: $ => statement("class",
      optional(seq("forall", repeat1($.type_var), ".")),
      optional(seq(wrapsep("(", ")", ",", $.type), "=>")),
      field("class", alias($.id, $.classname)), repeat(field("class_var", $.id)),
      optional(wrap("{", "}", repeat(choice(
        seq(alias(choice($.id, $.operator_id), $.field), ":", $.type, ";;"),
        field("default", seq("default", alias(choice($.id, $.operator_id), $.field), choice(
          repeat1(seq("|", sep1(",", $.or_pattern), "=", $.expression)),
          seq(wrapsep1("|", "|", ",", $.or_pattern), $.expression),
          seq("=", $.expression)),
          ";;")),
        $.fixity
      ))))
    ),

    instance_chain: $ => statement("instance", $.instance, repeat(seq("else", $.instance))),

    instance: $ => seq(
      optional(alias(/@[A-Za-z_]\w*/, $.name)),
      optional(seq("forall", repeat1($.type_var), ".")),
      optional(seq(wrapsep("(", ")", ",", $.type), "=>")),
      alias($.id, $.classname),
      repeat(choice($.id, wrap("(", ")", $.type))),
      optional(choice(
        "fail",
        wrap("{", "}", repeat(seq(alias($.id, $.field), optional(seq(":", $.type)), choice(
            repeat1(seq("|", sep1(",", $.or_pattern), "=", $.expression)),
            seq(wrapsep1("|", "|", ",", $.or_pattern), $.expression),
            seq("=", $.expression)
          ),
        ";;")
        ))
      )),
      ),

    region: $ => statement("region", optional(choice("open", "close")), $.region_id,
      optional(seq(
        alias($.id, $.repr), repeat(alias($.id, $.repr_param)), choice(
          wrapsep1("{", "}", ",", seq(alias($.id, $.data_accessor), ":", $.type, optional(seq("=", alias($.id, $.field))))),
          repeat1(seq("|", alias($.id, $.data_constructor), choice(
            seq(":", $.type, optional(seq("=", $.data_repr))),
            repeat(seq(
              choice($.id, $.unit, wrap("(", ")", seq($.type, optional(seq("=", alias($.id, $.field))))), $.tuple_type),
              optional(seq("=", $.data_repr))
            ))
          ))),
          seq("=",
            optional(seq(
              alias($.id, $.newtype_evidence),
              optional(seq(",", alias($.id, $.newtype_reverse_evidence))),
              "of"
            )),
            $.type,
            optional(seq("=", $.data_repr))
          ),
        ),
        optional(seq("=", $.data_repr))
      )),
      "with", optional(wrapsep("{", "}", ",", $.expression)),
      choice("_", seq(alias($.id, $.from), $.pattern, repeat1(seq("|", $.pattern, "=", $.expression)))),
      ",", choice("_", seq(alias($.id, $.to), $.pattern, repeat1(seq("|", $.pattern, "=", $.expression))))
    ),

    // type declaration
    data: $ => statement("data",
      repeat($.region_id), alias($.id, $.type_constructor), repeat(alias($.id, $.type_param)),
      optional(choice(
        seq("=",
          optional(seq(
            alias($.id, $.newtype_evidence),
            optional(seq(",", alias($.id, $.newtype_reverse_evidence))),
            "of"
          )),
          $.type,
          optional(seq("=", $.data_repr))
        ),
        repeat1(seq("|", alias($.id, $.data_constructor), choice(
          seq(":", $.type, optional(seq("=", $.data_repr))),
          repeat(seq(
            choice($.id, $.unit, wrap("(", ")", seq($.type, optional(seq("=", alias($.id, $.field))))), $.tuple_type),
            optional(seq("=", $.data_repr))
          ))
        ))),
        seq(
          wrapsep1("{", "}", ",", seq(
            repeat($.region_id),
            alias($.id, $.data_accessor),
            ":", $.type, optional(seq("=", alias($.id, $.field)))
          )),
          optional(seq("=", $.data_repr))
        )
      )),
      optional(seq("deriving", sep1(",", repeat1($.id)), optional(seq("via", sep1(",", repeat1($.id))))))
    ),

    // explicit representation of a data type and also its memory management
    data_repr: $ => seq(
      choice(
        alias($.id, $.var),
        wrapsep1("(", ")", ",", choice(
          seq(
            $.type,
            optional(seq("=", choice(alias($.id, $.value), alias(/\$[0-9]+/, $.field), $.number, $.string))),
            optional(alias($.region_id, $.annotation))
          ),
          $.data_repr
        ))
      ),
      optional(alias($.region_id, $.annotation))
    ),

    type_alias: $ => statement("type", alias($.id, $.name), repeat(field("var", $.id)), "=", $.type),

    // type expression
    type: $ => prec.right(PRIORITY.TYPE, seq(
      choice(
        seq("forall", repeat1(choice(
          wrapsep("{", "}", ",", $.region_id),
          alias($.id, $.type_var_id),
          // TODO: refine semantic of ":", after it should be kind expression instead of type
          wrap("(", ")", seq(alias($.id, $.type_var_id), choice("=", "~", ":"), $.type))
        )),".", $.type),
        alias(choice("->", "=>"), $.type_reserved_operator),
        $.operator,
        $.type_var,
        $.unit,
        $.tuple_type,
        wrap("(", ")", $.type),
        $.handler_type,
        $.record_type,
        $.primitive_type
      ),
      repeat(choice($.type, $.type_effect_suffix)),
    )),

    handler_type: $ => wrap("{", "}", seq(sep1(",", seq(optional(".."), $.type)), ";;", $.type, optional(seq(",", $.type)))),
    record_type: $ => wrapsep("{", "}", ",", seq(alias($.identifier, $.record_field), "=", $.type)),
    tuple_type: $ => tuple($.type),

    // type effect suffix
    type_effect_suffix: $ => choice(
      alias(seq("!", token.immediate(/'?[A-Za-z_]\w*/)), $.type_eff_name),
      wrapsep("!{", "}", ",", choice($.type, seq("..", choice($.identifier, $.type_implicit_var))))
    ),
    type_var: $ => choice(alias($.id, $.type_var_id), $.type_implicit_var, $.region_id),
    type_implicit_var: _$ => /'[A-Za-z_]\w*/,

    primitive_type: $ => choice(
      alias($.primitive_reverse_atom, $.primitive_reverse_atom_prim),
      wrap("#!(", ")", seq($.primitive_compound, choice(
        repeat(seq("|", $.primitive_compound)),
        repeat1(seq(",", $.primitive_compound)),
      ))),
    ),
    primitive_reverse_atom: _$ => seq("#!", token.immediate(/[A-Za-z_]\w*/)),
    primitive_reverse_prim: $ => wrap("#!(", ")", $.type),
    primitive_compound: $ => prec.left(PRIORITY.PRIMITIVE_TYPE, choice(
      seq(alias($.type_var, $.primitive_cons), repeat($.primitive_compound)),
      $.operator,
      $.primitive_reverse_atom,
      $.primitive_reverse_prim,
      wrap("[", "]", seq($.primitive_compound, ";", $.integer)),
      wrap("(", ")", seq($.primitive_compound, choice(
        repeat(seq("|", $.primitive_compound)),
        repeat1(seq(",", $.primitive_compound))
      )))
    )),

    constraint: $ => statement("rule", sep1(",", seq(alias(choice($.id, $.force_id, $.operator_id), $.rulename), repeat("_"))),
      wrap("{", "}", repeat(choice(
        seq(alias($.id, $.property), optional(seq("forall", repeat1($.id))), wrap("@", ".", seq(
          sep1(",", $.chr),
          optional(seq("\\", sep1(",", $.chr))),
          repeat1(seq(optional(seq("|", $.chr)), choice("<=>", "==>"), sep1(",", $.chr)))
        ))),
        $.fixity
      )))) ,

    chr: $ => seq(
      choice($.identifier, $.operator, $.force_operator_id, $.number, wrap("(", ")", $.chr)),
      optional($.chr)
    ),

    // pattern synomym
    binding: $ => statement("pattern", optional(wrap("{", "}", repeat(alias($.id, $.type_id)))),
      alias($.id, $.cons),
      repeat(choice(wrap("(", ")", seq($.id, ":", $.type)), $.id)),
      optional(seq(":", $.type)),
      "<-",
      $.pattern
    ),

    // operator fixity declaration
    fixity: $ => {
      const operators = repeat1(choice($.operator, $.operator_id, $.force_operator_id))
      return statement("fixity", /\d+/,
        choice(seq("_", operators), seq(operators, "_")),
        repeat(choice("infix", "postfix", "prefix"))
      )
    },

    // let binding includes effect block, value definition and function definition
    definition: $ => statement("let", choice($.id, $.force_id, $.operator_id), optional(seq(":", $.type)), choice(
      seq("=", $.expression),
      seq(wrapsep("|", "|", ",", $.or_pattern), $.expression),
      repeat1(seq("|", sep1(",", $.or_pattern), "=", $.expression)),
      seq("do", choice($.do_block, $.expression))
    )),

    // expression
    expression: $ => prec.left(PRIORITY.EXPRESSION, seq(
      choice(
        $.macro_id, $.identifier, $.number,
        $.string, $.string_cons, $.operator,
        $.operator_id, $.force_operator_id,
        $.handler, $.macro_block, $.region_id, "%null",
        seq("forall", repeat1($.id), ".", $.expression),
        seq("let", sep1(",", seq($.or_pattern, choice("=", wrapsep("|", "|", ",", $.or_pattern)), $.expression)), "in", $.expression),
        seq(wrapsep1("|", "|", ",", $.or_pattern), $.expression),
        seq(optional(seq("with", sep1(",", choice("auto", $.expression)))), "do", choice($.do_block, $.expression)),
        seq("match", sep1(",", $.expression), "with", wrapsep1(seq("[", optional("|")), "]", "|", seq(sep1(",", $.or_pattern), "=", $.expression))),
        wrapsep1("[", "]", "|", seq(sep1(",", $.or_pattern), "=", $.expression)),
        $.unit,
        $.unlift_type,
        wrapsep1("(", ")", ",", $.expression),
      ), optional(choice(seq(":", $.type), repeat1($.expression)))
    )),

    unit: _$ => seq("(", ")"),
    unlift_type: $ => choice(
      wrapsep("@(", ")", ",", $.type),
      seq("@", alias(token.immediate(/'?[A-Za-z_]\w*/), $.unlift_var))
    ),

    macro_block: $ => wrap(alias(seq("{", token.immediate(/[A-Za-z_]\w*!/)), $.name), "}",
      field("content", repeat($._macro_block_content))),
    _macro_block_content: $ => choice(
      $.identifier, $.number, $.string, $.operator, $.operator_id,
      $.force_operator_id, $.macro_id, $.macro_block,
      choice("|", ",", ":", "="),
      wrap("(", ")", repeat($._macro_block_content)),
      wrap("[", "]", repeat($._macro_block_content)),
      wrap("{", "}", repeat($._macro_block_content))
    ),

    or_pattern: $ => seq($.pattern, optional(field("altpattern", choice(
      seq("or", choice(wrapsep("{", "}", ",", choice(seq(optional(seq($.id, "=")), $.expression), seq(optional(".."), $.pattern_var))), $.expression, $.pattern_var)),
      seq("return", seq(optional(seq("..", $.id, ",")), $.expression))
    )))),

    do_block: $ => wrapsep("{", "}", ";;", choice(
      sep1(",", seq($.or_pattern, "=", $.expression)),
      sep1(",", seq($.expression, optional(seq("->", $.or_pattern))))
    )),

    handler: $ => prec(PRIORITY.EXPRESSION, seq("handler", choice(
      seq("with", wrapsep("{", "}", ",", $.expression)),
      seq("auto", "with", wrapsep("{", "}", ",", $.type)),
      seq(sep1(",", alias($.id, $.method)), alias($.id, $.handler_resume), choice("=", wrapsep("|", "|", ",", $.or_pattern)), $.expression),
      seq(optional(seq("forall", repeat(choice($.id, $.force_id)))), wrapsep1("|", "|", ",", $.type), wrapsep("{", "}", ",",
        seq(alias($.id, $.method), choice(alias($.id, $.handler_resume), wrap("(", ")", seq(alias($.id, $.handler_resume), ":", $.type))), choice(
          seq("=", $.expression),
          seq(wrapsep("|", "|", ",", $.or_pattern), $.expression),
          repeat1(seq("|", sep1(",", $.or_pattern), "=", $.expression)))
        )))
    ))),

    // definition of pattern
    pattern: $ => prec.right(PRIORITY.PATTERN, seq(
      choice(
        seq(choice(alias("_", $.pattern_wildcard), $.pattern_var), optional(seq(
          token.immediate("@"),
          choice($.unit, wrapsep("(", ")", ",", $.pattern))
        ))),
        $.region_id, "%null",
        field("constructor", $.pattern_cons),
        field("literal", choice($.unit, $.string_cons, $.string, $.number)),
        wrapsep1("(", ")", ",", $.pattern)
      ),
      repeat($.pattern),
      optional(field("annotation", seq(":", $.type))),
      optional(field("view", seq("<-", $.expression)))
    )),
    pattern_var: _$ => seq("?", token.immediate(/[A-Za-z_]\w*/)),
    pattern_cons: $ => prec(PRIORITY.PATTERN, choice($.identifier, $.operator, $.operator_id, $.force_operator_id)),

    // effect
    effect: $ => statement("effect", choice(
      seq(optional(seq("forall", repeat1($.id), ".")), sep1(",", alias($.id, $.named)), ":", sep1(",", $.type)),
      seq("auto", sep1(",", seq($.id, repeat(choice($.id, $.region_id))))),
      seq(alias($.id, $.name), repeat(choice($.id, $.region_id)), wrapsep("{", "}", ";;",
        seq(optional(seq("forall", repeat1($.id), ".")), alias($.id, $.method), ":", sep1(",", $.type)))
      )
    )),

    // definition of terminal symbol
    hex_integer: _$ => /0[xX][_A-Za-z0-9]+/,
    octet_integer: _$ => /0[oO][_0-7]+/,

    // integer
    integer: $ => choice(/\d[0-9_]*/, $.hex_integer, $.octet_integer),
    floating: _$ => /([0-9][0-9_]*)\.[_0-9]*/,

    // language literals
    number: $ => seq(choice($.floating, $.integer), optional(token.immediate(/\.[A-Za-z_]\w*/))),

    // C string from https://github.com/tree-sitter/tree-sitter-c/blob/master/grammar.js
    string: $ => wrap("\"","\"", repeat(choice(
        alias(token.immediate(prec(PRIORITY.EXPRESSION, /[^\\"\n]+/)), $.string_content),
        $._escape_sequence,
      ))),
    _escape_sequence: _$ => token(prec(PRIORITY.EXPRESSION, seq(
      '\\',
      choice(
        /[^xuU]/,
        /\d{2,3}/,
        /x[0-9a-fA-F]{1,4}/,
        /u[0-9a-fA-F]{4}/,
        /U[0-9a-fA-F]{8}/,
      ),
    ))),

    string_cons: $ => seq($.string, alias(token.immediate(/\.[A-Za-z_]\w*/), $.string_cons_var)),
    attribute: $ => $.string,

    absolute_id: _$ => /#[A-Za-z_]\w*(#[A-Za-z_]\w*)*/,
    namespace: _$ => token(sep1("/", /[A-Za-z_]\w*/)),

    // operator
    operator: _$ => /[+\-*/\\!~@#$%^&?.><;]|[+\-*/\\!~@#$%^&=|?,.><;:][+\-*/\\!~@#$%^&=|?,.><;:]+/,
    operator_id: _$ => /\([+\-*/\\!~@#$%^&=|?,.><;:]+\)/,
    force_operator_id: _$ => /#\([+\-*/\\!~@#$%^&=|?,.><;:]+\)/,
    macro_id: _$ => /#[A-Za-z_]\w*(#[A-Za-z_]\w*)*!/,
    region_id: _$ => /%[A-Za-z_]\w*/,

    id: _$ => /[A-Za-z_]\w*/,
    // #prefix#name or #prefix#name#(++) simple operator
    qualified_id: _$ => /#[A-Za-z_]\w*(#[A-Za-z_]\w*)*(#\([+:;\-*/\\!~@#$%^&=|?.><]+\))?/,
    force_id: _$ => /##[A-Za-z_]\w*/,
    identifier: $ => choice($.force_id, $.qualified_id, $.id)
  }
})

/**
 * @param {RuleOrLiteral} rule
 */
function tuple(rule) {
  return wrap("(", ")", seq(rule, repeat1(seq(",", rule))))
}

/**
 * @param {RuleOrLiteral} delimiter
 * @param {RuleOrLiteral} rule
 */
function sep1(delimiter ,rule) {
  return seq(rule, repeat(seq(delimiter, rule)))
}

/**
 * @param {RuleOrLiteral} delimiter
 * @param {RuleOrLiteral} rule
 */
function sep(delimiter, rule) {
  return optional(sep1(delimiter, rule))
}

/**
 * @param {RuleOrLiteral} left
 * @param {RuleOrLiteral} right
 * @param {RuleOrLiteral} rule
 */
function wrap(left, right, rule) {
  return seq(left, rule, right)
}

/**
 * @param {RuleOrLiteral} left
 * @param {RuleOrLiteral} right
 * @param {RuleOrLiteral} delimiter
 * @param {RuleOrLiteral} rule
 */
function wrapsep(left, right, delimiter, rule) {
  return wrap(left, right, sep(delimiter, rule))
}

/**
 * @param {RuleOrLiteral} left
 * @param {RuleOrLiteral} right
 * @param {RuleOrLiteral} delimiter
 * @param {RuleOrLiteral} rule
 */
function wrapsep1(left, right, delimiter, rule) {
  return wrap(left, right, sep1(delimiter, rule))
}

/**
 * @param {any[]} rules
 */
function statement(...rules) {
  return seq(...rules, ";;")
}
