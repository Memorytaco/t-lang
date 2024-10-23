/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "yolang",
  // extras: $ => [/\s|\\\r?\n/, $.comment],
  extras: $ => [/\s/, $.comment],
  rules: {
    source_file: $ => seq($.module, repeat($._top)),

    // top level items in a file, include all things here.
    _top: $ => choice(
      $.use,
      $.declaration,
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

    declaration: $ => choice(
      $.data,
      $.let_binding,
      $.fixity,
      $.effect,
      $.binding,
      $.constraint
    ),

    // type declaration
    data: $ => statement("data", repeat($.type_annotation_var), alias($.id, $.type_constructor), repeat(alias($.id, $.type_param)), optional(choice(
      seq("=",
        optional(seq(
          alias($.id, $.newtype_evidence),
          optional(seq(",", alias($.id, $.newtype_reverse_evidence))),
          "of"
        )),
        $.type
      ),
      repeat1(seq("|", repeat($.type_annotation_var), alias($.id, $.data_constructor), choice(
        seq(":", $.type),
        repeat(choice($.id, $.unit, wrap("(", ")", $.type), $.tuple_type))
      ))),
      wrap("{", "}", sep1(",", seq(repeat($.type_annotation_var), $.id, ":", $.type)))
    ))),

    // type expression
    type: $ => prec.left(1, seq(
      choice(
        seq("forall", repeat1(choice(
          wrapsep("{", "}", ",", $.type_annotation_var),
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
    type_var: $ => choice(alias($.id, $.type_var_id), $.type_implicit_var, $.type_annotation_var),
    type_implicit_var: _$ => /'[A-Za-z_]\w*/,
    type_annotation_var: _$ => /%[A-Za-z_]\w*/,

    primitive_type: $ => choice(
      alias($.primitive_reverse_atom, $.primitive_reverse_atom_prim),
      wrap("#!(", ")", seq($.primitive_compound, choice(
        repeat(seq("|", $.primitive_compound)),
        repeat1(seq(",", $.primitive_compound)),
      ))),
    ),
    primitive_reverse_atom: _$ => seq("#!", token.immediate(/[A-Za-z_]\w*/)),
    primitive_reverse_prim: $ => wrap("#!(", ")", $.type),
    primitive_compound: $ => prec.left(2, choice(
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
    binding: $ => statement("bind", optional(wrap("{", "}", repeat(alias($.id, $.type_id)))),
      alias($.id, $.cons),
      repeat(choice(wrap("(", ")", seq($.id, ":", $.type)), $.id)),
      optional(seq(":", $.type)),
      "<-",
      $.pattern
    ),

    // operator fixity declaration
    fixity: $ => {
      const operators = repeat1(choice($.operator, $.force_operator_id))
      return statement("fixity", /\d+/,
        choice(seq("_", operators), seq(operators, "_")),
        repeat(choice("infix", "postfix", "prefix"))
      )
    },

    // let binding includes effect block, value definition and function definition
    let_binding: $ => statement("let", choice($.id, $.force_id, $.operator_id), optional(seq(":", $.type)), choice(
      seq("=", $.expression),
      seq(wrapsep("|", "|", ",", $.or_pattern), $.expression),
      repeat1(seq("|", sep1(",", $.or_pattern), "=", $.expression)),
      seq("do", choice($.do_block, $.expression))
    )),

    // expression
    expression: $ => prec.left(1, seq(
      choice(
        $.macro_id, $.identifier, $.number,
        $.string, $.string_cons, $.operator,
        $.operator_id, $.force_operator_id,
        $.handler,
        seq("forall", repeat1($.id), ".", $.expression),
        seq("let", sep1(",", seq($.or_pattern, "=", $.expression)), "in", $.expression),
        seq(wrapsep1("|", "|", ",", $.or_pattern), $.expression),
        seq(optional(seq("with", sep1(",", choice("auto", $.expression)))), "do", choice($.do_block, $.expression)),
        wrapsep1("[", "]", "|", seq(sep1(",", $.or_pattern), "=", $.expression)),
        $.unit,
        $.unlift_type,
        wrapsep1("(", ")", ",", $.expression),
      ), optional(choice(seq(":", $.type), repeat1($.expression)))
    )),

    unit: _$ => token(seq("(", ")")),
    unlift_type: $ => choice(
      wrapsep("@(", ")", ",", $.type),
      seq("@", alias(token.immediate(/'?[A-Za-z_]\w*/), $.unlift_var))
    ),

    or_pattern: $ => seq($.pattern, optional(seq("or", $.expression))),

    do_block: $ => wrapsep("{", "}", ";;", choice(
      seq("bind", sep1(",", seq($.or_pattern, "=", $.expression))),
      $.expression
    )),

    handler: $ => prec(1, seq("handler", choice(
      seq("with", wrapsep("{", "}", ",", $.expression)),
      seq("auto", "with", wrapsep("{", "}", ",", $.type)),
      seq(sep1(",", alias($.id, $.method)), alias($.id, $.handler_resume), "=", wrapsep("|", "|", ",", $.or_pattern), $.expression),
      seq(optional(seq("forall", repeat(choice($.id, $.force_id)))), wrapsep1("|", "|", ",", $.type), wrapsep("{", "}", ",",
        seq(alias($.id, $.method), choice(alias($.id, $.handler_resume), wrap("(", ")", seq(alias($.id, $.handler_resume), ":", $.type))), choice(
          seq("=", $.expression),
          seq(wrapsep("|", "|", ",", $.or_pattern), $.expression),
          repeat1(seq("|", sep1(",", $.or_pattern), "=", $.expression)))
        )))
    ))),

    // definition of pattern
    pattern: $ => prec.left(1, seq(
      choice(
        seq(choice(alias("_", $.pattern_wildcard), $.pattern_var), optional(seq(
          token.immediate("@"),
          choice($.unit, wrapsep("(", ")", ",", $.pattern))
        ))),
        $.pattern_cons,
        $.unit,
        $.string_cons,
        $.string,
        $.number,
        // seq("!", $.expression, "->", $.pattern),
        wrapsep1("(", ")", ",", $.pattern)
      ),
      optional(choice(seq(":", $.type), repeat1($.pattern))),
      optional(seq("<-", $.expression))
    )),
    pattern_var: _$ => seq("?", token.immediate(/[A-Za-z_]\w*/)),
    pattern_cons: $ => prec(1, $.identifier),

    // effect
    effect: $ => statement("effect", choice(
      seq(optional(seq("forall", repeat1($.id), ".")), sep1(",", alias($.id, $.method)), ":", sep1(",", $.type)),
      seq("auto", sep1(",", seq($.id, repeat(choice($.id, $.type_annotation_var))))),
      seq($.id, repeat(choice($.id, $.type_annotation_var)), wrapsep("{", "}", ";;",
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
        alias(token.immediate(prec(1, /[^\\"\n]+/)), $.string_content),
        $._escape_sequence,
      ))),
    _escape_sequence: _$ => token(prec(1, seq(
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
    operator: _$ => /[+\-*/\\!~@#$%^&=|?.><;:]+/,
    operator_id: _$ => /\([+:;\-*/\\!~@#$%^&=|?.><]+\)/,
    force_operator_id: _$ => /#\([+:;\-*/\\!~@#$%^&=|?.><]+\)/,
    macro_id: _$ => /#[A-Za-z_]\w*(#[A-Za-z_]\w*)*!/,

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
