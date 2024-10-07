/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "yolang",
  extras: $ => [/\s|\\\r?\n/, $.comment],
  rules: {
    source_file: $ => seq($.module, repeat($._top)),

    // top level items in a file, include all things here.
    _top: $ => choice(
      $.use,
      $.declaration,
    ),

    // module declaration and its exporting list
    module: $ => statement("module", optional($.module_attr), $.module_path, optional(choice(
      // inside parenthesis
      wrapsep("(", ")", ",", choice(
        $.qualified_id,
        $.id,
        $.module_export,
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
    module_attr: $ => $.string,
    module_annotation: _$ => /[A-Za-z_]\w*:/,
    module_path: _$ => token(sep1("/", /[A-Za-z_]\w*/)),
    module_export: $ => seq("module", choice(
      seq($.module_id, optional(choice(
        wrap("(", ")", sep(",", $.identifier)),
        seq("*", optional(seq("exclude", wrap("(", ")", sep(",", $.identifier)))))
      ))),
      wrap("(", ")", sep(",", $.module_id)),
      seq("*", optional(seq("exclude", wrap("(", ")", sep(",", $.module_id)))))
    )),
    module_id: _$ => /#[A-Za-z_]\w*(#[A-Za-z_]\w*)*/,

    // use statement
    use: $ => statement("use", optional($.module_attr), optional($.module_annotation), $.use_piece),
    use_piece: $ => seq(
      alias($.module_path, $.use_path),
      optional(choice(
        seq(alias("as", $.use_keyword_as), $.id),
        seq(alias("alias", $.use_keyword_alias), $.id),
        alias("self", $.use_keyword_self))),
      optional(choice(
        wrap("{", "}", sep(",", choice($.use_piece, $.operator_id, $.force_id))),
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
    ),

    // type declaration
    data: $ => statement("data", alias($.id, $.type_constructor), repeat(alias($.id, $.type_param)), optional(choice(
      seq("=",
        optional(seq(
          alias($.id, $.newtype_evidence),
          optional(seq(",", alias($.id, $.newtype_reverse_evidence))),
          "of"
        )),
        $.type
      ),
      repeat1(seq("|", alias($.id, $.data_constructor), repeat(choice($.id, $.type_unit, $.type_tuple)))),
      wrap("{", "}", sep1(",", seq($.id, ":", $.type)))
    ))),

    // type expression
    type: $ => prec.left(1, seq(
      choice(
        seq("forall", repeat1(choice(
          alias($.id, $.type_var_id),
          // TODO: refine semantic of ":", after it should be kind expression instead of type
          wrap("(", ")", seq(alias($.id, $.type_var_id), choice("=", "~", ":"), $.type))
        )),".", $.type),
        alias(choice("->", "=>"), $.type_reserved_operator),
        $.operator,
        $.type_var,
        $.type_unit,
        $.type_tuple,
        wrap("(", ")", $.type),
        $.handler_type,
        $.record_type,
        $.primitive_type
      ),
      repeat(choice($.type, $.type_effect_suffix)),
    )),

    handler_type: $ => wrap("{", "}", seq(sep1(",", seq(optional(".."), $.type)), ";;", $.type)),
    record_type: $ => wrapsep("{", "}", ",", seq(alias($.identifier, $.record_field), "=", $.type)),

    // type effect suffix
    type_effect_suffix: $ => choice(
      alias(seq("!", token.immediate(/'?[A-Za-z_]\w*/)), $.type_eff_name),
      wrapsep("!{", "}", ",", choice($.type, seq("..", choice($.identifier, $.type_implicit_var))))
    ),
    type_unit: _$ => /\(\s*\)/,
    type_tuple: $ => wrap("(", ")", sep1(",", $.type)),
    type_var: $ => choice(alias($.id, $.type_var_id), $.type_implicit_var),
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


    // operator fixity declaration
    fixity: $ => statement("fixity", /\d+/,
      choice(seq("_", repeat1($.operator)), seq(repeat1($.operator), "_")),
      repeat(choice("infix", "postfix", "prefix"))
    ),

    // let binding includes effect block, value definition and function definition
    let_binding: $ => statement("let", choice($.id, $.force_id, $.operator_id), optional(seq(":", $.type)), choice(
      seq("=", $.expression),
      repeat1(seq("|", sep1(",", $.or_pattern), "=", $.expression)),
      seq("do", choice($.do_block, $.expression))
    )),

    // expression
    expression: $ => prec.left(1, seq(
      choice(
        $.macro_id, $.identifier, $.number,
        $.string, $.string_cons, $.operator,
        $.handler,
        seq("forall", repeat1($.id), ".", $.expression),
        seq("let", sep1(",", seq($.or_pattern, "=", $.expression)), "in", $.expression),
        seq(wrapsep1("|", "|", ",", $.or_pattern), $.expression),
        seq(optional(seq("with", sep1(",", $.expression))), "do", choice($.do_block, $.expression)),
        wrapsep1("[", "]", "|", seq(sep1(",", $.or_pattern), "=", $.expression)),
        wrapsep("(", ")", ",", $.expression),
      ), optional(choice(seq(":", $.type), repeat1($.expression)))
    )),

    or_pattern: $ => seq($.pattern, optional(seq("or", $.expression))),

    do_block: $ => wrapsep("{", "}", ";;", choice(
      seq("let", $.or_pattern, "<-", $.expression),
      seq("const", sep1(",", seq($.or_pattern, "=", $.expression))),
      $.expression
    )),

    handler: $ => prec(1, seq("handler", choice(
      seq(sep1(",", alias($.id, $.method)), alias($.id, $.handler_resume), "=", $.expression),
      seq(optional(seq("forall", repeat(choice($.id, $.force_id)))), wrapsep1("|", "|", ",", $.type), wrapsep("{", "}", ",",
        seq(alias($.id, $.method), alias($.id, $.handler_resume), choice(
          seq("=", $.expression),
          repeat1(seq("|", sep1(",", $.or_pattern), "=", $.expression)))
        )))
    ))),

    // definition of pattern
    pattern: $ => prec.left(1, seq(
      choice(
        seq(choice(alias("_", $.pattern_wildcard), $.pattern_var), optional(seq(
          token.immediate("@"),
          choice($.pattern_unit, wrapsep("(", ")", ",", $.pattern))
        ))),
        $.pattern_cons,
        $.pattern_unit,
        $.string_cons,
        $.string,
        $.number,
        seq("!", $.expression, "->", $.pattern),
        wrapsep1("(", ")", ",", $.pattern)
      ), optional(choice(seq(":", $.type), repeat1($.pattern)))
    )),
    pattern_var: _$ => seq("?", token.immediate(/[A-Za-z_]\w*/)),
    pattern_cons: $ => prec(1, $.id),
    pattern_unit: _$ => /\(\s*\)/,


    // effect
    effect: $ => statement("effect", choice(
      seq(optional(seq(repeat($.id), ".")), sep1(",", alias($.id, $.method)), ":", $.type),
      seq(repeat1($.id), wrapsep("{", "}", ",", seq(alias($.id, $.method), ":", $.type)))
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
    _escape_sequence: _ => token(prec(1, seq(
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

    // operator
    operator: _$ => /[+\-*/\\!@#$%^&=|?.><;:]+/,
    operator_id: _$ => /\([+:;\-*/\\!@#$%^&=|?.><]+\)/,
    macro_id: _$ => /#[A-Za-z_]\w*(#[A-Za-z_]\w*)*!/,

    id: _$ => /[A-Za-z_]\w*/,
    // #prefix#name or #prefix#name#(++) simple operator
    qualified_id: _$ => /#[A-Za-z_]\w*(#[A-Za-z_]\w*)*(#\([+:\-*/\\!@#$%^&=|?.><]+\))?/,
    force_id: _$ => /##[A-Za-z_]\w*/,
    identifier: $ => choice($.force_id, $.qualified_id, $.id)
  }
})

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
