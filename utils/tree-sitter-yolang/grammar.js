/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "yolang",
  extras: $ => [/\s|\\\r?\n/, $.comment],
  rules: {
    source_file: $ => seq($.module, repeat($._top)),

    _top: $ => choice(
      $.declaration,
      $.use
    ),

    // module declaration
    module: $ => statement(seq("module", optional($.module_attr), $.module_path, optional(choice(
      // inside parenthesis
      wrap("(", ")", sep(",", choice(
        $.qualified_id,
        $.id,
        $.module_export,
        seq("*", optional(seq("exclude", wrap("(", ")", $.qualified_id))))
      ))),
      // after everything *
      seq("*", optional(seq("exclude", wrap("(", ")", sep(",", choice(
        $.identifier,
        $.operator_id,
      ))))))
    )))),
    module_attr: $ => $.string,
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
    use: $ => statement(seq("use", optional($.module_attr), $.use_piece)),
    use_piece: $ => seq(
      alias($.module_path, $.use_path),
      optional(choice(
        seq(alias("as", $.use_keyword_as), $.id),
        seq(alias("alias", $.use_keyword_alias), $.id),
        alias("self", $.use_keyword_self))),
      optional(choice(
        wrap("{", "}", sep(",", choice($.use_piece, $.operator_id))),
        seq(
          alias("*", $.use_keyword_star),
          optional(seq(alias("exclude", $.use_keyword_exclude), wrap("(", ")", choice($.id, $.operator_id))))
        )
      ))
    ),

    // C comment from https://github.com/tree-sitter/tree-sitter-c/blob/master/grammar.js
    comment: _$ => token(choice(
      seq('//', /(\\+(.|\r?\n)|[^\\\n])*/),
      seq( '/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/',))),

    declaration: $ => choice($.data),

    // type declaration
    data: _$ => statement("data"),

    type_decl: _$ => /type/,

    // definition of terminal symbol
    _digit : _$ => /\d/,
    _octet: _$ => /[0-7]/,
    _hex: _$ => /[a-fA-F]|\d/,
    _hex_integer: $ => seq(/0[xX]/, repeat1($._digit)),
    _octet_integer: $ => seq(/0[oO]/, repeat1($._octet)),

    // integer
    integer: $ => choice(repeat1($._digit), $._hex_integer, $._octet_integer),
    floating: $ => seq(repeat($._digit), ".", repeat1($._digit)),

    // language literals
    number: $ => choice($.floating, $.integer),

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

    // operator
    operator: _$ => /[+\-*/\\!@#$%^&=|?.><]+/,
    operator_id: _$ => /\([+:\-*/\\!@#$%^&=|?.><]+\)/,

    id: _$ => /[A-Za-z_]\w*/,
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
 * @param {any[]} rules
 */
function statement(...rules) {
  return seq(...rules, ";;")
}
