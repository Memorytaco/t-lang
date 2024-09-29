#include "tree_sitter/parser.h"

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 149
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 50
#define ALIAS_COUNT 3
#define TOKEN_COUNT 32
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 0
#define MAX_ALIAS_SEQUENCE_LENGTH 12
#define PRODUCTION_ID_COUNT 8

enum ts_symbol_identifiers {
  anon_sym_module = 1,
  anon_sym_LPAREN = 2,
  anon_sym_STAR = 3,
  anon_sym_exclude = 4,
  anon_sym_RPAREN = 5,
  anon_sym_COMMA = 6,
  anon_sym_SEMI_SEMI = 7,
  sym_module_path = 8,
  sym_module_id = 9,
  anon_sym_use = 10,
  anon_sym_as = 11,
  anon_sym_alias = 12,
  anon_sym_self = 13,
  anon_sym_LBRACE = 14,
  anon_sym_RBRACE = 15,
  sym_comment = 16,
  anon_sym_data = 17,
  sym_type_decl = 18,
  sym__digit = 19,
  sym__octet = 20,
  sym__hex = 21,
  aux_sym__hex_integer_token1 = 22,
  aux_sym__octet_integer_token1 = 23,
  anon_sym_DOT = 24,
  anon_sym_DQUOTE = 25,
  aux_sym_string_token1 = 26,
  sym__escape_sequence = 27,
  sym_operator_id = 28,
  sym_id = 29,
  sym_qualified_id = 30,
  sym_force_id = 31,
  sym_source_file = 32,
  sym__top = 33,
  sym_module = 34,
  sym_module_attr = 35,
  sym_module_export = 36,
  sym_use = 37,
  sym_use_piece = 38,
  sym_declaration = 39,
  sym_data = 40,
  sym_string = 41,
  sym_identifier = 42,
  aux_sym_source_file_repeat1 = 43,
  aux_sym_module_repeat1 = 44,
  aux_sym_module_repeat2 = 45,
  aux_sym_module_export_repeat1 = 46,
  aux_sym_module_export_repeat2 = 47,
  aux_sym_use_piece_repeat1 = 48,
  aux_sym_string_repeat1 = 49,
  alias_sym_use_keyword_exclude = 50,
  alias_sym_use_keyword_star = 51,
  alias_sym_use_path = 52,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [anon_sym_module] = "module",
  [anon_sym_LPAREN] = "(",
  [anon_sym_STAR] = "*",
  [anon_sym_exclude] = "exclude",
  [anon_sym_RPAREN] = ")",
  [anon_sym_COMMA] = ",",
  [anon_sym_SEMI_SEMI] = ";;",
  [sym_module_path] = "module_path",
  [sym_module_id] = "module_id",
  [anon_sym_use] = "use",
  [anon_sym_as] = "use_keyword_as",
  [anon_sym_alias] = "use_keyword_alias",
  [anon_sym_self] = "use_keyword_self",
  [anon_sym_LBRACE] = "{",
  [anon_sym_RBRACE] = "}",
  [sym_comment] = "comment",
  [anon_sym_data] = "data",
  [sym_type_decl] = "type_decl",
  [sym__digit] = "_digit",
  [sym__octet] = "_octet",
  [sym__hex] = "_hex",
  [aux_sym__hex_integer_token1] = "_hex_integer_token1",
  [aux_sym__octet_integer_token1] = "_octet_integer_token1",
  [anon_sym_DOT] = ".",
  [anon_sym_DQUOTE] = "\"",
  [aux_sym_string_token1] = "string_content",
  [sym__escape_sequence] = "_escape_sequence",
  [sym_operator_id] = "operator_id",
  [sym_id] = "id",
  [sym_qualified_id] = "qualified_id",
  [sym_force_id] = "force_id",
  [sym_source_file] = "source_file",
  [sym__top] = "_top",
  [sym_module] = "module",
  [sym_module_attr] = "module_attr",
  [sym_module_export] = "module_export",
  [sym_use] = "use",
  [sym_use_piece] = "use_piece",
  [sym_declaration] = "declaration",
  [sym_data] = "data",
  [sym_string] = "string",
  [sym_identifier] = "identifier",
  [aux_sym_source_file_repeat1] = "source_file_repeat1",
  [aux_sym_module_repeat1] = "module_repeat1",
  [aux_sym_module_repeat2] = "module_repeat2",
  [aux_sym_module_export_repeat1] = "module_export_repeat1",
  [aux_sym_module_export_repeat2] = "module_export_repeat2",
  [aux_sym_use_piece_repeat1] = "use_piece_repeat1",
  [aux_sym_string_repeat1] = "string_repeat1",
  [alias_sym_use_keyword_exclude] = "use_keyword_exclude",
  [alias_sym_use_keyword_star] = "use_keyword_star",
  [alias_sym_use_path] = "use_path",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [anon_sym_module] = anon_sym_module,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_STAR] = anon_sym_STAR,
  [anon_sym_exclude] = anon_sym_exclude,
  [anon_sym_RPAREN] = anon_sym_RPAREN,
  [anon_sym_COMMA] = anon_sym_COMMA,
  [anon_sym_SEMI_SEMI] = anon_sym_SEMI_SEMI,
  [sym_module_path] = sym_module_path,
  [sym_module_id] = sym_module_id,
  [anon_sym_use] = anon_sym_use,
  [anon_sym_as] = anon_sym_as,
  [anon_sym_alias] = anon_sym_alias,
  [anon_sym_self] = anon_sym_self,
  [anon_sym_LBRACE] = anon_sym_LBRACE,
  [anon_sym_RBRACE] = anon_sym_RBRACE,
  [sym_comment] = sym_comment,
  [anon_sym_data] = anon_sym_data,
  [sym_type_decl] = sym_type_decl,
  [sym__digit] = sym__digit,
  [sym__octet] = sym__octet,
  [sym__hex] = sym__hex,
  [aux_sym__hex_integer_token1] = aux_sym__hex_integer_token1,
  [aux_sym__octet_integer_token1] = aux_sym__octet_integer_token1,
  [anon_sym_DOT] = anon_sym_DOT,
  [anon_sym_DQUOTE] = anon_sym_DQUOTE,
  [aux_sym_string_token1] = aux_sym_string_token1,
  [sym__escape_sequence] = sym__escape_sequence,
  [sym_operator_id] = sym_operator_id,
  [sym_id] = sym_id,
  [sym_qualified_id] = sym_qualified_id,
  [sym_force_id] = sym_force_id,
  [sym_source_file] = sym_source_file,
  [sym__top] = sym__top,
  [sym_module] = sym_module,
  [sym_module_attr] = sym_module_attr,
  [sym_module_export] = sym_module_export,
  [sym_use] = sym_use,
  [sym_use_piece] = sym_use_piece,
  [sym_declaration] = sym_declaration,
  [sym_data] = sym_data,
  [sym_string] = sym_string,
  [sym_identifier] = sym_identifier,
  [aux_sym_source_file_repeat1] = aux_sym_source_file_repeat1,
  [aux_sym_module_repeat1] = aux_sym_module_repeat1,
  [aux_sym_module_repeat2] = aux_sym_module_repeat2,
  [aux_sym_module_export_repeat1] = aux_sym_module_export_repeat1,
  [aux_sym_module_export_repeat2] = aux_sym_module_export_repeat2,
  [aux_sym_use_piece_repeat1] = aux_sym_use_piece_repeat1,
  [aux_sym_string_repeat1] = aux_sym_string_repeat1,
  [alias_sym_use_keyword_exclude] = alias_sym_use_keyword_exclude,
  [alias_sym_use_keyword_star] = alias_sym_use_keyword_star,
  [alias_sym_use_path] = alias_sym_use_path,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [anon_sym_module] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_STAR] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_exclude] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COMMA] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_SEMI_SEMI] = {
    .visible = true,
    .named = false,
  },
  [sym_module_path] = {
    .visible = true,
    .named = true,
  },
  [sym_module_id] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_use] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_as] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_alias] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_self] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_LBRACE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RBRACE] = {
    .visible = true,
    .named = false,
  },
  [sym_comment] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_data] = {
    .visible = true,
    .named = false,
  },
  [sym_type_decl] = {
    .visible = true,
    .named = true,
  },
  [sym__digit] = {
    .visible = false,
    .named = true,
  },
  [sym__octet] = {
    .visible = false,
    .named = true,
  },
  [sym__hex] = {
    .visible = false,
    .named = true,
  },
  [aux_sym__hex_integer_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym__octet_integer_token1] = {
    .visible = false,
    .named = false,
  },
  [anon_sym_DOT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DQUOTE] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_string_token1] = {
    .visible = true,
    .named = true,
  },
  [sym__escape_sequence] = {
    .visible = false,
    .named = true,
  },
  [sym_operator_id] = {
    .visible = true,
    .named = true,
  },
  [sym_id] = {
    .visible = true,
    .named = true,
  },
  [sym_qualified_id] = {
    .visible = true,
    .named = true,
  },
  [sym_force_id] = {
    .visible = true,
    .named = true,
  },
  [sym_source_file] = {
    .visible = true,
    .named = true,
  },
  [sym__top] = {
    .visible = false,
    .named = true,
  },
  [sym_module] = {
    .visible = true,
    .named = true,
  },
  [sym_module_attr] = {
    .visible = true,
    .named = true,
  },
  [sym_module_export] = {
    .visible = true,
    .named = true,
  },
  [sym_use] = {
    .visible = true,
    .named = true,
  },
  [sym_use_piece] = {
    .visible = true,
    .named = true,
  },
  [sym_declaration] = {
    .visible = true,
    .named = true,
  },
  [sym_data] = {
    .visible = true,
    .named = true,
  },
  [sym_string] = {
    .visible = true,
    .named = true,
  },
  [sym_identifier] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_source_file_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_module_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_module_repeat2] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_module_export_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_module_export_repeat2] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_use_piece_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_string_repeat1] = {
    .visible = false,
    .named = false,
  },
  [alias_sym_use_keyword_exclude] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_use_keyword_star] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_use_path] = {
    .visible = true,
    .named = true,
  },
};

static const TSSymbol ts_alias_sequences[PRODUCTION_ID_COUNT][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
  [1] = {
    [0] = alias_sym_use_path,
  },
  [2] = {
    [0] = alias_sym_use_path,
    [1] = alias_sym_use_keyword_star,
  },
  [3] = {
    [0] = alias_sym_use_path,
    [2] = alias_sym_use_keyword_star,
  },
  [4] = {
    [0] = alias_sym_use_path,
    [3] = alias_sym_use_keyword_star,
  },
  [5] = {
    [0] = alias_sym_use_path,
    [1] = alias_sym_use_keyword_star,
    [2] = alias_sym_use_keyword_exclude,
  },
  [6] = {
    [0] = alias_sym_use_path,
    [2] = alias_sym_use_keyword_star,
    [3] = alias_sym_use_keyword_exclude,
  },
  [7] = {
    [0] = alias_sym_use_path,
    [3] = alias_sym_use_keyword_star,
    [4] = alias_sym_use_keyword_exclude,
  },
};

static const uint16_t ts_non_terminal_alias_map[] = {
  0,
};

static const TSStateId ts_primary_state_ids[STATE_COUNT] = {
  [0] = 0,
  [1] = 1,
  [2] = 2,
  [3] = 3,
  [4] = 4,
  [5] = 5,
  [6] = 6,
  [7] = 7,
  [8] = 8,
  [9] = 9,
  [10] = 10,
  [11] = 11,
  [12] = 12,
  [13] = 13,
  [14] = 14,
  [15] = 15,
  [16] = 16,
  [17] = 17,
  [18] = 18,
  [19] = 19,
  [20] = 20,
  [21] = 21,
  [22] = 22,
  [23] = 23,
  [24] = 24,
  [25] = 25,
  [26] = 26,
  [27] = 27,
  [28] = 28,
  [29] = 29,
  [30] = 30,
  [31] = 31,
  [32] = 32,
  [33] = 33,
  [34] = 34,
  [35] = 35,
  [36] = 36,
  [37] = 37,
  [38] = 38,
  [39] = 39,
  [40] = 40,
  [41] = 41,
  [42] = 42,
  [43] = 43,
  [44] = 44,
  [45] = 45,
  [46] = 46,
  [47] = 47,
  [48] = 48,
  [49] = 49,
  [50] = 50,
  [51] = 51,
  [52] = 52,
  [53] = 53,
  [54] = 54,
  [55] = 55,
  [56] = 56,
  [57] = 57,
  [58] = 58,
  [59] = 59,
  [60] = 60,
  [61] = 61,
  [62] = 62,
  [63] = 63,
  [64] = 64,
  [65] = 65,
  [66] = 66,
  [67] = 67,
  [68] = 68,
  [69] = 69,
  [70] = 70,
  [71] = 71,
  [72] = 72,
  [73] = 73,
  [74] = 74,
  [75] = 75,
  [76] = 76,
  [77] = 77,
  [78] = 78,
  [79] = 79,
  [80] = 80,
  [81] = 81,
  [82] = 82,
  [83] = 83,
  [84] = 84,
  [85] = 85,
  [86] = 86,
  [87] = 87,
  [88] = 88,
  [89] = 89,
  [90] = 90,
  [91] = 91,
  [92] = 92,
  [93] = 93,
  [94] = 94,
  [95] = 95,
  [96] = 96,
  [97] = 97,
  [98] = 98,
  [99] = 99,
  [100] = 100,
  [101] = 101,
  [102] = 102,
  [103] = 103,
  [104] = 104,
  [105] = 105,
  [106] = 106,
  [107] = 107,
  [108] = 108,
  [109] = 109,
  [110] = 110,
  [111] = 111,
  [112] = 112,
  [113] = 113,
  [114] = 114,
  [115] = 115,
  [116] = 116,
  [117] = 117,
  [118] = 118,
  [119] = 119,
  [120] = 120,
  [121] = 121,
  [122] = 122,
  [123] = 123,
  [124] = 124,
  [125] = 125,
  [126] = 126,
  [127] = 127,
  [128] = 128,
  [129] = 129,
  [130] = 130,
  [131] = 131,
  [132] = 132,
  [133] = 133,
  [134] = 134,
  [135] = 135,
  [136] = 136,
  [137] = 137,
  [138] = 138,
  [139] = 139,
  [140] = 140,
  [141] = 141,
  [142] = 142,
  [143] = 143,
  [144] = 144,
  [145] = 145,
  [146] = 146,
  [147] = 147,
  [148] = 148,
};

static TSCharacterRange sym_operator_id_character_set_1[] = {
  {'!', '!'}, {'#', '&'}, {'*', '+'}, {'-', '/'}, {':', ':'}, {'<', '@'}, {'\\', '\\'}, {'^', '^'},
  {'|', '|'},
};

static TSCharacterRange sym_operator_id_character_set_2[] = {
  {'!', '!'}, {'#', '&'}, {')', '+'}, {'-', '/'}, {':', ':'}, {'<', '@'}, {'\\', '\\'}, {'^', '^'},
  {'|', '|'},
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(70);
      ADVANCE_MAP(
        '"', 103,
        '#', 12,
        '(', 73,
        ')', 76,
        '*', 74,
        ',', 77,
        '.', 102,
        '/', 20,
        '0', 95,
        ';', 23,
      );
      if (lookahead == '\\') SKIP(66);
      if (lookahead == 'a') ADVANCE(98);
      if (lookahead == 'd') ADVANCE(97);
      if (lookahead == 'e') ADVANCE(99);
      if (lookahead == 'm') ADVANCE(41);
      if (lookahead == 's') ADVANCE(30);
      if (lookahead == 't') ADVANCE(49);
      if (lookahead == 'u') ADVANCE(44);
      if (lookahead == '{') ADVANCE(86);
      if (lookahead == '}') ADVANCE(87);
      if (lookahead == '8' ||
          lookahead == '9') ADVANCE(94);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(0);
      if (('1' <= lookahead && lookahead <= '7')) ADVANCE(94);
      if (('A' <= lookahead && lookahead <= 'F') ||
          ('b' <= lookahead && lookahead <= 'f')) ADVANCE(96);
      END_STATE();
    case 1:
      if (lookahead == '\n') SKIP(14);
      END_STATE();
    case 2:
      if (lookahead == '\n') SKIP(14);
      if (lookahead == '\r') SKIP(1);
      END_STATE();
    case 3:
      if (lookahead == '\n') SKIP(15);
      END_STATE();
    case 4:
      if (lookahead == '\n') SKIP(15);
      if (lookahead == '\r') SKIP(3);
      END_STATE();
    case 5:
      if (lookahead == '\n') SKIP(10);
      END_STATE();
    case 6:
      if (lookahead == '\n') SKIP(10);
      if (lookahead == '\r') SKIP(5);
      END_STATE();
    case 7:
      if (lookahead == '\n') SKIP(11);
      if (lookahead == '"') ADVANCE(103);
      if (lookahead == '/') ADVANCE(104);
      if (lookahead == '\\') ADVANCE(8);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') ADVANCE(107);
      if (lookahead != 0) ADVANCE(108);
      END_STATE();
    case 8:
      if (lookahead == '\n') ADVANCE(110);
      if (lookahead == '\r') ADVANCE(109);
      if (lookahead == 'U') ADVANCE(60);
      if (lookahead == 'u') ADVANCE(56);
      if (lookahead == 'x') ADVANCE(53);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(112);
      if (lookahead != 0) ADVANCE(109);
      END_STATE();
    case 9:
      if (lookahead == '\r') ADVANCE(91);
      if (lookahead == '\\') ADVANCE(89);
      if (lookahead != 0) ADVANCE(90);
      END_STATE();
    case 10:
      if (lookahead == '"') ADVANCE(103);
      if (lookahead == '(') ADVANCE(51);
      if (lookahead == '/') ADVANCE(20);
      if (lookahead == '\\') SKIP(6);
      if (lookahead == '}') ADVANCE(87);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(10);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(79);
      END_STATE();
    case 11:
      if (lookahead == '"') ADVANCE(103);
      if (lookahead == '/') ADVANCE(20);
      if (lookahead == '\\') ADVANCE(8);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(11);
      END_STATE();
    case 12:
      if (lookahead == '#') ADVANCE(61);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(80);
      END_STATE();
    case 13:
      if (lookahead == '#') ADVANCE(61);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(124);
      END_STATE();
    case 14:
      if (lookahead == '#') ADVANCE(13);
      if (lookahead == '(') ADVANCE(51);
      if (lookahead == ')') ADVANCE(76);
      if (lookahead == '/') ADVANCE(20);
      if (lookahead == '\\') SKIP(2);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(14);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(122);
      END_STATE();
    case 15:
      if (lookahead == '#') ADVANCE(62);
      if (lookahead == ')') ADVANCE(76);
      if (lookahead == '*') ADVANCE(74);
      if (lookahead == '/') ADVANCE(20);
      if (lookahead == '\\') SKIP(4);
      if (lookahead == 'm') ADVANCE(120);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(15);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(122);
      END_STATE();
    case 16:
      if (lookahead == '(') ADVANCE(50);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(80);
      END_STATE();
    case 17:
      if (lookahead == '(') ADVANCE(50);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(124);
      END_STATE();
    case 18:
      if (lookahead == ')') ADVANCE(123);
      if (set_contains(sym_operator_id_character_set_2, 9, lookahead)) ADVANCE(18);
      END_STATE();
    case 19:
      if (lookahead == ')') ADVANCE(116);
      if (set_contains(sym_operator_id_character_set_2, 9, lookahead)) ADVANCE(19);
      END_STATE();
    case 20:
      if (lookahead == '*') ADVANCE(22);
      if (lookahead == '/') ADVANCE(90);
      END_STATE();
    case 21:
      if (lookahead == '*') ADVANCE(21);
      if (lookahead == '/') ADVANCE(88);
      if (lookahead != 0) ADVANCE(22);
      END_STATE();
    case 22:
      if (lookahead == '*') ADVANCE(21);
      if (lookahead != 0) ADVANCE(22);
      END_STATE();
    case 23:
      if (lookahead == ';') ADVANCE(78);
      END_STATE();
    case 24:
      if (lookahead == 'a') ADVANCE(45);
      END_STATE();
    case 25:
      if (lookahead == 'a') ADVANCE(92);
      END_STATE();
    case 26:
      if (lookahead == 'a') ADVANCE(43);
      END_STATE();
    case 27:
      if (lookahead == 'c') ADVANCE(39);
      END_STATE();
    case 28:
      if (lookahead == 'd') ADVANCE(47);
      END_STATE();
    case 29:
      if (lookahead == 'd') ADVANCE(34);
      END_STATE();
    case 30:
      if (lookahead == 'e') ADVANCE(38);
      END_STATE();
    case 31:
      if (lookahead == 'e') ADVANCE(82);
      END_STATE();
    case 32:
      if (lookahead == 'e') ADVANCE(93);
      END_STATE();
    case 33:
      if (lookahead == 'e') ADVANCE(71);
      END_STATE();
    case 34:
      if (lookahead == 'e') ADVANCE(75);
      END_STATE();
    case 35:
      if (lookahead == 'f') ADVANCE(85);
      END_STATE();
    case 36:
      if (lookahead == 'i') ADVANCE(26);
      END_STATE();
    case 37:
      if (lookahead == 'l') ADVANCE(36);
      if (lookahead == 's') ADVANCE(83);
      END_STATE();
    case 38:
      if (lookahead == 'l') ADVANCE(35);
      END_STATE();
    case 39:
      if (lookahead == 'l') ADVANCE(46);
      END_STATE();
    case 40:
      if (lookahead == 'l') ADVANCE(33);
      END_STATE();
    case 41:
      if (lookahead == 'o') ADVANCE(28);
      END_STATE();
    case 42:
      if (lookahead == 'p') ADVANCE(32);
      END_STATE();
    case 43:
      if (lookahead == 's') ADVANCE(84);
      END_STATE();
    case 44:
      if (lookahead == 's') ADVANCE(31);
      END_STATE();
    case 45:
      if (lookahead == 't') ADVANCE(25);
      END_STATE();
    case 46:
      if (lookahead == 'u') ADVANCE(29);
      END_STATE();
    case 47:
      if (lookahead == 'u') ADVANCE(40);
      END_STATE();
    case 48:
      if (lookahead == 'x') ADVANCE(27);
      END_STATE();
    case 49:
      if (lookahead == 'y') ADVANCE(42);
      END_STATE();
    case 50:
      if (set_contains(sym_operator_id_character_set_1, 9, lookahead)) ADVANCE(18);
      END_STATE();
    case 51:
      if (set_contains(sym_operator_id_character_set_1, 9, lookahead)) ADVANCE(19);
      END_STATE();
    case 52:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(109);
      END_STATE();
    case 53:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(115);
      END_STATE();
    case 54:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(52);
      END_STATE();
    case 55:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(54);
      END_STATE();
    case 56:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(55);
      END_STATE();
    case 57:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(56);
      END_STATE();
    case 58:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(57);
      END_STATE();
    case 59:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(58);
      END_STATE();
    case 60:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(59);
      END_STATE();
    case 61:
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(125);
      END_STATE();
    case 62:
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(124);
      END_STATE();
    case 63:
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(79);
      END_STATE();
    case 64:
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 65:
      if (eof) ADVANCE(70);
      if (lookahead == '\n') SKIP(0);
      END_STATE();
    case 66:
      if (eof) ADVANCE(70);
      if (lookahead == '\n') SKIP(0);
      if (lookahead == '\r') SKIP(65);
      END_STATE();
    case 67:
      if (eof) ADVANCE(70);
      if (lookahead == '\n') SKIP(69);
      END_STATE();
    case 68:
      if (eof) ADVANCE(70);
      if (lookahead == '\n') SKIP(69);
      if (lookahead == '\r') SKIP(67);
      END_STATE();
    case 69:
      if (eof) ADVANCE(70);
      if (lookahead == '#') ADVANCE(64);
      if (lookahead == '(') ADVANCE(73);
      if (lookahead == ')') ADVANCE(76);
      if (lookahead == '*') ADVANCE(74);
      if (lookahead == ',') ADVANCE(77);
      if (lookahead == '/') ADVANCE(20);
      if (lookahead == ';') ADVANCE(23);
      if (lookahead == '\\') SKIP(68);
      if (lookahead == 'a') ADVANCE(37);
      if (lookahead == 'd') ADVANCE(24);
      if (lookahead == 'e') ADVANCE(48);
      if (lookahead == 's') ADVANCE(30);
      if (lookahead == 'u') ADVANCE(44);
      if (lookahead == '{') ADVANCE(86);
      if (lookahead == '}') ADVANCE(87);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(69);
      END_STATE();
    case 70:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 71:
      ACCEPT_TOKEN(anon_sym_module);
      END_STATE();
    case 72:
      ACCEPT_TOKEN(anon_sym_module);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(122);
      END_STATE();
    case 73:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 74:
      ACCEPT_TOKEN(anon_sym_STAR);
      END_STATE();
    case 75:
      ACCEPT_TOKEN(anon_sym_exclude);
      END_STATE();
    case 76:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 77:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 78:
      ACCEPT_TOKEN(anon_sym_SEMI_SEMI);
      END_STATE();
    case 79:
      ACCEPT_TOKEN(sym_module_path);
      if (lookahead == '/') ADVANCE(63);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(79);
      END_STATE();
    case 80:
      ACCEPT_TOKEN(sym_module_id);
      if (lookahead == '#') ADVANCE(16);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(80);
      END_STATE();
    case 81:
      ACCEPT_TOKEN(sym_module_id);
      if (lookahead == '#') ADVANCE(64);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(81);
      END_STATE();
    case 82:
      ACCEPT_TOKEN(anon_sym_use);
      END_STATE();
    case 83:
      ACCEPT_TOKEN(anon_sym_as);
      END_STATE();
    case 84:
      ACCEPT_TOKEN(anon_sym_alias);
      END_STATE();
    case 85:
      ACCEPT_TOKEN(anon_sym_self);
      END_STATE();
    case 86:
      ACCEPT_TOKEN(anon_sym_LBRACE);
      END_STATE();
    case 87:
      ACCEPT_TOKEN(anon_sym_RBRACE);
      END_STATE();
    case 88:
      ACCEPT_TOKEN(sym_comment);
      END_STATE();
    case 89:
      ACCEPT_TOKEN(sym_comment);
      if (lookahead == '\r') ADVANCE(91);
      if (lookahead == '\\') ADVANCE(89);
      if (lookahead != 0) ADVANCE(90);
      END_STATE();
    case 90:
      ACCEPT_TOKEN(sym_comment);
      if (lookahead == '\\') ADVANCE(9);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(90);
      END_STATE();
    case 91:
      ACCEPT_TOKEN(sym_comment);
      if (lookahead == '\\') ADVANCE(9);
      if (lookahead != 0) ADVANCE(90);
      END_STATE();
    case 92:
      ACCEPT_TOKEN(anon_sym_data);
      END_STATE();
    case 93:
      ACCEPT_TOKEN(sym_type_decl);
      END_STATE();
    case 94:
      ACCEPT_TOKEN(sym__digit);
      END_STATE();
    case 95:
      ACCEPT_TOKEN(sym__digit);
      if (lookahead == 'O' ||
          lookahead == 'o') ADVANCE(101);
      if (lookahead == 'X' ||
          lookahead == 'x') ADVANCE(100);
      END_STATE();
    case 96:
      ACCEPT_TOKEN(sym__hex);
      END_STATE();
    case 97:
      ACCEPT_TOKEN(sym__hex);
      if (lookahead == 'a') ADVANCE(45);
      END_STATE();
    case 98:
      ACCEPT_TOKEN(sym__hex);
      if (lookahead == 'l') ADVANCE(36);
      if (lookahead == 's') ADVANCE(83);
      END_STATE();
    case 99:
      ACCEPT_TOKEN(sym__hex);
      if (lookahead == 'x') ADVANCE(27);
      END_STATE();
    case 100:
      ACCEPT_TOKEN(aux_sym__hex_integer_token1);
      END_STATE();
    case 101:
      ACCEPT_TOKEN(aux_sym__octet_integer_token1);
      END_STATE();
    case 102:
      ACCEPT_TOKEN(anon_sym_DOT);
      END_STATE();
    case 103:
      ACCEPT_TOKEN(anon_sym_DQUOTE);
      END_STATE();
    case 104:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '*') ADVANCE(106);
      if (lookahead == '/') ADVANCE(108);
      if (lookahead != 0 &&
          lookahead != '\n' &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(108);
      END_STATE();
    case 105:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '*') ADVANCE(105);
      if (lookahead == '/') ADVANCE(108);
      if (lookahead != 0 &&
          lookahead != '\n' &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(106);
      END_STATE();
    case 106:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '*') ADVANCE(105);
      if (lookahead != 0 &&
          lookahead != '\n' &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(106);
      END_STATE();
    case 107:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead == '/') ADVANCE(104);
      if (lookahead == '\t' ||
          (0x0b <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') ADVANCE(107);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(108);
      END_STATE();
    case 108:
      ACCEPT_TOKEN(aux_sym_string_token1);
      if (lookahead != 0 &&
          lookahead != '\n' &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(108);
      END_STATE();
    case 109:
      ACCEPT_TOKEN(sym__escape_sequence);
      END_STATE();
    case 110:
      ACCEPT_TOKEN(sym__escape_sequence);
      if (lookahead == '\\') ADVANCE(8);
      END_STATE();
    case 111:
      ACCEPT_TOKEN(sym__escape_sequence);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(109);
      END_STATE();
    case 112:
      ACCEPT_TOKEN(sym__escape_sequence);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(111);
      END_STATE();
    case 113:
      ACCEPT_TOKEN(sym__escape_sequence);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(109);
      END_STATE();
    case 114:
      ACCEPT_TOKEN(sym__escape_sequence);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(113);
      END_STATE();
    case 115:
      ACCEPT_TOKEN(sym__escape_sequence);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(114);
      END_STATE();
    case 116:
      ACCEPT_TOKEN(sym_operator_id);
      END_STATE();
    case 117:
      ACCEPT_TOKEN(sym_id);
      if (lookahead == 'd') ADVANCE(121);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(122);
      END_STATE();
    case 118:
      ACCEPT_TOKEN(sym_id);
      if (lookahead == 'e') ADVANCE(72);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(122);
      END_STATE();
    case 119:
      ACCEPT_TOKEN(sym_id);
      if (lookahead == 'l') ADVANCE(118);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(122);
      END_STATE();
    case 120:
      ACCEPT_TOKEN(sym_id);
      if (lookahead == 'o') ADVANCE(117);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(122);
      END_STATE();
    case 121:
      ACCEPT_TOKEN(sym_id);
      if (lookahead == 'u') ADVANCE(119);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(122);
      END_STATE();
    case 122:
      ACCEPT_TOKEN(sym_id);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(122);
      END_STATE();
    case 123:
      ACCEPT_TOKEN(sym_qualified_id);
      END_STATE();
    case 124:
      ACCEPT_TOKEN(sym_qualified_id);
      if (lookahead == '#') ADVANCE(17);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(124);
      END_STATE();
    case 125:
      ACCEPT_TOKEN(sym_force_id);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(125);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 0},
  [2] = {.lex_state = 69},
  [3] = {.lex_state = 69},
  [4] = {.lex_state = 69},
  [5] = {.lex_state = 69},
  [6] = {.lex_state = 14},
  [7] = {.lex_state = 15},
  [8] = {.lex_state = 14},
  [9] = {.lex_state = 15},
  [10] = {.lex_state = 14},
  [11] = {.lex_state = 10},
  [12] = {.lex_state = 14},
  [13] = {.lex_state = 0},
  [14] = {.lex_state = 0},
  [15] = {.lex_state = 15},
  [16] = {.lex_state = 14},
  [17] = {.lex_state = 7},
  [18] = {.lex_state = 10},
  [19] = {.lex_state = 69},
  [20] = {.lex_state = 10},
  [21] = {.lex_state = 7},
  [22] = {.lex_state = 69},
  [23] = {.lex_state = 7},
  [24] = {.lex_state = 69},
  [25] = {.lex_state = 14},
  [26] = {.lex_state = 10},
  [27] = {.lex_state = 69},
  [28] = {.lex_state = 69},
  [29] = {.lex_state = 10},
  [30] = {.lex_state = 0},
  [31] = {.lex_state = 0},
  [32] = {.lex_state = 69},
  [33] = {.lex_state = 0},
  [34] = {.lex_state = 0},
  [35] = {.lex_state = 69},
  [36] = {.lex_state = 69},
  [37] = {.lex_state = 69},
  [38] = {.lex_state = 69},
  [39] = {.lex_state = 0},
  [40] = {.lex_state = 0},
  [41] = {.lex_state = 69},
  [42] = {.lex_state = 0},
  [43] = {.lex_state = 0},
  [44] = {.lex_state = 0},
  [45] = {.lex_state = 69},
  [46] = {.lex_state = 69},
  [47] = {.lex_state = 0},
  [48] = {.lex_state = 69},
  [49] = {.lex_state = 69},
  [50] = {.lex_state = 0},
  [51] = {.lex_state = 0},
  [52] = {.lex_state = 69},
  [53] = {.lex_state = 0},
  [54] = {.lex_state = 0},
  [55] = {.lex_state = 0},
  [56] = {.lex_state = 0},
  [57] = {.lex_state = 0},
  [58] = {.lex_state = 69},
  [59] = {.lex_state = 0},
  [60] = {.lex_state = 0},
  [61] = {.lex_state = 0},
  [62] = {.lex_state = 0},
  [63] = {.lex_state = 69},
  [64] = {.lex_state = 69},
  [65] = {.lex_state = 69},
  [66] = {.lex_state = 69},
  [67] = {.lex_state = 0},
  [68] = {.lex_state = 0},
  [69] = {.lex_state = 0},
  [70] = {.lex_state = 0},
  [71] = {.lex_state = 0},
  [72] = {.lex_state = 0},
  [73] = {.lex_state = 0},
  [74] = {.lex_state = 0},
  [75] = {.lex_state = 0},
  [76] = {.lex_state = 69},
  [77] = {.lex_state = 0},
  [78] = {.lex_state = 0},
  [79] = {.lex_state = 0},
  [80] = {.lex_state = 0},
  [81] = {.lex_state = 10},
  [82] = {.lex_state = 0},
  [83] = {.lex_state = 0},
  [84] = {.lex_state = 0},
  [85] = {.lex_state = 0},
  [86] = {.lex_state = 0},
  [87] = {.lex_state = 0},
  [88] = {.lex_state = 69},
  [89] = {.lex_state = 0},
  [90] = {.lex_state = 0},
  [91] = {.lex_state = 0},
  [92] = {.lex_state = 69},
  [93] = {.lex_state = 0},
  [94] = {.lex_state = 0},
  [95] = {.lex_state = 14},
  [96] = {.lex_state = 14},
  [97] = {.lex_state = 69},
  [98] = {.lex_state = 0},
  [99] = {.lex_state = 14},
  [100] = {.lex_state = 0},
  [101] = {.lex_state = 0},
  [102] = {.lex_state = 0},
  [103] = {.lex_state = 0},
  [104] = {.lex_state = 0},
  [105] = {.lex_state = 0},
  [106] = {.lex_state = 69},
  [107] = {.lex_state = 0},
  [108] = {.lex_state = 0},
  [109] = {.lex_state = 69},
  [110] = {.lex_state = 0},
  [111] = {.lex_state = 10},
  [112] = {.lex_state = 0},
  [113] = {.lex_state = 14},
  [114] = {.lex_state = 0},
  [115] = {.lex_state = 0},
  [116] = {.lex_state = 0},
  [117] = {.lex_state = 10},
  [118] = {.lex_state = 0},
  [119] = {.lex_state = 0},
  [120] = {.lex_state = 14},
  [121] = {.lex_state = 10},
  [122] = {.lex_state = 0},
  [123] = {.lex_state = 0},
  [124] = {.lex_state = 0},
  [125] = {.lex_state = 0},
  [126] = {.lex_state = 0},
  [127] = {.lex_state = 69},
  [128] = {.lex_state = 0},
  [129] = {.lex_state = 0},
  [130] = {.lex_state = 0},
  [131] = {.lex_state = 0},
  [132] = {.lex_state = 0},
  [133] = {.lex_state = 0},
  [134] = {.lex_state = 14},
  [135] = {.lex_state = 0},
  [136] = {.lex_state = 0},
  [137] = {.lex_state = 10},
  [138] = {.lex_state = 0},
  [139] = {.lex_state = 0},
  [140] = {.lex_state = 0},
  [141] = {.lex_state = 0},
  [142] = {.lex_state = 0},
  [143] = {.lex_state = 0},
  [144] = {.lex_state = 14},
  [145] = {.lex_state = 10},
  [146] = {.lex_state = 0},
  [147] = {.lex_state = 0},
  [148] = {.lex_state = 0},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_module] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_STAR] = ACTIONS(1),
    [anon_sym_exclude] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [anon_sym_COMMA] = ACTIONS(1),
    [anon_sym_SEMI_SEMI] = ACTIONS(1),
    [sym_module_id] = ACTIONS(1),
    [anon_sym_use] = ACTIONS(1),
    [anon_sym_as] = ACTIONS(1),
    [anon_sym_alias] = ACTIONS(1),
    [anon_sym_self] = ACTIONS(1),
    [anon_sym_LBRACE] = ACTIONS(1),
    [anon_sym_RBRACE] = ACTIONS(1),
    [sym_comment] = ACTIONS(3),
    [anon_sym_data] = ACTIONS(1),
    [sym_type_decl] = ACTIONS(1),
    [sym__digit] = ACTIONS(1),
    [sym__octet] = ACTIONS(1),
    [sym__hex] = ACTIONS(1),
    [aux_sym__hex_integer_token1] = ACTIONS(1),
    [aux_sym__octet_integer_token1] = ACTIONS(1),
    [anon_sym_DOT] = ACTIONS(1),
    [anon_sym_DQUOTE] = ACTIONS(1),
    [sym_qualified_id] = ACTIONS(1),
    [sym_force_id] = ACTIONS(1),
  },
  [1] = {
    [sym_source_file] = STATE(136),
    [sym_module] = STATE(3),
    [anon_sym_module] = ACTIONS(5),
    [sym_comment] = ACTIONS(3),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 6,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(7), 1,
      ts_builtin_sym_end,
    ACTIONS(9), 1,
      anon_sym_use,
    ACTIONS(12), 1,
      anon_sym_data,
    STATE(76), 1,
      sym_data,
    STATE(2), 4,
      sym__top,
      sym_use,
      sym_declaration,
      aux_sym_source_file_repeat1,
  [22] = 6,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(15), 1,
      ts_builtin_sym_end,
    ACTIONS(17), 1,
      anon_sym_use,
    ACTIONS(19), 1,
      anon_sym_data,
    STATE(76), 1,
      sym_data,
    STATE(5), 4,
      sym__top,
      sym_use,
      sym_declaration,
      aux_sym_source_file_repeat1,
  [44] = 6,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(21), 1,
      anon_sym_STAR,
    ACTIONS(27), 1,
      anon_sym_self,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(25), 2,
      anon_sym_as,
      anon_sym_alias,
    ACTIONS(23), 3,
      anon_sym_COMMA,
      anon_sym_SEMI_SEMI,
      anon_sym_RBRACE,
  [66] = 6,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(17), 1,
      anon_sym_use,
    ACTIONS(19), 1,
      anon_sym_data,
    ACTIONS(31), 1,
      ts_builtin_sym_end,
    STATE(76), 1,
      sym_data,
    STATE(2), 4,
      sym__top,
      sym_use,
      sym_declaration,
      aux_sym_source_file_repeat1,
  [88] = 5,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(33), 1,
      anon_sym_RPAREN,
    ACTIONS(35), 1,
      sym_operator_id,
    STATE(70), 1,
      sym_identifier,
    ACTIONS(37), 3,
      sym_id,
      sym_qualified_id,
      sym_force_id,
  [106] = 7,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(39), 1,
      anon_sym_module,
    ACTIONS(41), 1,
      anon_sym_STAR,
    ACTIONS(43), 1,
      anon_sym_RPAREN,
    ACTIONS(45), 1,
      sym_id,
    ACTIONS(47), 1,
      sym_qualified_id,
    STATE(53), 1,
      sym_module_export,
  [128] = 5,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(49), 1,
      anon_sym_RPAREN,
    ACTIONS(51), 1,
      sym_operator_id,
    STATE(83), 1,
      sym_identifier,
    ACTIONS(37), 3,
      sym_id,
      sym_qualified_id,
      sym_force_id,
  [146] = 7,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(39), 1,
      anon_sym_module,
    ACTIONS(53), 1,
      anon_sym_STAR,
    ACTIONS(55), 1,
      anon_sym_RPAREN,
    ACTIONS(57), 1,
      sym_id,
    ACTIONS(59), 1,
      sym_qualified_id,
    STATE(39), 1,
      sym_module_export,
  [168] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(61), 1,
      sym_operator_id,
    STATE(108), 1,
      sym_identifier,
    ACTIONS(37), 3,
      sym_id,
      sym_qualified_id,
      sym_force_id,
  [183] = 6,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(63), 1,
      sym_module_path,
    ACTIONS(65), 1,
      anon_sym_DQUOTE,
    STATE(111), 1,
      sym_module_attr,
    STATE(121), 1,
      sym_string,
    STATE(122), 1,
      sym_use_piece,
  [202] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(67), 1,
      anon_sym_RPAREN,
    STATE(87), 1,
      sym_identifier,
    ACTIONS(37), 3,
      sym_id,
      sym_qualified_id,
      sym_force_id,
  [217] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(69), 1,
      anon_sym_STAR,
    ACTIONS(73), 1,
      anon_sym_LBRACE,
    ACTIONS(71), 3,
      anon_sym_COMMA,
      anon_sym_SEMI_SEMI,
      anon_sym_RBRACE,
  [232] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(75), 1,
      anon_sym_STAR,
    ACTIONS(79), 1,
      anon_sym_LBRACE,
    ACTIONS(77), 3,
      anon_sym_COMMA,
      anon_sym_SEMI_SEMI,
      anon_sym_RBRACE,
  [247] = 6,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(39), 1,
      anon_sym_module,
    ACTIONS(81), 1,
      anon_sym_STAR,
    ACTIONS(83), 1,
      sym_id,
    ACTIONS(85), 1,
      sym_qualified_id,
    STATE(103), 1,
      sym_module_export,
  [266] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(87), 1,
      anon_sym_RPAREN,
    STATE(72), 1,
      sym_identifier,
    ACTIONS(37), 3,
      sym_id,
      sym_qualified_id,
      sym_force_id,
  [281] = 5,
    ACTIONS(89), 1,
      sym_comment,
    ACTIONS(91), 1,
      anon_sym_DQUOTE,
    ACTIONS(93), 1,
      aux_sym_string_token1,
    ACTIONS(96), 1,
      sym__escape_sequence,
    STATE(17), 1,
      aux_sym_string_repeat1,
  [297] = 5,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(63), 1,
      sym_module_path,
    ACTIONS(99), 1,
      anon_sym_RBRACE,
    ACTIONS(101), 1,
      sym_operator_id,
    STATE(75), 1,
      sym_use_piece,
  [313] = 3,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(103), 1,
      anon_sym_exclude,
    ACTIONS(105), 3,
      anon_sym_COMMA,
      anon_sym_SEMI_SEMI,
      anon_sym_RBRACE,
  [325] = 5,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(65), 1,
      anon_sym_DQUOTE,
    ACTIONS(107), 1,
      sym_module_path,
    STATE(117), 1,
      sym_module_attr,
    STATE(121), 1,
      sym_string,
  [341] = 5,
    ACTIONS(89), 1,
      sym_comment,
    ACTIONS(109), 1,
      anon_sym_DQUOTE,
    ACTIONS(111), 1,
      aux_sym_string_token1,
    ACTIONS(113), 1,
      sym__escape_sequence,
    STATE(17), 1,
      aux_sym_string_repeat1,
  [357] = 5,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(33), 1,
      anon_sym_RPAREN,
    ACTIONS(115), 1,
      anon_sym_exclude,
    ACTIONS(117), 1,
      anon_sym_COMMA,
    STATE(31), 1,
      aux_sym_module_repeat1,
  [373] = 5,
    ACTIONS(89), 1,
      sym_comment,
    ACTIONS(119), 1,
      anon_sym_DQUOTE,
    ACTIONS(121), 1,
      aux_sym_string_token1,
    ACTIONS(123), 1,
      sym__escape_sequence,
    STATE(21), 1,
      aux_sym_string_repeat1,
  [389] = 3,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(125), 1,
      anon_sym_exclude,
    ACTIONS(127), 3,
      anon_sym_COMMA,
      anon_sym_SEMI_SEMI,
      anon_sym_RBRACE,
  [401] = 3,
    ACTIONS(3), 1,
      sym_comment,
    STATE(94), 1,
      sym_identifier,
    ACTIONS(37), 3,
      sym_id,
      sym_qualified_id,
      sym_force_id,
  [413] = 5,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(63), 1,
      sym_module_path,
    ACTIONS(129), 1,
      anon_sym_RBRACE,
    ACTIONS(131), 1,
      sym_operator_id,
    STATE(57), 1,
      sym_use_piece,
  [429] = 5,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(43), 1,
      anon_sym_RPAREN,
    ACTIONS(117), 1,
      anon_sym_COMMA,
    ACTIONS(133), 1,
      anon_sym_exclude,
    STATE(51), 1,
      aux_sym_module_repeat1,
  [445] = 3,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(135), 1,
      anon_sym_exclude,
    ACTIONS(137), 3,
      anon_sym_COMMA,
      anon_sym_SEMI_SEMI,
      anon_sym_RBRACE,
  [457] = 5,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(63), 1,
      sym_module_path,
    ACTIONS(139), 1,
      anon_sym_RBRACE,
    ACTIONS(141), 1,
      sym_operator_id,
    STATE(80), 1,
      sym_use_piece,
  [473] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(143), 1,
      anon_sym_LPAREN,
    ACTIONS(145), 1,
      anon_sym_STAR,
    ACTIONS(147), 2,
      anon_sym_RPAREN,
      anon_sym_COMMA,
  [487] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(49), 1,
      anon_sym_RPAREN,
    ACTIONS(117), 1,
      anon_sym_COMMA,
    STATE(69), 1,
      aux_sym_module_repeat1,
  [500] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(149), 3,
      ts_builtin_sym_end,
      anon_sym_use,
      anon_sym_data,
  [509] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(151), 1,
      anon_sym_LPAREN,
    ACTIONS(153), 1,
      anon_sym_STAR,
    ACTIONS(155), 1,
      anon_sym_SEMI_SEMI,
  [522] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(157), 3,
      anon_sym_COMMA,
      anon_sym_SEMI_SEMI,
      anon_sym_RBRACE,
  [531] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(159), 3,
      ts_builtin_sym_end,
      anon_sym_use,
      anon_sym_data,
  [540] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(161), 3,
      ts_builtin_sym_end,
      anon_sym_use,
      anon_sym_data,
  [549] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(163), 1,
      anon_sym_LPAREN,
    ACTIONS(165), 1,
      anon_sym_STAR,
    ACTIONS(167), 1,
      sym_module_id,
  [562] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(169), 3,
      ts_builtin_sym_end,
      anon_sym_use,
      anon_sym_data,
  [571] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(43), 1,
      anon_sym_RPAREN,
    ACTIONS(117), 1,
      anon_sym_COMMA,
    STATE(51), 1,
      aux_sym_module_repeat1,
  [584] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(171), 1,
      anon_sym_RPAREN,
    ACTIONS(173), 1,
      anon_sym_COMMA,
    STATE(43), 1,
      aux_sym_module_repeat2,
  [597] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(175), 3,
      ts_builtin_sym_end,
      anon_sym_use,
      anon_sym_data,
  [606] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(177), 3,
      anon_sym_COMMA,
      anon_sym_SEMI_SEMI,
      anon_sym_RBRACE,
  [615] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(179), 1,
      anon_sym_RPAREN,
    ACTIONS(181), 1,
      anon_sym_COMMA,
    STATE(43), 1,
      aux_sym_module_repeat2,
  [628] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(117), 1,
      anon_sym_COMMA,
    ACTIONS(184), 1,
      anon_sym_RPAREN,
    STATE(69), 1,
      aux_sym_module_repeat1,
  [641] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(186), 3,
      ts_builtin_sym_end,
      anon_sym_use,
      anon_sym_data,
  [650] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(188), 3,
      ts_builtin_sym_end,
      anon_sym_use,
      anon_sym_data,
  [659] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(117), 1,
      anon_sym_COMMA,
    ACTIONS(171), 1,
      anon_sym_RPAREN,
    STATE(68), 1,
      aux_sym_module_repeat1,
  [672] = 3,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(190), 1,
      anon_sym_exclude,
    ACTIONS(147), 2,
      anon_sym_RPAREN,
      anon_sym_COMMA,
  [683] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(192), 3,
      ts_builtin_sym_end,
      anon_sym_use,
      anon_sym_data,
  [692] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(194), 1,
      anon_sym_RPAREN,
    ACTIONS(196), 1,
      anon_sym_COMMA,
    STATE(77), 1,
      aux_sym_module_export_repeat1,
  [705] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(33), 1,
      anon_sym_RPAREN,
    ACTIONS(117), 1,
      anon_sym_COMMA,
    STATE(69), 1,
      aux_sym_module_repeat1,
  [718] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(198), 3,
      ts_builtin_sym_end,
      anon_sym_use,
      anon_sym_data,
  [727] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(33), 1,
      anon_sym_RPAREN,
    ACTIONS(117), 1,
      anon_sym_COMMA,
    STATE(31), 1,
      aux_sym_module_repeat1,
  [740] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(200), 1,
      anon_sym_LPAREN,
    ACTIONS(202), 1,
      anon_sym_STAR,
    ACTIONS(204), 1,
      anon_sym_SEMI_SEMI,
  [753] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(206), 3,
      anon_sym_COMMA,
      anon_sym_SEMI_SEMI,
      anon_sym_RBRACE,
  [762] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(71), 3,
      anon_sym_COMMA,
      anon_sym_SEMI_SEMI,
      anon_sym_RBRACE,
  [771] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(139), 1,
      anon_sym_RBRACE,
    ACTIONS(208), 1,
      anon_sym_COMMA,
    STATE(82), 1,
      aux_sym_use_piece_repeat1,
  [784] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(210), 3,
      ts_builtin_sym_end,
      anon_sym_use,
      anon_sym_data,
  [793] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(87), 1,
      anon_sym_RPAREN,
    ACTIONS(196), 1,
      anon_sym_COMMA,
    STATE(77), 1,
      aux_sym_module_export_repeat1,
  [806] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(67), 1,
      anon_sym_RPAREN,
    ACTIONS(212), 1,
      anon_sym_COMMA,
    STATE(85), 1,
      aux_sym_module_export_repeat2,
  [819] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(214), 3,
      anon_sym_COMMA,
      anon_sym_SEMI_SEMI,
      anon_sym_RBRACE,
  [828] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(117), 1,
      anon_sym_COMMA,
    ACTIONS(216), 1,
      anon_sym_RPAREN,
    STATE(44), 1,
      aux_sym_module_repeat1,
  [841] = 3,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(218), 1,
      anon_sym_exclude,
    ACTIONS(220), 2,
      anon_sym_RPAREN,
      anon_sym_COMMA,
  [852] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(222), 3,
      ts_builtin_sym_end,
      anon_sym_use,
      anon_sym_data,
  [861] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(224), 3,
      ts_builtin_sym_end,
      anon_sym_use,
      anon_sym_data,
  [870] = 3,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(226), 1,
      anon_sym_exclude,
    ACTIONS(228), 2,
      anon_sym_RPAREN,
      anon_sym_COMMA,
  [881] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(87), 1,
      anon_sym_RPAREN,
    ACTIONS(212), 1,
      anon_sym_COMMA,
    STATE(84), 1,
      aux_sym_module_export_repeat2,
  [894] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(117), 1,
      anon_sym_COMMA,
    ACTIONS(216), 1,
      anon_sym_RPAREN,
    STATE(69), 1,
      aux_sym_module_repeat1,
  [907] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(228), 1,
      anon_sym_RPAREN,
    ACTIONS(230), 1,
      anon_sym_COMMA,
    STATE(69), 1,
      aux_sym_module_repeat1,
  [920] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(49), 1,
      anon_sym_RPAREN,
    ACTIONS(173), 1,
      anon_sym_COMMA,
    STATE(86), 1,
      aux_sym_module_repeat2,
  [933] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(233), 1,
      anon_sym_RPAREN,
    ACTIONS(235), 1,
      anon_sym_COMMA,
    STATE(71), 1,
      aux_sym_module_export_repeat2,
  [946] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(196), 1,
      anon_sym_COMMA,
    ACTIONS(238), 1,
      anon_sym_RPAREN,
    STATE(50), 1,
      aux_sym_module_export_repeat1,
  [959] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(240), 1,
      anon_sym_COMMA,
    ACTIONS(243), 1,
      anon_sym_RBRACE,
    STATE(73), 1,
      aux_sym_use_piece_repeat1,
  [972] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(208), 1,
      anon_sym_COMMA,
    ACTIONS(245), 1,
      anon_sym_RBRACE,
    STATE(73), 1,
      aux_sym_use_piece_repeat1,
  [985] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(208), 1,
      anon_sym_COMMA,
    ACTIONS(245), 1,
      anon_sym_RBRACE,
    STATE(89), 1,
      aux_sym_use_piece_repeat1,
  [998] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(247), 3,
      ts_builtin_sym_end,
      anon_sym_use,
      anon_sym_data,
  [1007] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(249), 1,
      anon_sym_RPAREN,
    ACTIONS(251), 1,
      anon_sym_COMMA,
    STATE(77), 1,
      aux_sym_module_export_repeat1,
  [1020] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(254), 3,
      anon_sym_COMMA,
      anon_sym_SEMI_SEMI,
      anon_sym_RBRACE,
  [1029] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(256), 3,
      anon_sym_COMMA,
      anon_sym_SEMI_SEMI,
      anon_sym_RBRACE,
  [1038] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(99), 1,
      anon_sym_RBRACE,
    ACTIONS(208), 1,
      anon_sym_COMMA,
    STATE(74), 1,
      aux_sym_use_piece_repeat1,
  [1051] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(63), 1,
      sym_module_path,
    ACTIONS(258), 1,
      sym_operator_id,
    STATE(98), 1,
      sym_use_piece,
  [1064] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(99), 1,
      anon_sym_RBRACE,
    ACTIONS(208), 1,
      anon_sym_COMMA,
    STATE(73), 1,
      aux_sym_use_piece_repeat1,
  [1077] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(173), 1,
      anon_sym_COMMA,
    ACTIONS(260), 1,
      anon_sym_RPAREN,
    STATE(40), 1,
      aux_sym_module_repeat2,
  [1090] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(212), 1,
      anon_sym_COMMA,
    ACTIONS(238), 1,
      anon_sym_RPAREN,
    STATE(71), 1,
      aux_sym_module_export_repeat2,
  [1103] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(212), 1,
      anon_sym_COMMA,
    ACTIONS(262), 1,
      anon_sym_RPAREN,
    STATE(71), 1,
      aux_sym_module_export_repeat2,
  [1116] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(173), 1,
      anon_sym_COMMA,
    ACTIONS(260), 1,
      anon_sym_RPAREN,
    STATE(43), 1,
      aux_sym_module_repeat2,
  [1129] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(196), 1,
      anon_sym_COMMA,
    ACTIONS(262), 1,
      anon_sym_RPAREN,
    STATE(59), 1,
      aux_sym_module_export_repeat1,
  [1142] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(264), 3,
      ts_builtin_sym_end,
      anon_sym_use,
      anon_sym_data,
  [1151] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(208), 1,
      anon_sym_COMMA,
    ACTIONS(266), 1,
      anon_sym_RBRACE,
    STATE(73), 1,
      aux_sym_use_piece_repeat1,
  [1164] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(268), 3,
      anon_sym_COMMA,
      anon_sym_SEMI_SEMI,
      anon_sym_RBRACE,
  [1173] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(270), 2,
      anon_sym_RPAREN,
      anon_sym_COMMA,
  [1181] = 3,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(262), 1,
      anon_sym_RPAREN,
    ACTIONS(272), 1,
      sym_module_id,
  [1191] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(274), 2,
      anon_sym_RPAREN,
      anon_sym_COMMA,
  [1199] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(249), 2,
      anon_sym_RPAREN,
      anon_sym_COMMA,
  [1207] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(276), 2,
      sym_operator_id,
      sym_id,
  [1215] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(278), 2,
      sym_operator_id,
      sym_id,
  [1223] = 3,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(155), 1,
      anon_sym_SEMI_SEMI,
    ACTIONS(280), 1,
      anon_sym_exclude,
  [1233] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(243), 2,
      anon_sym_COMMA,
      anon_sym_RBRACE,
  [1241] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(282), 2,
      sym_operator_id,
      sym_id,
  [1249] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(233), 2,
      anon_sym_RPAREN,
      anon_sym_COMMA,
  [1257] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(284), 2,
      anon_sym_RPAREN,
      anon_sym_COMMA,
  [1265] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(286), 2,
      anon_sym_RPAREN,
      anon_sym_COMMA,
  [1273] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(228), 2,
      anon_sym_RPAREN,
      anon_sym_COMMA,
  [1281] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(220), 2,
      anon_sym_RPAREN,
      anon_sym_COMMA,
  [1289] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(288), 2,
      anon_sym_RPAREN,
      anon_sym_COMMA,
  [1297] = 3,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(290), 1,
      anon_sym_RPAREN,
    ACTIONS(292), 1,
      sym_module_id,
  [1307] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(294), 2,
      anon_sym_RPAREN,
      anon_sym_COMMA,
  [1315] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(179), 2,
      anon_sym_RPAREN,
      anon_sym_COMMA,
  [1323] = 3,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(296), 1,
      anon_sym_exclude,
    ACTIONS(298), 1,
      anon_sym_SEMI_SEMI,
  [1333] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(300), 2,
      anon_sym_RPAREN,
      anon_sym_COMMA,
  [1341] = 3,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(63), 1,
      sym_module_path,
    STATE(143), 1,
      sym_use_piece,
  [1351] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(302), 1,
      anon_sym_LPAREN,
  [1358] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(304), 1,
      sym_qualified_id,
  [1365] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(306), 1,
      anon_sym_RPAREN,
  [1372] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(308), 1,
      anon_sym_LPAREN,
  [1379] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(310), 1,
      anon_sym_SEMI_SEMI,
  [1386] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(312), 1,
      sym_module_path,
  [1393] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(314), 1,
      anon_sym_RPAREN,
  [1400] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(316), 1,
      anon_sym_SEMI_SEMI,
  [1407] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(318), 1,
      sym_qualified_id,
  [1414] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(320), 1,
      sym_module_path,
  [1421] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(322), 1,
      anon_sym_SEMI_SEMI,
  [1428] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(324), 1,
      anon_sym_LPAREN,
  [1435] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(326), 1,
      anon_sym_RPAREN,
  [1442] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(328), 1,
      anon_sym_RPAREN,
  [1449] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(330), 1,
      anon_sym_LPAREN,
  [1456] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(332), 1,
      sym_module_id,
  [1463] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(334), 1,
      anon_sym_LPAREN,
  [1470] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(336), 1,
      anon_sym_SEMI_SEMI,
  [1477] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(338), 1,
      anon_sym_SEMI_SEMI,
  [1484] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(340), 1,
      anon_sym_LPAREN,
  [1491] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(342), 1,
      anon_sym_RPAREN,
  [1498] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(344), 1,
      anon_sym_SEMI_SEMI,
  [1505] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(346), 1,
      sym_qualified_id,
  [1512] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(348), 1,
      anon_sym_RPAREN,
  [1519] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(350), 1,
      ts_builtin_sym_end,
  [1526] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(352), 1,
      sym_module_path,
  [1533] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(354), 1,
      anon_sym_LPAREN,
  [1540] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(356), 1,
      anon_sym_LPAREN,
  [1547] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(358), 1,
      anon_sym_SEMI_SEMI,
  [1554] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(360), 1,
      anon_sym_SEMI_SEMI,
  [1561] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(362), 1,
      anon_sym_LPAREN,
  [1568] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(364), 1,
      anon_sym_SEMI_SEMI,
  [1575] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(366), 1,
      sym_id,
  [1582] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(368), 1,
      sym_module_path,
  [1589] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(370), 1,
      anon_sym_LPAREN,
  [1596] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(372), 1,
      anon_sym_SEMI_SEMI,
  [1603] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(298), 1,
      anon_sym_SEMI_SEMI,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 22,
  [SMALL_STATE(4)] = 44,
  [SMALL_STATE(5)] = 66,
  [SMALL_STATE(6)] = 88,
  [SMALL_STATE(7)] = 106,
  [SMALL_STATE(8)] = 128,
  [SMALL_STATE(9)] = 146,
  [SMALL_STATE(10)] = 168,
  [SMALL_STATE(11)] = 183,
  [SMALL_STATE(12)] = 202,
  [SMALL_STATE(13)] = 217,
  [SMALL_STATE(14)] = 232,
  [SMALL_STATE(15)] = 247,
  [SMALL_STATE(16)] = 266,
  [SMALL_STATE(17)] = 281,
  [SMALL_STATE(18)] = 297,
  [SMALL_STATE(19)] = 313,
  [SMALL_STATE(20)] = 325,
  [SMALL_STATE(21)] = 341,
  [SMALL_STATE(22)] = 357,
  [SMALL_STATE(23)] = 373,
  [SMALL_STATE(24)] = 389,
  [SMALL_STATE(25)] = 401,
  [SMALL_STATE(26)] = 413,
  [SMALL_STATE(27)] = 429,
  [SMALL_STATE(28)] = 445,
  [SMALL_STATE(29)] = 457,
  [SMALL_STATE(30)] = 473,
  [SMALL_STATE(31)] = 487,
  [SMALL_STATE(32)] = 500,
  [SMALL_STATE(33)] = 509,
  [SMALL_STATE(34)] = 522,
  [SMALL_STATE(35)] = 531,
  [SMALL_STATE(36)] = 540,
  [SMALL_STATE(37)] = 549,
  [SMALL_STATE(38)] = 562,
  [SMALL_STATE(39)] = 571,
  [SMALL_STATE(40)] = 584,
  [SMALL_STATE(41)] = 597,
  [SMALL_STATE(42)] = 606,
  [SMALL_STATE(43)] = 615,
  [SMALL_STATE(44)] = 628,
  [SMALL_STATE(45)] = 641,
  [SMALL_STATE(46)] = 650,
  [SMALL_STATE(47)] = 659,
  [SMALL_STATE(48)] = 672,
  [SMALL_STATE(49)] = 683,
  [SMALL_STATE(50)] = 692,
  [SMALL_STATE(51)] = 705,
  [SMALL_STATE(52)] = 718,
  [SMALL_STATE(53)] = 727,
  [SMALL_STATE(54)] = 740,
  [SMALL_STATE(55)] = 753,
  [SMALL_STATE(56)] = 762,
  [SMALL_STATE(57)] = 771,
  [SMALL_STATE(58)] = 784,
  [SMALL_STATE(59)] = 793,
  [SMALL_STATE(60)] = 806,
  [SMALL_STATE(61)] = 819,
  [SMALL_STATE(62)] = 828,
  [SMALL_STATE(63)] = 841,
  [SMALL_STATE(64)] = 852,
  [SMALL_STATE(65)] = 861,
  [SMALL_STATE(66)] = 870,
  [SMALL_STATE(67)] = 881,
  [SMALL_STATE(68)] = 894,
  [SMALL_STATE(69)] = 907,
  [SMALL_STATE(70)] = 920,
  [SMALL_STATE(71)] = 933,
  [SMALL_STATE(72)] = 946,
  [SMALL_STATE(73)] = 959,
  [SMALL_STATE(74)] = 972,
  [SMALL_STATE(75)] = 985,
  [SMALL_STATE(76)] = 998,
  [SMALL_STATE(77)] = 1007,
  [SMALL_STATE(78)] = 1020,
  [SMALL_STATE(79)] = 1029,
  [SMALL_STATE(80)] = 1038,
  [SMALL_STATE(81)] = 1051,
  [SMALL_STATE(82)] = 1064,
  [SMALL_STATE(83)] = 1077,
  [SMALL_STATE(84)] = 1090,
  [SMALL_STATE(85)] = 1103,
  [SMALL_STATE(86)] = 1116,
  [SMALL_STATE(87)] = 1129,
  [SMALL_STATE(88)] = 1142,
  [SMALL_STATE(89)] = 1151,
  [SMALL_STATE(90)] = 1164,
  [SMALL_STATE(91)] = 1173,
  [SMALL_STATE(92)] = 1181,
  [SMALL_STATE(93)] = 1191,
  [SMALL_STATE(94)] = 1199,
  [SMALL_STATE(95)] = 1207,
  [SMALL_STATE(96)] = 1215,
  [SMALL_STATE(97)] = 1223,
  [SMALL_STATE(98)] = 1233,
  [SMALL_STATE(99)] = 1241,
  [SMALL_STATE(100)] = 1249,
  [SMALL_STATE(101)] = 1257,
  [SMALL_STATE(102)] = 1265,
  [SMALL_STATE(103)] = 1273,
  [SMALL_STATE(104)] = 1281,
  [SMALL_STATE(105)] = 1289,
  [SMALL_STATE(106)] = 1297,
  [SMALL_STATE(107)] = 1307,
  [SMALL_STATE(108)] = 1315,
  [SMALL_STATE(109)] = 1323,
  [SMALL_STATE(110)] = 1333,
  [SMALL_STATE(111)] = 1341,
  [SMALL_STATE(112)] = 1351,
  [SMALL_STATE(113)] = 1358,
  [SMALL_STATE(114)] = 1365,
  [SMALL_STATE(115)] = 1372,
  [SMALL_STATE(116)] = 1379,
  [SMALL_STATE(117)] = 1386,
  [SMALL_STATE(118)] = 1393,
  [SMALL_STATE(119)] = 1400,
  [SMALL_STATE(120)] = 1407,
  [SMALL_STATE(121)] = 1414,
  [SMALL_STATE(122)] = 1421,
  [SMALL_STATE(123)] = 1428,
  [SMALL_STATE(124)] = 1435,
  [SMALL_STATE(125)] = 1442,
  [SMALL_STATE(126)] = 1449,
  [SMALL_STATE(127)] = 1456,
  [SMALL_STATE(128)] = 1463,
  [SMALL_STATE(129)] = 1470,
  [SMALL_STATE(130)] = 1477,
  [SMALL_STATE(131)] = 1484,
  [SMALL_STATE(132)] = 1491,
  [SMALL_STATE(133)] = 1498,
  [SMALL_STATE(134)] = 1505,
  [SMALL_STATE(135)] = 1512,
  [SMALL_STATE(136)] = 1519,
  [SMALL_STATE(137)] = 1526,
  [SMALL_STATE(138)] = 1533,
  [SMALL_STATE(139)] = 1540,
  [SMALL_STATE(140)] = 1547,
  [SMALL_STATE(141)] = 1554,
  [SMALL_STATE(142)] = 1561,
  [SMALL_STATE(143)] = 1568,
  [SMALL_STATE(144)] = 1575,
  [SMALL_STATE(145)] = 1582,
  [SMALL_STATE(146)] = 1589,
  [SMALL_STATE(147)] = 1596,
  [SMALL_STATE(148)] = 1603,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, SHIFT_EXTRA(),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [7] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0),
  [9] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(11),
  [12] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(129),
  [15] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1, 0, 0),
  [17] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [19] = {.entry = {.count = 1, .reusable = true}}, SHIFT(129),
  [21] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [23] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_use_piece, 1, 0, 1),
  [25] = {.entry = {.count = 1, .reusable = true}}, SHIFT(144),
  [27] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [29] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [31] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 2, 0, 0),
  [33] = {.entry = {.count = 1, .reusable = true}}, SHIFT(133),
  [35] = {.entry = {.count = 1, .reusable = true}}, SHIFT(70),
  [37] = {.entry = {.count = 1, .reusable = true}}, SHIFT(102),
  [39] = {.entry = {.count = 1, .reusable = false}}, SHIFT(37),
  [41] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [43] = {.entry = {.count = 1, .reusable = true}}, SHIFT(140),
  [45] = {.entry = {.count = 1, .reusable = false}}, SHIFT(53),
  [47] = {.entry = {.count = 1, .reusable = true}}, SHIFT(53),
  [49] = {.entry = {.count = 1, .reusable = true}}, SHIFT(119),
  [51] = {.entry = {.count = 1, .reusable = true}}, SHIFT(83),
  [53] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [55] = {.entry = {.count = 1, .reusable = true}}, SHIFT(148),
  [57] = {.entry = {.count = 1, .reusable = false}}, SHIFT(39),
  [59] = {.entry = {.count = 1, .reusable = true}}, SHIFT(39),
  [61] = {.entry = {.count = 1, .reusable = true}}, SHIFT(108),
  [63] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [65] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [67] = {.entry = {.count = 1, .reusable = true}}, SHIFT(93),
  [69] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [71] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_use_piece, 3, 0, 1),
  [73] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [75] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [77] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_use_piece, 2, 0, 1),
  [79] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [81] = {.entry = {.count = 1, .reusable = true}}, SHIFT(66),
  [83] = {.entry = {.count = 1, .reusable = false}}, SHIFT(103),
  [85] = {.entry = {.count = 1, .reusable = true}}, SHIFT(103),
  [87] = {.entry = {.count = 1, .reusable = true}}, SHIFT(101),
  [89] = {.entry = {.count = 1, .reusable = false}}, SHIFT_EXTRA(),
  [91] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_string_repeat1, 2, 0, 0),
  [93] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_string_repeat1, 2, 0, 0), SHIFT_REPEAT(17),
  [96] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_string_repeat1, 2, 0, 0), SHIFT_REPEAT(17),
  [99] = {.entry = {.count = 1, .reusable = true}}, SHIFT(78),
  [101] = {.entry = {.count = 1, .reusable = true}}, SHIFT(75),
  [103] = {.entry = {.count = 1, .reusable = true}}, SHIFT(115),
  [105] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_use_piece, 4, 0, 4),
  [107] = {.entry = {.count = 1, .reusable = true}}, SHIFT(54),
  [109] = {.entry = {.count = 1, .reusable = false}}, SHIFT(145),
  [111] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [113] = {.entry = {.count = 1, .reusable = false}}, SHIFT(17),
  [115] = {.entry = {.count = 1, .reusable = true}}, SHIFT(131),
  [117] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [119] = {.entry = {.count = 1, .reusable = false}}, SHIFT(137),
  [121] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [123] = {.entry = {.count = 1, .reusable = false}}, SHIFT(21),
  [125] = {.entry = {.count = 1, .reusable = true}}, SHIFT(138),
  [127] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_use_piece, 2, 0, 2),
  [129] = {.entry = {.count = 1, .reusable = true}}, SHIFT(56),
  [131] = {.entry = {.count = 1, .reusable = true}}, SHIFT(57),
  [133] = {.entry = {.count = 1, .reusable = true}}, SHIFT(142),
  [135] = {.entry = {.count = 1, .reusable = true}}, SHIFT(128),
  [137] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_use_piece, 3, 0, 3),
  [139] = {.entry = {.count = 1, .reusable = true}}, SHIFT(79),
  [141] = {.entry = {.count = 1, .reusable = true}}, SHIFT(80),
  [143] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [145] = {.entry = {.count = 1, .reusable = true}}, SHIFT(63),
  [147] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_export, 2, 0, 0),
  [149] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module, 3, 0, 0),
  [151] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [153] = {.entry = {.count = 1, .reusable = true}}, SHIFT(109),
  [155] = {.entry = {.count = 1, .reusable = true}}, SHIFT(41),
  [157] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_use_piece, 6, 0, 5),
  [159] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module, 12, 0, 0),
  [161] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_data, 2, 0, 0),
  [163] = {.entry = {.count = 1, .reusable = true}}, SHIFT(106),
  [165] = {.entry = {.count = 1, .reusable = true}}, SHIFT(48),
  [167] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [169] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module, 11, 0, 0),
  [171] = {.entry = {.count = 1, .reusable = true}}, SHIFT(130),
  [173] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [175] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module, 4, 0, 0),
  [177] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_use_piece, 8, 0, 7),
  [179] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_module_repeat2, 2, 0, 0),
  [181] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_module_repeat2, 2, 0, 0), SHIFT_REPEAT(10),
  [184] = {.entry = {.count = 1, .reusable = true}}, SHIFT(147),
  [186] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module, 8, 0, 0),
  [188] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_use, 3, 0, 0),
  [190] = {.entry = {.count = 1, .reusable = true}}, SHIFT(112),
  [192] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module, 10, 0, 0),
  [194] = {.entry = {.count = 1, .reusable = true}}, SHIFT(110),
  [196] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [198] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module, 5, 0, 0),
  [200] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [202] = {.entry = {.count = 1, .reusable = true}}, SHIFT(97),
  [204] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [206] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_use_piece, 7, 0, 6),
  [208] = {.entry = {.count = 1, .reusable = true}}, SHIFT(81),
  [210] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_use, 4, 0, 0),
  [212] = {.entry = {.count = 1, .reusable = true}}, SHIFT(127),
  [214] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_use_piece, 7, 0, 1),
  [216] = {.entry = {.count = 1, .reusable = true}}, SHIFT(141),
  [218] = {.entry = {.count = 1, .reusable = true}}, SHIFT(126),
  [220] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_export, 3, 0, 0),
  [222] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module, 9, 0, 0),
  [224] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module, 6, 0, 0),
  [226] = {.entry = {.count = 1, .reusable = true}}, SHIFT(123),
  [228] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_module_repeat1, 2, 0, 0),
  [230] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_module_repeat1, 2, 0, 0), SHIFT_REPEAT(15),
  [233] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_module_export_repeat2, 2, 0, 0),
  [235] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_module_export_repeat2, 2, 0, 0), SHIFT_REPEAT(127),
  [238] = {.entry = {.count = 1, .reusable = true}}, SHIFT(105),
  [240] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_use_piece_repeat1, 2, 0, 0), SHIFT_REPEAT(81),
  [243] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_use_piece_repeat1, 2, 0, 0),
  [245] = {.entry = {.count = 1, .reusable = true}}, SHIFT(90),
  [247] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_declaration, 1, 0, 0),
  [249] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_module_export_repeat1, 2, 0, 0),
  [251] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_module_export_repeat1, 2, 0, 0), SHIFT_REPEAT(25),
  [254] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_use_piece, 5, 0, 1),
  [256] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_use_piece, 4, 0, 1),
  [258] = {.entry = {.count = 1, .reusable = true}}, SHIFT(98),
  [260] = {.entry = {.count = 1, .reusable = true}}, SHIFT(116),
  [262] = {.entry = {.count = 1, .reusable = true}}, SHIFT(91),
  [264] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module, 7, 0, 0),
  [266] = {.entry = {.count = 1, .reusable = true}}, SHIFT(61),
  [268] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_use_piece, 6, 0, 1),
  [270] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_export, 5, 0, 0),
  [272] = {.entry = {.count = 1, .reusable = true}}, SHIFT(67),
  [274] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_export, 4, 0, 0),
  [276] = {.entry = {.count = 1, .reusable = true}}, SHIFT(114),
  [278] = {.entry = {.count = 1, .reusable = true}}, SHIFT(124),
  [280] = {.entry = {.count = 1, .reusable = true}}, SHIFT(146),
  [282] = {.entry = {.count = 1, .reusable = true}}, SHIFT(135),
  [284] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_export, 6, 0, 0),
  [286] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_identifier, 1, 0, 0),
  [288] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_export, 7, 0, 0),
  [290] = {.entry = {.count = 1, .reusable = true}}, SHIFT(104),
  [292] = {.entry = {.count = 1, .reusable = true}}, SHIFT(60),
  [294] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_module_repeat1, 6, 0, 0),
  [296] = {.entry = {.count = 1, .reusable = true}}, SHIFT(139),
  [298] = {.entry = {.count = 1, .reusable = true}}, SHIFT(52),
  [300] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_export, 8, 0, 0),
  [302] = {.entry = {.count = 1, .reusable = true}}, SHIFT(92),
  [304] = {.entry = {.count = 1, .reusable = true}}, SHIFT(132),
  [306] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [308] = {.entry = {.count = 1, .reusable = true}}, SHIFT(99),
  [310] = {.entry = {.count = 1, .reusable = true}}, SHIFT(64),
  [312] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [314] = {.entry = {.count = 1, .reusable = true}}, SHIFT(62),
  [316] = {.entry = {.count = 1, .reusable = true}}, SHIFT(45),
  [318] = {.entry = {.count = 1, .reusable = true}}, SHIFT(118),
  [320] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_attr, 1, 0, 0),
  [322] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
  [324] = {.entry = {.count = 1, .reusable = true}}, SHIFT(113),
  [326] = {.entry = {.count = 1, .reusable = true}}, SHIFT(55),
  [328] = {.entry = {.count = 1, .reusable = true}}, SHIFT(47),
  [330] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [332] = {.entry = {.count = 1, .reusable = true}}, SHIFT(100),
  [334] = {.entry = {.count = 1, .reusable = true}}, SHIFT(96),
  [336] = {.entry = {.count = 1, .reusable = true}}, SHIFT(36),
  [338] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [340] = {.entry = {.count = 1, .reusable = true}}, SHIFT(120),
  [342] = {.entry = {.count = 1, .reusable = true}}, SHIFT(107),
  [344] = {.entry = {.count = 1, .reusable = true}}, SHIFT(88),
  [346] = {.entry = {.count = 1, .reusable = true}}, SHIFT(125),
  [348] = {.entry = {.count = 1, .reusable = true}}, SHIFT(42),
  [350] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [352] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 2, 0, 0),
  [354] = {.entry = {.count = 1, .reusable = true}}, SHIFT(95),
  [356] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [358] = {.entry = {.count = 1, .reusable = true}}, SHIFT(65),
  [360] = {.entry = {.count = 1, .reusable = true}}, SHIFT(38),
  [362] = {.entry = {.count = 1, .reusable = true}}, SHIFT(134),
  [364] = {.entry = {.count = 1, .reusable = true}}, SHIFT(58),
  [366] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [368] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_string, 3, 0, 0),
  [370] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [372] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
};

#ifdef __cplusplus
extern "C" {
#endif
#ifdef TREE_SITTER_HIDE_SYMBOLS
#define TS_PUBLIC
#elif defined(_WIN32)
#define TS_PUBLIC __declspec(dllexport)
#else
#define TS_PUBLIC __attribute__((visibility("default")))
#endif

TS_PUBLIC const TSLanguage *tree_sitter_yolang(void) {
  static const TSLanguage language = {
    .version = LANGUAGE_VERSION,
    .symbol_count = SYMBOL_COUNT,
    .alias_count = ALIAS_COUNT,
    .token_count = TOKEN_COUNT,
    .external_token_count = EXTERNAL_TOKEN_COUNT,
    .state_count = STATE_COUNT,
    .large_state_count = LARGE_STATE_COUNT,
    .production_id_count = PRODUCTION_ID_COUNT,
    .field_count = FIELD_COUNT,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .parse_table = &ts_parse_table[0][0],
    .small_parse_table = ts_small_parse_table,
    .small_parse_table_map = ts_small_parse_table_map,
    .parse_actions = ts_parse_actions,
    .symbol_names = ts_symbol_names,
    .symbol_metadata = ts_symbol_metadata,
    .public_symbol_map = ts_symbol_map,
    .alias_map = ts_non_terminal_alias_map,
    .alias_sequences = &ts_alias_sequences[0][0],
    .lex_modes = ts_lex_modes,
    .lex_fn = ts_lex,
    .primary_state_ids = ts_primary_state_ids,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
