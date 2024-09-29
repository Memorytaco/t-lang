package tree_sitter_yolang_test

import (
	"testing"

	tree_sitter "github.com/smacker/go-tree-sitter"
	"github.com/tree-sitter/tree-sitter-yolang"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_yolang.Language())
	if language == nil {
		t.Errorf("Error loading Yolang grammar")
	}
}
