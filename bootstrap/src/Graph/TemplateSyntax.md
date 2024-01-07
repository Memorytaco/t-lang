# Template Haskell for Graph Node Lanugage

This is the language definiton for creating Graph Nodes
and writing common logic relating to Graph Nodes.

## Basic Structures

We use Algebraic-Graph library as internal implementation
of graph and thus we now have two basic core structures for
our graph.

1. `Hole e a` in `Graph.Core`
  > Our internal representation of a type of node in graph. Using `a` as
  > common attributes and `e` as supporting for different formats of nodes.

2. `Link e` in `Graph.Core`
  > Our internal representation of edges in graph. Using `e` as holder
  > for different kinds of edge labels.


## Basic Syntaxs

```
node := node_constructor [ ":[" (attribute | (attribute (',' attribute)+)) ']' ]
        '(' node_parameter ("," node_parameter)* ')'
      | #Constructor:[attributes]

literal := integer | string | constant

integer := digit+
digit := '0'..'9'

node_constructor := '#' value_constructor
node_parameter 
```

We use `#Constructor` to represent constructor for a node format and use
`#Constructor :[attributes]` to help writing node formats. e.g.
`#Pht` or `#Sum (1)`

## WIP...