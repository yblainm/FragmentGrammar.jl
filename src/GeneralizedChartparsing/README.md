# GeneralizedChartparsing

## Demos

### Parsing Jazz harmony sequences

For a first simple interaction with the parser try the following:

```julia
using GeneralizedChartparsing
grammar = make_jazz_grammar();
train!(grammar, make_parsable_trees());
sequence = split("C^7 Dm7 G7 C^7");
forest = run_chartparser(sequence, grammar);
best_tree(forest)
```

The output will be a representation of a parse tree in bracket notation.

```
[I_{C}F(1,5)[I_{C}F(1,2)[C^7]][I_{C}F(2,5)[V_{C}F(2,4)[V_{g}F(2,3)[Dm7]][V_{C}F(3,4)[G7]]][I_{C}F(4,5)[C^7]]]]
```

### Parsing with a simple English grammar

```julia
rules = """
       .1 Det _the
       .1 N _cooks
       .1 V _cook
       .1 NP Det N
       .1 VP V
       .1 S NP VP
       """
grammar = Grammar(rules, ["S"])
forest = run_chartparser(split("the cooks cook"), grammar)
best_tree(forest)
```

## Data types

### Grammar types

The file `src/grammars/Grammars.jl` contains the type declarations of Rules, States, and Grammars. They are parameterized by category types (generalized non-terminal symbols) and terminal types (generalized terminal symbols). Categories and terminals can be of any  type and should be immutable for performance reasons. The following figure provides an example of a simple context-free grammar where all  categories and terminals are strings.

![sample-grammar](sample-grammar.png)

**Plain context-free rules** contain a single category as left hand side and a tuple of categories (or terminals) as right hand side. 

```julia
immutable ContextFreeRule{C1,C2}
    lhs :: C1                # left hand side
    rhs :: Tuple{Vararg{C2}} # right hand side
end
```

**Meta-rules** are essentially sets of rules that together form a function. They are implemented consisting of a name and a vector of rules.

```julia
immutable MetaRule{R}
    name  :: String
    rules :: Vector{R}
end
```

**States** keep track of read prefixes of right hand sides of rules during the parsing process. They so to say collapse rules that share a common right hand side prefix and thus reduce the number of items in the agenda and the chart, and make the parsing efficient. States can be thought of as states of a finite-state automaton (or [search trie](https://en.wikipedia.org/wiki/Trie)) where reaching a final states corresponds to the automaton accepting a fully read right hand side and returning a tuple of the respective left hand side and the rule, called **(state) completion** (see the following figure). 

![search-trie](search-trie.png)

The state implementation consists of the possible completions, the transitions states, and a Boolean indicator if there is any further transition possible.

```julia
# C ... (non-terminal) category type
# CR ... type of rules that rewrite categories to
#        sequences of (non-terminal) categories
type State{C, CR} # category and rule
    # completions: tuples of categories and rules
    comp    :: Vector{Tuple{C, CR}}  
    # possible transitions
    trans   :: Dict{C, State{C, CR}} 
    isfinal :: Bool
end
```

For performance reasons, states are compared and hashed by their object ID. All states of a grammar are thus constructed exactly once.

**Grammars** are implemented consisting of a start state, a vector of possible start symbols (initial categories of the generative model), a hash table that maps terminals to their terminal completions (tuples of categories and terminal rules), and a distribution of rules conditioned on categories (e.g. a Dirichlet multinomial or simple urn model). A copy of the prior distribution is also included.

```julia
# S ... score type (in principle any semi-ring)
type Grammar{C, T, CR, TR, S, Cond}
    startstate    :: State{C, CR}
    startsymbols  :: Vector{C}
    terminal_dict :: Dict{T, Vector{Tuple{C, TR}}}
    rule_cond     :: Cond
    prior_cond    :: Cond # needed for GibbsSampling
end
```

### Rule conditionals

The probability model of a grammar is described in the **rule conditional**. That is a probability distribution of grammar rules conditioned on categories. In the process of randomly sampling a sequence from the grammar, the **unfold procedure**, first of all a start symbol category is drawn from the set of all start symbols. Then, for each non-terminal category a rewrite rule is sampled conditioned on this category until all leaves of the derived tree are terminals. Rule conditionals must implement the following simple interface. Additional functions are required for learning and grammar induction procedures. 

```julia
logscore(conditional, rule, category)
sample(conditional, category)
```

For example, a categorical distribution can be used if the number of rules is finite. A categorical distribution is a multinomial distribution where the number of outcomes is fixed to one. If one wants to learn the parameters of the categorical distribution in a Bayesian inference process, a [Dirichlet-multinomial](https://en.wikipedia.org/wiki/Dirichlet-multinomial_distribution) model might be the right choice — or more simply a [Pólya urn model](https://en.wikipedia.org/wiki/Pólya_urn_model) in the case of integer hyperparameters. In the case of infinitely many rewrite rules, a Chinese Restaurant Process might be suitable. 

### Grammar-parser interface

The parser interacts with the grammar using the following functions.

```julia
startstate(grammar)
startsymbols(grammar)
prob(grammar, category, rule) # prob of application
completions(grammar, state)
completions(grammar, terminal)
is_possible_transition(grammar, state, category)
transition(grammar, state, category)
isfinal(state) # only for improving performance
```

### The parser

The file `src/parser/parser_types.jl` contains the type declarations of traversals, completions, keys, edges, constituents, the parser logbook, the chart, the agenda, and the parse forest -- the return type of the parser. `src/parser/parser_types.jl ` contains the functions that implement the parser algorithm.

#### Edges and constituents

Analog to [Earley items](https://en.wikipedia.org/wiki/Earley_parser), we call the compound of a state together with a start and end index including additional values such as back pointers and probabilities an **edge** (a.k.a. active item) and the compound of a category together with a start and end index including some additional values **constituent** (a.k.a. passive item). The start indices and end indices define intervals that items span over the input string. These intervals might be generalized as **range** objects.

| grammar object (without range) | parser object (incl. range)   |
| ------------------------------ | ----------------------------- |
| state                          | edge                          |
| category                       | constituent                   |

A state or category together with a start and an end index are called **edge key** and **constituent key**, respectively. They uniquely identify items in the agenda and the chart.

```julia
# is not a bitstype (states are mutable)
@auto_hash_equals immutable EdgeKey{St}
    start :: Int
    tail  :: Int
    state :: St
end
make_accesses(EdgeKey) # create access functions

# is a bitstype
immutable ConsKey{C}
    start :: Int
    tail  :: Int
    cat   :: C
end
make_accesses(ConsKey)
```

To ensure type stability (important for great performance of Julia programs), each item is also indexed by an integer, called **item IDs**. **Edge IDs** are positive and **constituent IDs** negative. While item keys are used to *search* for an item if already existent or otherwise create a new item, item IDs are used to *store* an item type-independently.

#### Agenda and chart

In the initialization step, one constituent for each terminal symbol in the input string is enqueued in the agenda. The **agenda** is a priority queue containing item IDs of items of which the final semi-ring value (e.g. probability) is not known yet. 

```julia
type Agenda{T}
    pqueue :: PriorityQueue{Int, T, Base.Order.ForwardOrdering}
end
```

Items of small length are favored and edges are preferred over constituents to implement a bottom-up chart parsing. The **chart** on the other hand contains exactly the items of which the final semi-ring value is known. 

```julia
type ChartCell{C,St}
    edgeids :: Dict{St, Vector{Int}}
    consids :: Dict{C, Vector{Int}}
end
type Chart{C,St}
    cells :: Vector{ChartCell{C,St}}
end
```

Importantly, only the items in the chart are used to build properly larger items to ensure dynamic programming.

#### The main loop

While the agenda is not empty, the parser dequeues an item from it and applies one of the three inference rules.

1. `introduce_edge!` -- create or update an edge over a given constituent with the same range
2. `complete_edge!` -- create or update one or more constituents over a given edge with the same range
3. `do_fundamental_rule` -- combine an edge with a constituent to its right to an edge the spans the interval from the start of the edge to the end of the constituent if a transition of the edge's state given the constituent's category is possible

After applying inference rule one or two, we could be located in a unary loop. The parser calculates the score of that loop by iteratively summing up the respective geometric sum. The new constructed or updated item is therefore not inserted into the chart, but enqueued into the agenda if the geometric sum is not yet sufficiently exactly calculated. If otherwise the sum is sufficiently exactly calculated, the item can be inserted into the chart and the fundamental rule (inference rule 3) can be applied which result is enqueued into the agenda. 

The **parser logbook** is essentially a hash table that maps item keys or item IDs to their items. It is in particular used after an inference rule was applied to point to the inferred item if it has been created already.

```julia
type ParserLogbook{C,T,CR,TR,St,S}
    edges :: Vector{Edge{St,S}}
    conss :: Vector{Constituent{C,T,CR,TR,S}}
    edgeids :: Dict{EdgeKey{St}, Int}
    consids :: Dict{ConsKey{C}, Int}
end
```

#### Traversals and completions

**Traversals** and **completions** are the back pointers of edges and constituents, respectively. Every traversal describes exactly one way of building an edge and every completion describes exactly one way of building a constituent. Since there can be multiple ways to build edges and constituents, edges contain a list of traversals and constituents contain a list of completions from which they were created or updated. 

**Traversals** are implemented as a compound of an edge ID, a constituent ID, and a semi-ring score.

```julia
immutable Traversal{S}
    edgeid  :: Int
    consid  :: Int
    score   :: S
end
```

If an edge was created by the fundamental rule, the corresponding edge ID and constituent ID is stored in the traversal. The score is in this case the product of the score of the edge and the score of the constituent. If otherwise an edge was created using the  `introduce_edge!` rule, zero is stored as edge id to indicate that the involved edge would have contained the start state of the grammar. Here, the score equals the score of the constituent.

**Completions** come either as edge completion or terminal completion.

```julia
abstract Completion{S}
immutable EdgeCompletion{CR,S} <: Completion{S}
    edgeid :: Int
    rule   :: CR
    score  :: S
end
immutable TerminalCompletion{T,TR,S} <: Completion{S}
    terminal :: T
    rule     :: TR
    score    :: S
end
```

They consist of either an edge ID or a terminal symbol, a rule, and a score. A constituent can only be created by the `complete_edge!` inference rule. In the case of an edge completion, the score is the product of the score of the edge and the score of applying the grammar rule to the category of the inferred constituent. In the other case of a terminal completion, the score simply equals the score of applying the terminal rule to the category of the inferred constituent. Note that every constituent can contain at most one terminal completion, but zero or more edge completions.

#### Parse forests

**Parser forests** are a compact representation of the set of all possible parse trees given a grammar and an input sequence. It is the return type of the `run_chartparser` function and consists of a list of possible tree roots (a.k.a heads), the parser logbook that contains pointers to all items in the chart, and a list of the terminal symbols of the input sequence for convenience. 

```julia
type ParseForest{C,T,CR,TR,St,S}
    heads :: Vector{Constituent{C,T,CR,TR,S}}
    logbook :: ParserLogbook{C,T,CR,TR,St,S}
    terminals :: Vector{T}
end
```

The set of implemented functions for parse forests include

```julia
is_complete(forest)
score(forest)
best_tree(forest)
sample_tree(forest)
```





