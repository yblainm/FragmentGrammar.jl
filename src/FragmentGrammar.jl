__precompile__()
module FragmentGrammars

export Analysis, BaseDistribution, Fragment, Pointer, FragmentGrammar, sample, ChineseRest, add_obs!, rm_obs, iterate

import Base: iterate, eltype, length, IteratorSize

include("GeneralizedChartparsing\\src\\GeneralizedChartparsing.jl")
using .GeneralizedChartparsing
using .GeneralizedChartparsing.Trees
using .GeneralizedChartparsing: ContextFreeRule, add_rule!

include("CompoundDists.jl");
using .CompoundDists
# import .CompoundDists: sample, add_obs!, rm_obs!

# TODO:
#   -Redefine FG with Vararg RHS base rules for indexing probs/etc.
#   -Redefine base measure with RHS base rules, idem.
#   -See below, what is needed to define these?
#   -Presumably a completion dict as a field of the FG
### Grammar-parser interface
```The parser interacts with the grammar using the following functions.

(SEE /src/GeneralizedChartparsing/src/grammars/grammar_interface.jl)

startstate(grammar)
startsymbols(grammar)
prob(grammar, category, rule) # prob of application
completions(grammar, state)
completions(grammar, terminal)
is_possible_transition(grammar, state, category)
transition(grammar, state, category)
isfinal(state) # only for improving performance

### The parser

The file 'src/parser/parser_types.jl' contains the type declarations of traversals, completions, keys, edges, constituents, the parser logbook, the chart, the agenda, and the parse forest -- the return type of the parser.
```

###########################
# Helper structs and functions #
###########################
struct Fragment
    tree :: Tree
    variables :: Vector{Tree}
end

struct Pointer
    fragment :: Fragment
    children :: Dict{Tree, Pointer}
end

struct Analysis # Is this struct even needed? It's basically a Tuple of what comes out of FG sample
    pointer :: Pointer
    dm_obs :: Vector{Tuple{Int,Int}}
    bb_obs :: Vector{Pair{Tuple{Int,Tuple{Vararg{Int}}},Bool}}
end

eltype(::Type{Pointer}) = Pointer
IteratorSize(::Type{Pointer}) = Base.SizeUnknown()
function iterate(frag_pointer::Pointer, state = [frag_pointer])
    if isempty(state)
        nothing
    else
        state[1], prepend!(state[2:end], collect(values(state[1].children)))
    end
end

get_idx(A::AbstractVector{T}, i::T) where T = (
    for (j,k) in enumerate(A)
        if i == k
            return j
        end
    end; error("element $i not found in $A")
)

# Basically copied from add_rule! in GeneralizedChartparsing/src/Grammars.jl
rm_rule!(state::State{C,CR}, rule, head, cats) where {C,CR} =
(
    s = state
    for c in cats
        if is_possible_transition(s, c)
            s = transition(s, c)
        else
            s.isfinal = false
            s = s.trans[c] = State(C, CR)
        end
    end
    filter!(x -> x≠(head, rule), s.comp, )
)

################################
# Fragment Grammar definitions #
################################

mutable struct FragmentGrammar{C, CR, T, TR, S, Sc}
    categories :: Vector{C}
    startcategories :: Vector{C}
    category_rules :: Vector{CR}
    terminals :: Vector{T}
    terminal_rules :: Vector{TR}
    startstate :: S
    terminal_dict :: Dict{T, Vector{C, TR}}
    CRP :: Vector{ChineseRest{Fragment}}
    DM :: Vector{DirCat{C, Float64}}
    BB :: Dict{Tuple{C, CR, C}, BetaBern{Bool, Int}}
end

function FragmentGrammar(cats::Vector{C}, starts::Vector{C}, cat_rules::Vector{CR}, terms:: Vector{T}, term_rules::Vector{TR}, ScoreType::DataType) where {C, CR, T, TR}

    startstate = State(C, CR)
    for r in cat_rules
        add_rule!(startstate, r, lhs(r), rhs(r))
    end

    CRP = ChineseRest{Fragment}[]

    DM = DirCat{C, Float64}[DirCat([r for (i, r) in enumerate(cat_rules) if isapplicable(r, cat)]) for (j, cat) in enumerate(cats)],

    BB = Dict{Tuple{C, CR, C}, BetaBern{Bool, Int}}((r.lhs, r, rhs) => BetaBern(1, 1) for r in cat_rules for rhs in r.rhs)

    FragmentGrammar(
end

struct BaseDistribution{C} <: Distribution{Fragment}
    fg :: FragmentGrammar{C}
    catidx :: Int
    category :: C
end

end
