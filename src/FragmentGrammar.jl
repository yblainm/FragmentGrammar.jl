__precompile__()
module FragmentGrammar

export Analysis, BaseDistribution, Fragment, Pointer, FragmentGrammar, sample, ChineseRest, add_obs!, rm_obs, iterate

import Base: iterate, eltype, length, IteratorSize

include("GeneralizedChartparsing\\src\\GeneralizedChartparsing.jl")
using .GeneralizedChartparsing

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
    variables :: Array{Tree,1}
end

struct Pointer
    fragment :: Fragment
    children :: Dict{Tree, Pointer}
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

struct Analysis # Is this struct even needed? It's basically a Tuple of what comes out of FG sample
    pointer :: Pointer
    dm_obs :: Array{Tuple{Int,Int},1}
    bb_obs :: Array{Pair{Tuple{Int,Int,Int},Bool},1}
end

get_idx(A::AbstractArray{T,1}, i::T) where T = (
    for (j,k) in enumerate(A)
        if i == k
            return j
        end
    end; error("element $i not found in $A")
)

################################
# Fragment Grammar definitions #
################################

struct BaseDistribution{C} <: Distribution{Fragment}
    fg :: FragmentGrammar{C}
    catidx :: Int
    category :: C
end

end
