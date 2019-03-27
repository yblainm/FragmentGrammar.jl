__precompile__()
module FragmentGrammars

export Analysis, BaseDistribution, Fragment, Pointer, FragmentGrammar
export sample, add_obs!, rm_obs!, iterate
export ContextFreeRule

import Base: iterate, eltype, length, IteratorSize

include("GeneralizedChartparsing\\src\\GeneralizedChartparsing.jl")
using .GeneralizedChartparsing
using .GeneralizedChartparsing.Trees
using .GeneralizedChartparsing: ContextFreeRule, lhs, rhs, add_rule!

include("CompoundDists.jl");
using .CompoundDists
import .CompoundDists: sample, add_obs!, rm_obs!

# TODO:
#   -Redefine FG with Vararg RHS base rules for indexing probs/etc.
#   -Redefine base measure with RHS base rules, idem.
#   -See below, what is needed to define these?
#   -Presumably a completion dict as a field of the FG
### Grammar-parser interface
"""The parser interacts with the grammar using the following functions.

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

The file 'src/parser/parser_types.jl' contains the type declarations of traversals, completions, keys, edges, constituents, the parser logbook, the chart, the agenda, and the parse forest -- the return type of the parser."""


###########################
# Helper structs and functions #
###########################
struct Fragment
    tree :: Tree
    variables :: Vector{Tree}
    leaves :: Vector{Tree}
end

struct Pointer
    fragment :: Fragment
    children :: Dict{Tree, Pointer}
end

struct Analysis{C, CR} # Is this struct even needed? It's basically a Tuple of what comes out of FG sample
    pointer :: Pointer
    dm_obs :: Vector{Tuple{C,CR}}
    bb_obs :: Vector{Pair{Tuple{C, CR, C},Bool}}
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
function rm_rule!(state::State{C,CR}, rule, head, cats) where {C,CR}
    s = state
    for c in cats
        if is_possible_transition(s, c)
            s = transition(s, c)
        else
            s.isfinal = false
            s = s.trans[c] = State(C, CR)
        end
    end
    filter!(x -> x≠(head, rule), s.comp)
end

################################
# Fragment Grammar definitions #
################################
"""
    FragmentGrammar(categories, startcategories, category_rules, terminals, terminal_rules, startstate, terminal_dict, CRP, DM, BB)
"""
mutable struct FragmentGrammar{C, CR, T, TR}
    categories :: Vector{C}
    startcategories :: Vector{C}
    category_rules :: Vector{CR}
    terminals :: Vector{T}
    terminal_rules :: Vector{TR}
    preterminals :: Vector{C}
    startstate :: State{C, CR}
    terminal_dict :: Dict{T, Vector{Tuple{C, TR}}}
    CRP :: Dict{C,ChineseRest{Fragment}}
    DM :: Dict{C, DirCat{CR, Float64}}
    BB :: Dict{Tuple{C, CR, C}, BetaBern{Bool, Int}}
end

category_type(fg::FragmentGrammar{C, CR, T, TR}) where {C, CR, T, TR} = C
terminal_type(fg::FragmentGrammar{C, CR, T, TR}) where {C, CR, T, TR} = T
category_rule_type(fg::FragmentGrammar{C, CR, T, TR}) where {C, CR, T, TR} = CR
terminal_rule_type(fg::FragmentGrammar{C, CR, T, TR}) where {C, CR, T, TR} = TR
categories(fg::FragmentGrammar) = fg.categories
terminals(fg::FragmentGrammar) = fg.terminals
preterminals(fg::FragmentGrammar) = fg.preterminals
startstate(fg::FragmentGrammar) = fg.startstate
startsymbols(fg::FragmentGrammar) = fg.startsymbols
score_type(fg::FragmentGrammar) = LogProb
state_type(fg::FragmentGrammar) = typeof(startstate(fg))

"""
    FragmentGrammar(categories, startcategories, category_rules, terminals, terminal_rules[, a::Float64, b::Float64])
"""
function FragmentGrammar(cats::Vector{C}, starts::Vector{C}, cat_rules::Vector{CR}, terms:: Vector{T}, term_rules::Vector{TR}, a=0.01, b=0.2) where {C, CR, T, TR}

    startstate = State(C, CR)
    for r in cat_rules
        add_rule!(startstate, r, r.lhs, r.rhs)
    end

    CRP = Dict{C,ChineseRest{Fragment}}()
    DM = Dict{C, DirCat{CR, Float64}}(cat => DirCat([r for (i, r) in enumerate(cat_rules) if r.lhs == cat]) for (j, cat) in enumerate(cats))
    BB = Dict{Tuple{C, CR, C}, BetaBern{Bool, Int}}((r.lhs, r, rhs) => BetaBern(1, 1) for r in cat_rules for rhs in r.rhs)
    preterminals = C[]
    terminal_dict = Dict{T, Vector{Tuple{C, TR}}}()
    for r in term_rules
        push!(preterminals, r.lhs)
        t = r.rhs[1]
        if haskey(terminal_dict, t)
            push!(terminal_dict[t], (r.lhs, r))
        else
            terminal_dict[t] = [(r.lhs, r)]
        end
    end

    fg = FragmentGrammar{C, CR, T, TR}(cats, starts, cat_rules, terms, term_rules, preterminals, startstate, terminal_dict, CRP, DM, BB)

    fg.CRP = Dict{C,ChineseRest{Fragment}}(cat => ChineseRest(a, b, BaseDistribution(fg, cat)) for (i, cat) in enumerate(cats))

    return fg
end

function sample(fg :: FragmentGrammar, cat :: C) where C
    crp_sample = sample(fg.CRP[cat])
    fragment, dm_counts, bb_counts = crp_sample # typeof(crp_sample) <: Tuple ? crp_sample : (crp_sample, [], [])
    children = Dict{Tree, Pointer}()
    for variable in fragment.variables
        ptr, dm_counts_child, bb_counts_child = sample(fg, variable.data)
        append!(dm_counts, dm_counts_child)
        append!(bb_counts, bb_counts_child)
        push!(children, variable => ptr)
    end
    return Pointer(fragment, children), dm_counts, bb_counts
end

struct BaseDistribution{C} <: Distribution{Fragment}
    fg :: FragmentGrammar{C}
    category :: C
end

function sample(basedist :: BaseDistribution)
    C, CR, T, TR = category_type(basedist.fg), category_rule_type(basedist.fg), terminal_type(basedist.fg), terminal_rule_type(basedist.fg)

    variables = Tree{C}[]
    leaves = Tree{C}[]
    tree = TreeNode(basedist.category)

    dm_sample = sample(basedist.fg.DM[basedist.category])
    dm_counts = Tuple{C,CR}[]
    push!(dm_counts, (basedist.category, dm_sample))
    bb_counts = Pair{Tuple{C, CR, C}, Bool}[]
    r = dm_sample

    children = r(basedist.category) # or r.rhs or rhs(r)
    Ty = typeof(children)
    isterm = length(children) === 1 && children[1] in terminals(basedist.fg)

    for child in children
        if child in basedist.fg.preterminals
            # Make a leaf node (non-variable)
            leaf_tree = TreeNode(child)
            push!(leaves, leaf_tree)
        else
            bbidx = (basedist.category, dm_sample, child)
            flip = sample(basedist.fg.BB[bbidx])
            push!(bb_counts, bbidx => flip)
            if flip # if we extend the fragment
                # Get (recursive) Pointer, DM counts, and BB counts
                ptr, dm_counts_child, bb_counts_child = sample(basedist.fg, child)
                frag = ptr.fragment # We only need the fragment for the base distribution
                # Merge BB counts
                append!(bb_counts, bb_counts_child)
                # Merge DM counts
                append!(dm_counts, dm_counts_child)
                insert_child!(tree, deepcopy(frag.tree))   # Better than modifying the underlying tree if taken from CRP preexisting fragment
                # Merge variables (non-terminal leaves) from recursive calls to FG
                append!(variables, frag.variables)
            else
                # Make a leaf node (variable)
                variable_tree = TreeNode(child)
                push!(variables, variable_tree)
                push!(leaves, variable_tree)
                insert_child!(tree, variable_tree)
            end
        end
    end

    return Fragment(tree, variables, leaves), dm_counts, bb_counts
end


# TODO: Write tests for add/rm_obs!
function add_obs!(fg :: FragmentGrammar, analysis :: Analysis)
    # Add DM counts
    for dm_obs in analysis.dm_obs
        add_obs!(fg.DM[dm_obs[1]], dm_obs[2])
    end
    # Add BB counts
    for bb_obs in analysis.bb_obs
        add_obs!(fg.BB[bb_obs[1]], bb_obs[2])
    end
    # Add fragments to CRP
    for frag_ptr in analysis.pointer
        add_obs!(fg.CRP[frag_ptr.fragment.tree.data], frag_ptr.fragment)
    end
    # Add completion to finite state machine (for approx. PCFG)
    root = analysis.pointer.fragment.tree.data
    leaves = getfield.(analysis.pointer.fragment.leaves, :data)
    add_rule!(startstate(fg), ContextFreeRule(root, (leaves...,)), root, leaves)
end

function rm_obs!(fg :: FragmentGrammar, analysis :: Analysis)
    # rm DM counts
    for dm_obs in analysis.dm_obs
        rm_obs!(fg.DM[dm_obs[1]], dm_obs[2])
    end
    # rm BB counts
    for bb_obs in analysis.bb_obs
        rm_obs!(fg.BB[bb_obs[1]], bb_obs[2])
    end
    # rm fragments from CRP
    for frag_ptr in analysis.pointer
        rm_obs!(fg.CRP[frag_ptr.fragment.tree.data], frag_ptr.fragment)
    end
    # rm completion from finite state machine (for approx. PCFG)
    root = analysis.pointer.fragment.tree.data
    leaves = getfield.(analysis.pointer.fragment.leaves, :data)
    rm_rule!(startstate(fg), ContextFreeRule(root, (leaves...,)), root, leaves)
end

end