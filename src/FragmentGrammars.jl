__precompile__()
module FragmentGrammars

export Analysis, BaseDistribution, BaseRule, FragmentRule, AbstractRule, Fragment, Pointer, FragmentGrammar
export sample, add_obs!, rm_obs!, iterate
export run_chartparser, sample_tree
export category_type, terminal_type, category_rule_type, terminal_rule_type, startstate, startsymbols, score_type, state_type, completions, prob

import Base: iterate, eltype, length, IteratorSize#, show

include("GeneralizedChartparsing\\src\\GeneralizedChartparsing.jl")
using .GeneralizedChartparsing
using .GeneralizedChartparsing.Trees
using .GeneralizedChartparsing: run_chartparser, sample_tree
import .GeneralizedChartparsing: lhs, rhs, category_type, terminal_type, category_rule_type, terminal_rule_type, startstate, add_rule!, startsymbols, score_type, state_type, completions, prob
import .GeneralizedChartparsing: categorical_sample

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
    crp_obs :: Vector{Fragment}
end

abstract type AbstractRule{C1,C2} end

struct BaseRule{C1,C2} <: AbstractRule{C1,C2}
    # rule :: ContextFreeRule{C1,C2}
    lhs :: C1
    rhs :: Tuple{Vararg{C2}}
end
BaseRule(rule) = BaseRule(#= rule, =# rule.lhs, rule.rhs)
# BaseRule(lhs::C1, rhs::Tuple{Vararg{C2}}) where {C1,C2} = BaseRule(ContextFreeRule{C1,C2}(lhs, rhs))

struct FragmentRule{C1,C2} <: AbstractRule{C1,C2}
    fragment :: Fragment
    # rule :: ContextFreeRule{C1,C2}
    lhs :: C1
    rhs :: Tuple{Vararg{C2}}
end
FragmentRule(rule, fragment) = FragmentRule(fragment, #= rule, =# rule.lhs, rule.rhs)
FragmentRule(fragment) = FragmentRule(fragment, fragment.tree.data, (getfield.(fragment.leaves, :data)...,))

struct ApproxRule{C1,C2} <: AbstractRule{C1,C2}
    lhs :: C1
    rhs :: Tuple{Vararg{C2}}
    rules :: Vector{AbstractRule{C1,C2}}
end

# Unzip rule-logprob pairs into two lists, cast the logprob list into floats, and call categorical_sample
# sample(r::ApproxRule{C1,C2}) = categorical_sample(( x -> (x[1], float.(x[2])) ) (collect(zip(r.rules...)))...)
categorical_sample(tokens::Vector{X}, weights::Vector{LogProb}) where X = categorical_sample(tokens, float.(weights))

lhs(r::AbstractRule) = r.lhs
rhs(r::AbstractRule) = r.rhs
(r::BaseRule)(cat) = r.rhs
(r::BaseRule)() = r.rhs
(r::FragmentRule)(cat) = r.rhs
(r::FragmentRule)() = r.rhs
(r::ApproxRule)(cat) = r.rhs
(r::ApproxRule)() = r.rhs

show(io::IO, r::AbstractRule{C1,C2}) where {C1,C2} =
print("AbstractRule($(lhs(r)),$(rhs(r)))")
show(io::IO, r::BaseRule{C1,C2}) where {C1,C2} =
print("BaseRule($(lhs(r)),$(rhs(r)))")
show(io::IO, r::FragmentRule{C1,C2}) where {C1,C2} =
print("FragmentRule($(lhs(r)),$(rhs(r)))")
show(io::IO, r::ApproxRule{C1,C2}) where {C1,C2} =
print("ApproxRule($(lhs(r)),$(rhs(r)))")

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
function add_rule!(state::State{C,AbstractRule{C,C}}, rule::AbstractRule{C,C}, head, cats) where {C} #, CR<:ApproxRule{C,C}}
    s = state
    for c in cats
        if is_possible_transition(s, c)
            s = transition(s, c)
        else
            s.isfinal = false
            s = s.trans[c] = State(C, AbstractRule{C,C})
        end
    end
    # All unique lhs-rhs pairs are collapsed to include base rules and fragments, and transitions between states determine RHS, therefore we only need to check LHS here.
    aridx = findall((comp -> comp[1] == head), s.comp)
    if isempty(aridx) # if no approx rule yet
        ar = ApproxRule(head, cats, AbstractRule{C,C}[rule])
        push!(s.comp, (head, ar))
    else
        ar = s.comp[aridx[1]][2] # there can be only one
        if !(rule in ar.rules) push!(ar.rules, rule) end
    end
    # push!(s.comp, (head, rule))
end

function rm_rule!(state::State{C,AbstractRule{C,C}}, rule::AbstractRule{C,C}, head, cats) where {C} #, CR<:ApproxRule{C,C}}
    s = state
    for c in cats
        if is_possible_transition(s, c)
            s = transition(s, c)
        else
            s.isfinal = false
            s = s.trans[c] = State(C, CR)
        end
    end
    aridx = findall((comp -> comp[1] == head), s.comp)
    # if isempty(aridx) # if no approx rule yet
        # Nothing needs to happen here, right?
    # else
    if !isempty(aridx) # Shouldn't this never be the case?
        ar = s.comp[aridx[1]][2] # there can be only one
        filter!(r -> r≠rule, ar.rules) # Remove "rule" from the approx rule
        if isempty(ar.rules)
            deleteat!(s.comp, aridx) # Remove empty approx rule from completions
        end
    end
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
    startstate :: State{C, AbstractRule{C,C}}
    terminal_dict :: Dict{T, Vector{Tuple{C, TR}}}
    CRP :: Dict{C, ChineseRest{Fragment}}
    DM :: Dict{C, DirCat{CR, Float64}}
    BB :: Dict{Tuple{C, CR, C}, BetaBern{Bool, Int}}
end

show(io::IO, fg::FragmentGrammar{C, CR, T, TR}) where {C, CR, T, TR} =
    print("FragmentGrammar{$C, $(CR), $T, $(TR)}()")

category_type(fg::FragmentGrammar{C, CR, T, TR}) where {C, CR, T, TR} = C
terminal_type(fg::FragmentGrammar{C, CR, T, TR}) where {C, CR, T, TR} = T
category_rule_type(fg::FragmentGrammar{C, CR, T, TR}) where {C, CR, T, TR} = AbstractRule{C,C}
terminal_rule_type(fg::FragmentGrammar{C, CR, T, TR}) where {C, CR, T, TR} = AbstractRule{C,C}
categories(fg::FragmentGrammar) = fg.categories
terminals(fg::FragmentGrammar) = fg.terminals
preterminals(fg::FragmentGrammar) = fg.preterminals
startstate(fg::FragmentGrammar) = fg.startstate
startsymbols(fg::FragmentGrammar) = fg.startcategories
score_type(fg::FragmentGrammar) = LogProb
state_type(fg::FragmentGrammar{C}) where C = State{C,AbstractRule{C,C}} # typeof(startstate(fg))
completions(fg::FragmentGrammar, state::State) =
    ((cat, r, prob(fg, cat, r)) for (cat, r) in completions(state))
completions(fg::FragmentGrammar{C, CR, T, TR}, t::T) where {C, T, CR, TR} =
    ((cat, rule, prob(fg, cat, rule)) for (cat, rule) in fg.terminal_dict[t])
prob(fg::FragmentGrammar{C}, cat::C, rule::BaseRule{C,C}) where C = one(score_type(fg))
prob(fg::FragmentGrammar{C}, cat::C, rule::R) where {C, R<:ApproxRule{C,C}} = mapreduce(+, rule.rules) do r
    if r isa BaseRule
        logscore(fg.DM[cat], r) * new_table_logscore(fg.CRP[cat])
    elseif r isa FragmentRule
        logscore(fg.CRP[cat], r.fragment, true)
    else error("ApproxRule subrule is of type $(typeof(r)), should be BaseRule or FragmentRule") end
end

"""
    FragmentGrammar(categories, startcategories, category_rules, terminals, terminal_rules[, a::Float64, b::Float64])
"""
function FragmentGrammar(cats::Vector{C}, starts::Vector{C}, cat_rules::Vector{CR}, terms::Vector{T}, term_rules::Vector{TR}, a=0.01, b=0.2) where {C, CR<:BaseRule{C,C}, T, TR}
    # cat_rules = Vector{CR}(cat_rules)
    startstate = State(C, AbstractRule{C,C})# ApproxRule{C,C})
    for r in cat_rules
        add_rule!(startstate, r, r.lhs, r.rhs)
        # rm_rule!(startstate, r, r.lhs, r.rhs)
    end

    CRP = Dict{C,ChineseRest{Fragment}}()
    DM = Dict{C, DirCat{CR, Float64}}(cat => DirCat{CR,Float64}(Dict(x => 1.0 for x in CR[r for (i, r) in enumerate(cat_rules) if r.lhs == cat])) for (j, cat) in enumerate(cats))
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

function sample(fg :: FragmentGrammar{C,CR,T,TR}, cat :: C) where {C, CR, T, TR}
    crp_sample = sample(fg.CRP[cat])
    fragment, dm_counts, bb_counts, crp_counts = crp_sample isa Tuple ? crp_sample : (crp_sample, Tuple{C,CR}[], Pair{Tuple{C,CR,C}, Bool}[], Fragment[])
    children = Dict{Tree, Pointer}()
    for variable in fragment.variables
        ptr, dm_counts_child, bb_counts_child, crp_counts_child = sample(fg, variable.data)
        append!(dm_counts, dm_counts_child)
        append!(bb_counts, bb_counts_child)
        append!(crp_counts, crp_counts_child)
        push!(children, variable => ptr)
    end
    return Pointer(fragment, children), dm_counts, bb_counts, crp_counts
end

struct BaseDistribution{C} <: Distribution{Fragment}
    fg :: FragmentGrammar{C}
    category :: C
end

show(io::IO, bd::BaseDistribution{C}) where C = print("BaseDistribution{$C}()")

# logscore(::BaseDistribution, frag::Fragment) =

function sample(basedist :: BaseDistribution)
    C, CR, T, TR = category_type(basedist.fg), category_rule_type(basedist.fg), terminal_type(basedist.fg), terminal_rule_type(basedist.fg)

    variables = Tree{C}[]
    leaves = Tree{C}[]
    tree = TreeNode(basedist.category)

    dm_sample = sample(basedist.fg.DM[basedist.category])
    dm_counts = Tuple{C,CR}[(basedist.category, dm_sample)]
    # push!(dm_counts, (basedist.category, dm_sample))
    bb_counts = Pair{Tuple{C, CR, C}, Bool}[]
    crp_counts = Fragment[]

    r = dm_sample
    children = r(basedist.category) # or r.rhs or rhs(r)

    for child in children
        if child in basedist.fg.preterminals
            # Make a leaf node (non-variable)
            leaf_tree = TreeNode(child)
            insert_child!(tree, leaf_tree)
            push!(leaves, leaf_tree)
        else
            bbidx = (basedist.category, dm_sample, child)
            extend = sample(basedist.fg.BB[bbidx])
            push!(bb_counts, bbidx => extend)
            if extend # if we extend the fragment
                # Get (recursive) Pointer, DM counts, BB counts, and CRP counts (fragments)
                ptr, dm_counts_child, bb_counts_child, crp_counts_child = sample(basedist.fg, child)
                fragchild = ptr.fragment # We only need the fragment for the base distribution
                # Merge BB counts
                append!(bb_counts, bb_counts_child)
                # Merge DM counts
                append!(dm_counts, dm_counts_child)
                # Merge CRP counts (recursively counts only if we're *extending* a non-terminal with another fragment)
                # If it's a new fragment, it may contain a fragment in crp_counts_child. If it's already stored, then it contains nothing.
                append!(crp_counts, crp_counts_child)
                # Add to tree
                insert_child!(tree, deepcopy(fragchild.tree))   # Better than modifying the underlying tree if taken from CRP preexisting fragment
                # Merge variables (non-terminal leaves) from recursive calls to FG
                append!(variables, fragchild.variables)
                append!(leaves, fragchild.leaves)
            else
                # Make a leaf node (variable)
                variable_tree = TreeNode(child)
                push!(variables, variable_tree)
                push!(leaves, variable_tree)
                insert_child!(tree, variable_tree)
            end
        end
    end

    frag = Fragment(tree, variables, leaves)
    push!(crp_counts, frag)

    return frag, dm_counts, bb_counts, crp_counts
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
    for frag in analysis.crp_obs
        add_obs!(fg.CRP[frag.tree.data], frag)
        fr = FragmentRule(frag)
        add_rule!(startstate(fg), fr, fr.lhs, fr.rhs)
    end
    for frag_ptr in analysis.pointer
        add_obs!(fg.CRP[frag_ptr.fragment.tree.data], frag_ptr.fragment)
        # Add completions to finite state machine (for approx. PCFG)
        fr = FragmentRule(frag_ptr.fragment)
        add_rule!(startstate(fg), fr, fr.lhs, fr.rhs)
    end
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
    for frag in analysis.crp_obs
        rm_obs!(fg.CRP[frag.tree.data], frag)
        fr = FragmentRule(frag)
        rm_rule!(startstate(fg), fr, fr.lhs, fr.rhs)
    end
    for frag_ptr in analysis.pointer
        rm_obs!(fg.CRP[frag_ptr.fragment.tree.data], frag_ptr.fragment)
        # rm completions to finite state machine (for approx. PCFG)
        fr = FragmentRule(frag_ptr.fragment)
        rm_rule!(startstate(fg), fr, fr.lhs, fr.rhs)
    end
end

end
