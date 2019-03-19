__precompile__()
module FragmentGrammars


import Base: iterate, eltype, length, IteratorSize

using GeneralizedChartParsing
using GeneralizedChartParsing: isapplicable
using GeneralizedChartParsing.Trees

include("CompoundDists.jl"); using .CompoundDists
import .CompoundDists: sample, add_obs!, rm_obs!

export Analysis, BaseDistribution, Fragment, Pointer, FragmentGrammar, sample, ChineseRest, add_obs!, rm_obs, iterate
# export convert

include("parse_a_tree.jl")

###########################
# Helper structs and functions #
###########################

struct DummyDistribution{T} <: Distribution{T} end
sample(dist::DummyDistribution{Tree}) = sampleTree(g, test_str)

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
    bb_obs :: Array{Pair{Tuple{Int,Int,Int},Bool},1} # Julia uses a flyweight for these pairs!
    # Should this be a dict though?
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

# TODO: initialize FG from a toy input grammar given in constructors
#   - Add 1 count per PCFG rule by default (?)  DONE
#   - FG initially has nothing in it, so given the input grammar, we can either
#       add_obs!() or sample. Sampling makes most sense, and will add observations
#       to the restaurants and BB (and DM).
#   - This requires us to write the base distribution code. Should work according
#       to Tim's book.

mutable struct FragmentGrammar{C, D}
    baseGrammar :: Grammar{C}
    CRP :: Array{ChineseRest{Fragment},1}
    DM :: Array{DirCat{Int, Float64},1}
    BB :: Dict{Tuple{Int, Int, Int}, BetaBern{Bool, Int}}
    treeType :: Type{D} # TODO Try using valtype(<object>) throughout instead?
    categories :: Array{D,1}
end

struct BaseDistribution{C} <: Distribution{Fragment}
    fg :: FragmentGrammar{C}
    catidx :: Int
    category :: C
end

function FragmentGrammar(g :: Grammar{C}, t::Type) where C
    let a = 0.01, b = 0.2
        fg = FragmentGrammar(g,
        # CRP
        ChineseRest{Fragment}[],
        # DM
        DirCat{Int, Float64}[DirCat([i for (i, r) in enumerate(g.all_rules) if isapplicable(r, cat)]) for (j, cat) in enumerate(g.categories)],
        # BB
        Dict{Tuple{Int, Int, Int}, BetaBern{Bool, Int}}((catidx, ruleidx, rhs) => BetaBern(1, 1) for (rhss, comps) in g.binary_dict for rhs in rhss for (catidx, ruleidx) in comps),
        # Type (for trees, ffs)
        t,
        # List of categories for sanity (will be Union{String, SubString{String}})
        t[cat for cat in g.categories]
        )
        # Instantiate CRPs
        fg.CRP = ChineseRest{Fragment}[ChineseRest(a, b, BaseDistribution(fg, i, cat)) for (i, cat) in enumerate(g.categories)]
        fg
    end
end

function sample(basedist :: BaseDistribution)
    variables = Tree{basedist.fg.treeType}[]
    tree = Tree(basedist.category, basedist.fg.treeType)

    # get_ruleidx(rules::Dict) = for (k, v) in rules if v == 1 return k end end

    dm_sample = sample(basedist.fg.DM[basedist.catidx])
    dm_counts = Tuple{Int,Int}[]
    push!(dm_counts, (basedist.catidx, dm_sample))
    bb_counts = Pair{Tuple{Int,Int,Int}, Bool}[]
    # ruleidx = get_ruleidx(dm_sample)
    r = basedist.fg.baseGrammar.all_rules[dm_sample]
    children = r(basedist.category)
    Ty = typeof(children)

    # TODO Maybe I can use Base.deepcopy(x) to make trees and what not

    if Ty <: Tuple  # if binary rule (implies RHS is non-terminal)
        for child in children
            childidx = get_idx(basedist.fg.categories, child)
            bbidx = (basedist.catidx, dm_sample, childidx)
            flip = sample(basedist.fg.BB[bbidx])
            push!(bb_counts, bbidx => flip)
            if flip # if we extend the fragment
                # Get (recursive) Pointer, DM counts, and BB counts
                ptr, dm_counts_child, bb_counts_child = sample(basedist.fg, childidx)
                frag = ptr.fragment # We only need the fragment for the base distribution
                # Merge BB counts
                append!(bb_counts, bb_counts_child)
                # Merge DM counts
                append!(dm_counts, dm_counts_child)
                add_child!(tree, deepcopy(frag.tree))   # Better than modifying the underlying tree if taken from CRP preexisting fragment
                # Merge variables (non-terminal leaves) from recursive calls to FG
                append!(variables, frag.variables)
            else
                # Make a leaf node (variable)
                variable_tree = Tree(child, basedist.fg.treeType)
                push!(variables, variable_tree)
                add_child!(tree, variable_tree)
            end
        end
    else    # if unary (terminal) rule
        # if children in keys(basedist.fg.baseGrammar.terminal_dict) # if RHS is terminal
        add_child!(tree, Tree(children, basedist.fg.treeType))
        dm_counts = Tuple{Int,Int}[]
        bb_counts = Pair{Tuple{Int,Int,Int},Bool}[]
        # end
    end

    return Fragment(tree, variables), dm_counts, bb_counts
end

function sample(fg :: FragmentGrammar, catidx :: Int) where C
    crp_sample = sample(fg.CRP[catidx])
    fragment, dm_counts, bb_counts = typeof(crp_sample) <: Tuple ? crp_sample : (crp_sample, [], [])
    children = Dict{Tree, Pointer}()
    for variable in fragment.variables
        ptr, dm_counts_child, bb_counts_child = sample(fg, get_idx(fg.categories, variable.value))
        append!(dm_counts, dm_counts_child)
        append!(bb_counts, bb_counts_child)
        push!(children, variable => ptr)
    end
    return Pointer(fragment, children), dm_counts, bb_counts
end

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
        add_obs!(fg.CRP[get_idx(fg.categories, frag_ptr.fragment.tree.value)], frag_ptr.fragment)
    end
end

function rm_obs!(fg :: FragmentGrammar, analysis :: Analysis)
    # Add DM counts
    for dm_obs in analysis.dm_obs
        rm_obs!(fg.DM[dm_obs[1]], dm_obs[2])
    end
    # Add BB counts
    for bb_obs in analysis.bb_obs
        rm_obs!(fg.BB[bb_obs[1]], bb_obs[2])
    end
    # Add fragments to CRP
    for frag_ptr in analysis.pointer
        rm_obs!(fg.CRP[get_idx(fg.categories, frag_ptr.fragment.tree.value)], frag_ptr.fragment)
    end
end

end
