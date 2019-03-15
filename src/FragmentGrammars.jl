__precompile__()
module FragmentGrammars


# import Base: convert, promote_rule

using GeneralizedChartParsing
using GeneralizedChartParsing: isapplicable
using GeneralizedChartParsing.Trees

include("CompoundDists.jl"); using .CompoundDists
import .CompoundDists: sample

export FragmentGrammar, BaseDistribution, sample
# export convert

include("parse_a_tree.jl")

###########################
# Dummy structs/functions #
###########################

struct DummyDistribution{T} <: Distribution{T} end
sample(dist::DummyDistribution{Tree}) = sampleTree(g, test_str)

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

struct Fragment
    tree :: Tree
    variables :: Array{Tree,1}
end

struct Pointer
    fragment :: Fragment
    children :: Dict{Tree, Pointer}
end

mutable struct FragmentGrammar{C, D}
    baseGrammar :: Grammar{C}
    CRP :: Array{ChineseRest{Fragment},1}
    DM :: Array{DirMul{Int, Float64},1}
    BB :: Dict{Tuple{Int, Int, Int}, BetaBern{Bool, Int}}
    treeType :: Type{D}
    categories :: Array{D, 1}
end

struct BaseDistribution{C} <: Distribution{Fragment}
    fg :: FragmentGrammar{C}
    catidx :: Int
    category :: C
end

# isapplicable(r, c) = r(c) !== nothing

function FragmentGrammar(g :: Grammar{C}, t::Type) where C
    let a = 0.2, b = 5.0
        fg = FragmentGrammar(g,
        # CRP
        ChineseRest{Fragment}[],
        # DM
        DirMul{Int, Float64}[DirMul([i for (i, r) in enumerate(g.all_rules) if isapplicable(r, cat)]) for (j, cat) in enumerate(g.categories)],
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

    get_ruleidx(rules::Dict) = for (k, v) in rules if v == 1 return k end end

    dm_sample = sample(basedist.fg.DM[basedist.catidx], 1)
    ruleidx = get_ruleidx(dm_sample)
    r = basedist.fg.baseGrammar.all_rules[ruleidx]
    children = r(basedist.category)
    Ty = typeof(children)

    # TODO Maybe I can use Base.deepcopy(x) to make trees and what not

    if Ty <: Tuple  # if binary rule (implies RHS is non-terminal)
        for child in children
            childidx = get_idx(basedist.fg.categories, child)
            bbidx = (basedist.catidx, ruleidx, childidx)
            if (flip = sample(basedist.fg.BB[bbidx])) # if we extend the fragment
                frag = sample(basedist.fg, childidx).fragment
                add_child!(tree, deepcopy(frag.tree))
                # push!(tree.children, frag.tree)   # Only points toward the tree, its parent is still ::Nothing
                append!(variables, frag.variables)
            else
                variable_tree = Tree(child, basedist.fg.treeType)
                push!(variables, variable_tree)
                add_child!(tree, variable_tree)
            end
        end
    else    # if unary (terminal) rule
        # if children in keys(basedist.fg.baseGrammar.terminal_dict) # if RHS is terminal
        add_child!(tree, Tree(children, basedist.fg.treeType))
        # end
    end

    return Fragment(tree, variables)
end

function sample(fg :: FragmentGrammar, catidx :: Int) where C
    fragment = sample(fg.CRP[catidx])
    children = Dict{Tree, Pointer}()
    for variable in fragment.variables
        push!(children, variable => sample(fg, get_idx(fg.categories, variable.value)))
    end
    return Pointer(fragment, children)
end

# function sampleHelper(fg :: FragmentGrammar, currentTree :: Tree{T}) where T
#     get_rule(rules::Dict) = for (k, v) in rules if v == 1 return k end end
#
#     dm_sample = sample(get(fg.DM, currentTree.value, nothing), 1) # sample from nothing in default case??? Fix this.
#     dm_counts = collect(Pair{Function, Int}, dm_sample)
#     local bb_counts = Tuple{Tuple{T,Function,T}, Bool}[]
#
#     r = get_rule(dm_sample)
#     children = r(currentTree.value)
#     Ty = typeof(children)
#
#     if Ty <: Tuple  # if binary rule (implies RHS is non-terminal)
#         for child in children
#             bbidx = (currentTree.value, r, child)
#             if (keep = sample(fg.BB[bbidx]))
#                 childTree, child_dm_sample, child_bb_counts = sampleHelper(fg, Tree(child, T))
#                 append!(bb_counts, child_bb_counts)
#                 child_dm_counts = collect(Pair{Function, Int}, child_dm_sample)
#                 add_child!(currentTree, childTree)
#                 append!(dm_counts, child_dm_counts)
#             else
#                 add_child!(currentTree, Tree(child,T))
#             end
#             push!(bb_counts, (bbidx, keep))
#         end
#     else    # if unary (terminal) rule
#         # bbidx = (currentTree.value, r, children)
#         if children in keys(fg.baseGrammar.terminal_dict) # if RHS is terminal
#             add_child!(currentTree, Tree(children, T))
#             bb_counts = Tuple{Tuple{T,Function,T}, Bool}[]
#         # elseif (keep = sample(fg.BB[bbidx])) # if RHS is non-terminal (won't happen)
#         #     childTree, child_dm_sample = sampleHelper(fg, Tree(child, T))
#         #     child_dm_counts = collect(Pair{Function, Int}, child_dm_sample)
#         #     append!(dm_counts, child_dm_counts)
#         #     add_child!(currentTree, childTree)
#         end
#     end
#
#     return currentTree, dm_counts, bb_counts
# end

end
