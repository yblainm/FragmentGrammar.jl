__precompile__()
module FragmentGrammars

using GeneralizedChartParsing
using GeneralizedChartParsing.Trees: Tree
include("CompoundDists.jl"); using .CompoundDists
import .CompoundDists: sample
import Base.convert
include("parse_a_tree.jl")

export FragmentGrammar, forwardSample, convert, DummyDistribution, sample, sampleHelper

###########################
# Dummy structs/functions #
###########################

struct DummyDistribution{T} <: Distribution{T} end
sample(dist::DummyDistribution{Tree}) = sampleTree(g, test_str)

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

mutable struct FragmentGrammar{C} <: Distribution{Tree}
    baseGrammar :: Grammar{C}

    restaurants :: Dict{C,ChineseRest{Tree}}
    a :: Float64 # discount parameter
    b :: Float64 # crp parameter

    # Double-check
    DM :: Dict{C, DirMul{<:Function, Float64}}
    BB :: Dict{Tuple{C, <:Function, C}, BetaBern{Bool, Int}}
end

isapplicable(r, c) = r(c) !== nothing

function FragmentGrammar(g :: Grammar{C}) where C
    let basedist = DummyDistribution{Tree}(), a = 0.2, b = 5.0
        FragmentGrammar(g, Dict{C,ChineseRest{Tree}}(cat => ChineseRest(a, b, basedist)
        for cat in g.categories), a, b, Dict{C, DirMul{<:Function, Float64}}(cat => DirMul([r for r in g.all_rules if isapplicable(r, cat)]) for cat in g.categories), Dict{Tuple{C,<:Function,C}, BetaBern{Bool, Int}}())
    end
end

function forwardSample(fg :: FragmentGrammar)
    start = fg.baseGrammar.start_categories[1]
    fragment = fg.restaurants[start] |> sample
    #TODO Write recursive helper function.
    #   helper(FG, currentTreeFragment, listOfFragments, fullTree)
    add_obs!(fg.restaurants[start], fragment)
end

function sampleHelper(fg :: FragmentGrammar, currentTree :: Tree, trees :: Array{Tree{T}, 1}, fullTree :: Tree) where T
    if currentTree.value in keys(fg.baseGrammar.terminal_dict)
        return currentTree
    end

    local r
    rules_sample = sample(get(fg.DM, currentTree.value, nothing), 1)
    if rules_sample === nothing
        return nothing
    end

    for (k, v) in rules_sample
        if v == 1
            r = k
        end
    end

    children = r(currentTree.value)
    Ty = typeof(children)

    if Ty <: Tuple  # if binary rule
        # append!(currentTree.children, [Tree(child) for child in children])
        for child in children
            childTree = sampleHelper(fg, Tree(child), trees, fullTree)

            # IF BB -> KEEP:
            currentTree.add_child!(childTree)
            # Add to fullTree (MAKE A COPY METHOD)

            # IF BB -> FRAGMENT then
            # append!(trees, childtree)
            # Add to fullTree (MAKE A COPY METHOD)
    else
        childTree = sampleHelper(fg, Tree(children), trees, fullTree)
        currentTree.add_child!(Tree(children))
        # append!(currentTree.children, Tree(children))
    end

    return currentTree
end

function convert(::Type{Tree{T}}, tree::Tree) where T
    newtree = Tree(convert(T, tree.value))
    for child in tree.children
        add_child!(newtree, convert(Tree{T}, child))
    end
    newtree
end

end
