__precompile__()
module FragmentGrammars


# import Base: convert, promote_rule

using GeneralizedChartParsing
using GeneralizedChartParsing.Trees

include("CompoundDists.jl"); using .CompoundDists
import .CompoundDists: sample

export FragmentGrammar, forwardSample, DummyDistribution, sample, sampleHelper
# export convert

include("parse_a_tree.jl")

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
        FragmentGrammar(g,
        Dict{C,ChineseRest{Tree}}(cat => ChineseRest(a, b, basedist) for cat in g.categories),
        a, b,
        Dict{C, DirMul{<:Function, Float64}}(cat => DirMul([r for r in g.all_rules if isapplicable(r, cat)]) for cat in g.categories),
        Dict{Tuple{C,<:Function,C}, BetaBern{Bool, Int}}((g.categories[catidx], g.all_rules[ruleidx], g.categories[rhs]) => BetaBern(1, 1) for (rhss, comps) in g.binary_dict for rhs in rhss for (catidx, ruleidx) in comps))

    end
end

function forwardSample(fg :: FragmentGrammar)
    start = fg.baseGrammar.start_categories[1]
    fragment = fg.restaurants[start] |> sample
    #TODO Write recursive helper function.
    #   helper(FG, currentTreeFragment, listOfFragments, fullTree)
    #   Parameter tree should be of type `supertype` of the categories, unless
    #   that supertype is Any. Hopefully that should work for most possible
    #   cases including custom structs.
    add_obs!(fg.restaurants[start], fragment)
end

function sampleHelper(fg :: FragmentGrammar, currentTree :: Tree{T}) where T
    get_rule(rules::Dict) = for (k, v) in rules if v == 1 return k end end

    dm_sample = sample(get(fg.DM, currentTree.value, nothing), 1) # sample from nothing in default case??? Fix this.
    dm_counts = collect(Pair{Function, Int}, dm_sample)
    local bb_counts # :: Tuple{Tuple{T,Function,T}, Bool}

    r = get_rule(dm_sample)
    children = r(currentTree.value)
    Ty = typeof(children)

    if Ty <: Tuple  # if binary rule (implies RHS is non-terminal)
        for child in children
            childTree, child_dm_sample, child_bb_counts = sampleHelper(fg, Tree(child, T))
            child_dm_counts = collect(Pair{Function, Int}, child_dm_sample)
            bbidx = (currentTree.value, r, child)
            if (keep = sample(fg.BB[bbidx]))
                add_child!(currentTree, childTree)
                append!(dm_counts, child_dm_counts)
            else
                add_child!(currentTree, Tree(child,T))
            end
            push!(child_bb_counts, (bbidx, keep))
            bb_counts = child_bb_counts
        end
    else    # if unary (terminal) rule
        # bbidx = (currentTree.value, r, children)
        if children in keys(fg.baseGrammar.terminal_dict) # if RHS is terminal
            add_child!(currentTree, Tree(children, T))
            bb_counts = Tuple{Tuple{T,Function,T}, Bool}[]
        # elseif (keep = sample(fg.BB[bbidx])) # if RHS is non-terminal (won't happen)
        #     childTree, child_dm_sample = sampleHelper(fg, Tree(child, T))
        #     child_dm_counts = collect(Pair{Function, Int}, child_dm_sample)
        #     append!(dm_counts, child_dm_counts)
        #     add_child!(currentTree, childTree)
        end
    end

    return currentTree, dm_counts, bb_counts
end

# function sampleHelper(fg :: FragmentGrammar, currentTree :: Tree{T}, trees :: Array{Tree{T}, 1}, fullTree :: Tree{T}) where T
#     # println(currentTree.value, " ", typeof(currentTree.value))
#     if currentTree.value in keys(fg.baseGrammar.terminal_dict)
#         return currentTree, fullTree
#     end
#
#     local r
#     rules_sample = sample(get(fg.DM, currentTree.value, nothing), 1)
#     if rules_sample === nothing
#         return nothing
#     end
#
#     for (k, v) in rules_sample
#         if v == 1 # There is only one of these
#             r = k
#         end
#     end
#
#     children = r(currentTree.value)
#     Ty = typeof(children)
#
#     if Ty <: Tuple  # if binary rule (implies RHS is non-terminal)
#         for child in children
#             childTree, childFullTree = sampleHelper(fg, Tree(child, T), trees, Tree(child, T))
#             add_child!(fullTree, childFullTree)
#
#             # IF BB -> KEEP:
#             if (keep = sample(fg.BB[(currentTree.value, r, child)]))
#                 add_child!(currentTree, childTree)
#
#             # IF BB -> FRAGMENT then
#             else
#                 add_child!(currentTree, Tree(child, T)) # add the non-terminal only
#                 push!(trees, childTree)
#             end
#         end
#     else    # if unary (terminal) rule
#         childTree, childFullTree = sampleHelper(fg, Tree(children, T), trees, Tree(children, T))
#         add_child!(currentTree, childTree)
#         add_child!(fullTree, childFullTree)
#     end
#
#     return currentTree, fullTree
# end

# function convert(::Type{Tree{T}}, tree::Tree{V}) where {T, V}
#     if V === T return tree end
#     newtree = Tree(convert(T, tree.value))
#     for child in tree.children
#         add_child!(newtree, convert(Tree{T}, child))
#     end
#     newtree
# end
#
# promote_rule(::Type{Tree{SubString{String}}}, ::Type{Tree{String}}) = Tree{String}


end
