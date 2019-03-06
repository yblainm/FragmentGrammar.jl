__precompile__()
module FragmentGrammars

using GeneralizedChartParsing
using GeneralizedChartParsing.Trees: Tree
include("CompoundDists.jl"); using .CompoundDists; import .CompoundDists: sample
include("parse_a_tree.jl")

export FragmentGrammar, forwardSample, DummyDistribution, sample

###########################
# Dummy structs/functions #
###########################

struct DummyDistribution{T} <: Distribution{T} end
sample(dist::DummyDistribution{Tree}) = sampleTree(g, test_str)

################################
# Fragment Grammar definitions #
################################

# TODO: initialize FG from a toy input grammar given in constructors
#   - Add 1 count per PCFG rule by default (?)
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

    # For these, index by rule index or by the rule function itself?
    pi :: Dict{Tuple{Int, Int}, Int}
    psi :: Dict{Int, Int}
end

function FragmentGrammar(g :: Grammar{C}) where C
    let basedist = DummyDistribution{Tree}(), a = 0.2, b = 5.0
        FragmentGrammar(g, Dict{C,ChineseRest{Tree}}(cat => ChineseRest(a, b, basedist)
        for cat in g.categories), a, b, Dict{Tuple{Int, Int}, Int}(), Dict{Int, Int}())
    end
end

function forwardSample(fg :: FragmentGrammar)
    start = fg.baseGrammar.start_categories[1]
    fragment = fg.restaurants[start] |> sample
    # println(fg.restaurants[start].num_tables)
    add_obs!(fg.restaurants[start], fragment)
end

end
