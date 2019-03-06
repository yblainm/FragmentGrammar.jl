__precompile__()
module FragmentGrammars

using GeneralizedChartParsing
using GeneralizedChartParsing.Trees: Tree
include("CompoundDists.jl"); using .CompoundDists
include("parse_a_tree.jl")

export FragmentGrammar, forwardSample

###########################
# Dummy structs/functions #
###########################

struct DummyDistribution{T} <: Distribution{T} end
sample(dist::DummyDistribution) = sampleTree(g, test_str)

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
    let basedist = DummyDistribution{Tree}(), a = 0.0, b = 0.1
        FragmentGrammar(g, Dict{C,ChineseRest{Tree}}(cat => ChineseRest(a, b, basedist)
        for cat in g.categories), a, b, Dict{Tuple{Int, Int}, Int}(), Dict{Int, Int}())
    end
end

function forwardSample(fg :: FragmentGrammar{C}) where C
    start = fg.baseGrammar.start_categories[1]
    #fragment = fg.restaurants[start] |> sample
    #add_obs!(fg.restaurants[start], fragment)
end

end

#########
# Tests #
#########

using .FragmentGrammars
using GeneralizedChartParsing
include("CompoundDists.jl"); using .CompoundDists
include("parse_a_tree.jl")

str = "the dog paints prep dog"
g2 = add_score(g, :enum_forest, enum_forest_score)
score = parse(g2, split(str))["S"].enum_forest.trees[1]

println(forwardSample(FragmentGrammar(g)))
#println("Fuck")
