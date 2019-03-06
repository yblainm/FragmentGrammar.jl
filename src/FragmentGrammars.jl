module FragmentGrammars

using GeneralizedChartParsing
using GeneralizedChartParsing.Trees: Tree
include("CompoundDists.jl"); using .CompoundDists
include("parse_a_tree.jl")

export FragmentGrammar

###########################
# Dummy structs/functions #
###########################

struct DummyDistribution{T} <: Distribution{T} end
sample(dist::DummyDistribution{Tree}) = sampleTree(g, test_str)

################################
# Fragment Grammar definitions #
################################

mutable struct FragmentGrammar{C}
    baseGrammar :: Grammar{C}
    restaurants :: Dict{C,ChineseRest{Tree}}

    a :: Float64 # discount parameter
    b :: Float64 # crp parameter

    # For these, index by rule index or by the rule function itself?
    pi :: Dict{Tuple{String, Int}, Int}
    psi :: Dict{Int, Int}
end

function FragmentGrammar(g :: Grammar{C}) where C
    let basedist = DummyDistribution{Tree}(), a = 0.0, b = 0.1
        FragmentGrammar(g, Dict{C,ChineseRest{Tree}}(cat => ChineseRest(a, b, basedist) for cat in g.categories), a, b, Dict{Tuple{String, Int}, Int}(), Dict{Int, Int}())
    end
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

println(FragmentGrammar(g))
