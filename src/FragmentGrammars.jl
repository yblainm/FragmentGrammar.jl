module FragmentGrammars

using GeneralizedChartParsing
include("CompoundDists.jl"); using .CompoundDists
include("parse_a_tree.jl")

mutable struct FragmentGrammar{C}
    baseGrammar :: Grammar{C}
    restaurants :: Dict{C,ChineseRest{GeneralizedChartParsing.Trees.Tree}}

    # For these, index by rule index or by the rule function itself?
    pi :: Dict{Tuple{String, Int}, Int}
    psi :: Dict{Int}
end

FragmentGrammar() = FragmentGrammar(
    g, ChineseRest()
)

function FragmentGrammar(g :: Grammar)

end

# println(FragmentGrammar(g))
# println("test")

# println(g.categories)

str = "the dog paints prep dog"
g2 = add_score(g, :enum_forest, enum_forest_score)
score = parse(g2, split(str))["S"].enum_forest.trees[1]

println(score)

end
