module FragmentGrammars

using GeneralizedChartParsing
include("CompoundDists.jl"); using .CompoundDists
include("parse_a_tree.jl")

mutable struct FragmentGrammar{C}
    baseGrammar :: Grammar
    restaurants :: Dict{C,ChineseRest}

    # For these, index by rule index or by the rule function itself?
    pi :: Dict{Tuple{String, Int}, Int}
    psi :: Dict{Int}
end

function FragmentGrammar(g :: Grammar)

end

# println(FragmentGrammar(g))
# println("test")

# println(g.categories)

str = "the dog paints prep dog"
scores = parse(g, split(str))
g2 = add_score(g, :enum_forest, enum_forest_score)
score = parse(g2, split(str))

println(scores)

end
