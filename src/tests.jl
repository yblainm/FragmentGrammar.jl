#########
# Tests #
#########
module tests

include("FragmentGrammars.jl"); using .FragmentGrammars
using .FragmentGrammars: DummyDistribution
using GeneralizedChartParsing
using GeneralizedChartParsing.Trees: Tree
include("parse_a_tree.jl")

# str = "the dog paints prep dog"
# g2 = add_score(g, :enum_forest, enum_forest_score)
# score = parse(g2, split(str))["S"].enum_forest.trees[1]
fg = FragmentGrammar(g)
for i in 1:100
    forwardSample(fg)
end
#println(fg.restaurants)
println(fg.DM)

end
