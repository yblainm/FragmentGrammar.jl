#########
# Tests #
#########
module tests

include("FragmentGrammars.jl"); using .FragmentGrammars
using .FragmentGrammars: DummyDistribution
using GeneralizedChartParsing
using GeneralizedChartParsing.Trees
include("parse_a_tree.jl")

# str = "the dog paints prep dog"
# g2 = add_score(g, :enum_forest, enum_forest_score)
# score = parse(g2, split(str))["S"].enum_forest.trees[1]
isapplicable(r, c) = r(c) !== nothing

fg = FragmentGrammar(g)
# for i in 1:100
#     forwardSample(fg)
# end
#println(fg.restaurants)
# println(fg.DM)
#println(fg.BB)

# a = Tree(SubString{String}("Okay"), AbstractString)
# add_child!(a, Tree("I see", AbstractString))
# println(a)

trees = [Tree(fg.baseGrammar.start_categories[1], AbstractString)]
sampledTree = Tree(fg.baseGrammar.start_categories[1], AbstractString)
println("Test run------------")
sampleHelper(fg, trees[1], trees, sampledTree)
println("Full tree: ", sampledTree)
println("Fragments:")
for t in trees println(t) end


# a = Tree("A", AbstractString)
# add_child!(a, Tree("B", AbstractString))
# push!(trees, a)
# println(trees)

end
