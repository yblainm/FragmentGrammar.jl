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

println("Test run------------")
@show sampleHelper(fg, Tree(fg.baseGrammar.start_categories[1], AbstractString))
# @time for i in 1:1 sampleHelper(fg, Tree(fg.baseGrammar.start_categories[1], AbstractString)) end
# @show hash(Tree(fg.baseGrammar.start_categories[1], AbstractString))
# @show hash(Tree(fg.baseGrammar.start_categories[1], AbstractString))
# @show isequal(Tree(fg.baseGrammar.start_categories[1], AbstractString),Tree(fg.baseGrammar.start_categories[1], AbstractString))

# frags = [Tree(fg.baseGrammar.start_categories[1], AbstractString)]
# full = Tree(fg.baseGrammar.start_categories[1], AbstractString)
# @time for i in 1:50000 sampleHelper(fg, frags[1], frags, full) end

end
