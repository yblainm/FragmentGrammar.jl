#########
# Tests #
#########
module tests

include("FragmentGrammars.jl"); using .FragmentGrammars
using .FragmentGrammars: DummyDistribution
using GeneralizedChartParsing
using GeneralizedChartParsing.Trees
include("parse_a_tree.jl")

using Profile

# str = "the dog paints prep dog"
# g2 = add_score(g, :enum_forest, enum_forest_score)
# score = parse(g2, split(str))["S"].enum_forest.trees[1]
isapplicable(r, c) = r(c) !== nothing

# for i in 1:100
#     forwardSample(fg)
# end
#println(fg.restaurants)
# println(fg.DM)
#println(fg.BB)

# a = Tree(SubString{String}("Okay"), AbstractString)
# add_child!(a, Tree("I see", AbstractString))
# println(a)
#
# @time for i in 1:10 sampleHelper(fg, Tree(fg.baseGrammar.start_categories[1], AbstractString)) end
# @time for i in 1:1000 sampleHelper(fg, Tree(fg.baseGrammar.start_categories[1], AbstractString)) end
# @time for i in 1:10000 sampleHelper(fg, Tree(fg.baseGrammar.start_categories[1], AbstractString)) end
# @show hash(Tree(fg.baseGrammar.start_categories[1], AbstractString))
# @show hash(Tree(fg.baseGrammar.start_categories[1], AbstractString))
# @show isequal(Tree(fg.baseGrammar.start_categories[1], AbstractString),Tree(fg.baseGrammar.start_categories[1], AbstractString))

# frags = [Tree(fg.baseGrammar.start_categories[1], AbstractString)]
# full = Tree(fg.baseGrammar.start_categories[1], AbstractString)
# @time for i in 1:50000 sampleHelper(fg, frags[1], frags, full) end


println("Test run------------")
fg = FragmentGrammar(g, Union{String, SubString{String}})
@profile sample(fg, "S")
Profile.print()
end
