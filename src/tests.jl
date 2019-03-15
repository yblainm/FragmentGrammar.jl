#########
# Tests #
#########
module tests
using Base: to_index

include("FragmentGrammars.jl"); using .FragmentGrammars
using .FragmentGrammars: DummyDistribution
using GeneralizedChartParsing
using GeneralizedChartParsing.Trees
include("parse_a_tree.jl")

using Profile

# str = "the dog paints prep dog"
# g2 = add_score(g, :enum_forest, enum_forest_score)
# score = parse(g2, split(str))["S"].enum_forest.trees[1]

println("Test run------------")
@time fg = FragmentGrammar(g, Union{String, SubString{String}})
# @show fg.categories
@show sample(fg, 1)
# @show @time sample(fg, 1)
# @time Dict(i=>i for i in 1:100000)
# @show @time sample(fg, 1) # 1.037180 seconds (1.98 M allocations: 98.387 MiB, 6.16% gc time)
# @time b = Dict(i=>i for i in 1:100000) # 0.046093 seconds (91.41 k allocations: 10.169 MiB)
# @time sampleTree(g, test_str) # 0.007541 seconds (9.36 k allocations: 514.309 KiB)
# @show @time sample(fg, 1) # 0.000381 seconds (417 allocations: 27.750 KiB)
# @show @time for i in 1:10000 sample(fg, 1) end # 1.972832 seconds (4.64 M allocations: 288.958 MiB, 7.24% gc time)
# @profile sample(fg, "S")
# Profile.print()
end
