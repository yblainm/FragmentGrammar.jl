#########
# Tests #
#########
module tests
include("FragmentGrammars.jl"); using .FragmentGrammars
using GeneralizedChartParsing
using GeneralizedChartParsing.Trees
include("parse_a_tree.jl")

using Profile # Lets you see how many backtraces (function calls) come from a specific code block during runtime. Good for finding bottlenecks.
# EXAMPLE:
# Profile.clear()  # in case we have any previous profiling data
# @profile sample(fg, "S")
# Profile.print()
# OR
# using ProfileView; ProfileView.view() # opens an interactive visualization instead of printing to stdout.
#

# str = "the dog paints prep dog"
# g2 = add_score(g, :enum_forest, enum_forest_score)
# score = parse(g2, split(str))["S"].enum_forest.trees[1]

println("Test run------------")

# some_tree = Tree("NP", Union{String, SubString{String}})
# add_child!(some_tree, Tree("D", Union{String, SubString{String}}))
# add_child!(some_tree, Tree("N", Union{String, SubString{String}}))

fg = FragmentGrammar(g, Union{String, SubString{String}})
# add_obs!(fg.CRP[2], Fragment(some_tree, some_tree.children))
analyses = Analysis[]
for i in 1:1000
    anly = Analysis(sample(fg, rand(1:4))...)
    push!(analyses, anly)
    @time add_obs!(fg, anly)
end
println(fg.DM)
println(fg.BB)
for rest in values(fg.CRP) println(rest.tables) end

println(last(analyses))
# for i in 1:100
#     @time sample(fg,1)
# end
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
