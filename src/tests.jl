#########
# Tests #
#########
module tests
include("GeneralizedChartparsing\\src\\GeneralizedChartparsing.jl")
using .GeneralizedChartparsing
using .GeneralizedChartparsing: ContextFreeRule

# using Test
# g = Grammar([["0.5", "S", "S", "S"]], [["0.5", "S", "a"]], ["S"], BigInt)
# @show g.terminal_dict['a']

# include("catalan_test.jl")


# g = Grammar(
# """0.5 S S S""",
# """0.5 S a""",
# ["S"], LogProb)
#
# @show run_chartparser(["a" for i in 1:3], g) |> score

g = Grammar(
    [ContextFreeRule(1, (2,3)),
    ContextFreeRule(1, (1,1))],
    [ContextFreeRule(1, Tuple{Vararg{Int}}(4)),
    ContextFreeRule(2, Tuple{Vararg{Int}}(4)),
    ContextFreeRule(3, Tuple{Vararg{Int}}(4))],
    [1],
    LogProb
)

@show run_chartparser([4 for i in 1:10], g) |> score

# for n in 1:10
#     frst = run_chartparser(["a" for i in 1:n], g)
#     @show (score(frst), catalan_number(n-1))
# end
#
# @show catalan_test(30)

# using Profile # Lets you see how many backtraces (function calls) come from a specific code block during runtime. Good for finding bottlenecks.

end
