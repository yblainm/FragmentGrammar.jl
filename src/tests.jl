#########
# Tests #
#########
module tests
include("GeneralizedChartparsing\\src\\GeneralizedChartparsing.jl")
using .GeneralizedChartparsing
# using Test
# g = Grammar([["0.5", "S", "S", "S"]], [["0.5", "S", "a"]], ["S"], BigInt)
# @show g.terminal_dict['a']

include("catalan_test.jl")
catalan_test(30)

# using Profile # Lets you see how many backtraces (function calls) come from a specific code block during runtime. Good for finding bottlenecks.

end
