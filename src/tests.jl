#########
# Tests #
#########
module tests

# include("catalan_test.jl")

# include("GeneralizedChartparsing\\src\\GeneralizedChartparsing.jl")
# using .GeneralizedChartparsing
# using .GeneralizedChartparsing: ContextFreeRule

include("FragmentGrammars.jl")
using .FragmentGrammars
using .FragmentGrammars: category_rule_type

# @show Vector{AbstractRule{String,String}}([BaseRule("S", ("S", "T"))]) |> typeof

# Test FG with base grammar:
# S -> S T | T S | T
# T -> a

fg = FragmentGrammar(["S"], ["S"], [BaseRule("S", ("S", "T")), BaseRule("S", ("T", "S")), BaseRule("S", ("T",))], ["a"], [BaseRule("T", ("a",))])

for tr in fg.startstate.trans
    println(tr)
end

@show anal = Analysis(sample(fg, "S")...) # lol

add_obs!(fg, anal)
for tr in fg.startstate.trans
    println(tr)
end
@show "--rm--"
rm_obs!(fg, anal)
for tr in fg.startstate.trans
    println(tr)
end

# NOTE: The grammar below doesn't work. Always have unique terminal rules.
# Fragments must end with preterminals.
# g = Grammar(
# """0.5 S S a""",
# """0.5 S a""",
# ["S"], LogProb)

# NOTE This one works, for example.
# g = Grammar(
# """0.5 S S T
# 0.5 S T""",
# """1.0 T a""",
# ["S"], LogProb)
#
# @show run_chartparser(["a" for i in 1:4], g) |> best_tree |> leafs

# @show ContextFreeRule("S", ("S", "T")) == ContextFreeRule("S", ("S", "T"))
# @show ContextFreeRule("S", ("S", "T")) === ContextFreeRule("S", ("S", "T"))

# a = ContextFreeRule("S", ("S", "T"))
# @show a("test") |> typeof

# using Profile # Lets you see how many backtraces (function calls) come from a specific code block during runtime. Good for finding bottlenecks.

end
