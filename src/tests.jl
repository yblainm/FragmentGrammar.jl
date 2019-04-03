#########
# Tests #
#########
module tests

# include("catalan_test.jl")

# include("GeneralizedChartparsing\\src\\GeneralizedChartparsing.jl")
# using .GeneralizedChartparsing
# using .GeneralizedChartparsing: run_chartparser

include("FragmentGrammars.jl")
using .FragmentGrammars
using .FragmentGrammars: ApproxRule

# @show Vector{AbstractRule{String,String}}([BaseRule("S", ("S", "T"))]) |> typeof

# Test FG with base grammar:
# S -> S T | T S | T
# T -> a

println("-----start-----")
fg = FragmentGrammar(["S"], ["S"], [BaseRule("S", ("S", "T")), BaseRule("S", ("T", "S")), BaseRule("S", ("T",))], ["a"], [BaseRule("T", ("a",))], 0.2, 0.5)

# @show state_type(fg)
# @show startstate(fg)

# TODO:
# -When expanding a fragment's non-terminal, if we sample a stored fragment, it should be counted as a CRP observation even though its tree is subsumed under the current fragment. Currently this doesn't happen.
# -It doesn't look like fragment leaves are well bookkept.

# ---------- Parsing ------------
for i in 1:100
    add_obs!(fg, Analysis(sample(fg, "S")...))
end
# @show (run_chartparser(["a" for i in 1:10], fg) |> best_tree).data |> typeof
# Tuple{Main.tests.FragmentGrammars.GeneralizedChartparsing.Constituent{String, String, Main.tests.FragmentGrammars.AbstractRule{String, String}, Main.tests.FragmentGrammars.BaseRule{String, String},LogProbs.LogProb}, Main.tests.FragmentGrammars.ApproxRule{String, String}}

sampled_approx_tree = (run_chartparser(["a" for i in 1:10], fg) |> sample_tree)
for tr in sampled_approx_tree
    if tr.data[2] isa ApproxRule
        @show sample(tr.data[2]) # sample from ApproxRule
        break
    end
end

# --------- Observations --------
# for i in 1:5
#     println("-----New obs-----")
#     anal = Analysis(sample(fg, "S")...)
#     # println(anal)
#     add_obs!(fg, anal)
#     rm_obs!(fg, anal)
#     println(fg.startstate)
# end

# --------- Time tests ----------
# @time for i in 1:1 add_obs!(fg, Analysis(sample(fg, "S")...)) end

# println(Analysis(sample(fg,"S")...))

# @time for i in 1:100 add_obs!(fg, Analysis(sample(fg, "S")...)) end
#
# println(Analysis(sample(fg,"S")...))

# @time for i in 1:10000 add_obs!(fg, Analysis(sample(fg, "S")...)) end
#
# println(Analysis(sample(fg,"S")...))

# # for tr in fg.startstate.trans
# #     println(tr)
# # end
# #
# println("-----analysis-----")
# anal = Analysis(sample(fg, "S")...) # lol
# println(anal)
# println(anal)
#
# println("-----add-----")
# add_obs!(fg, anal)
#
# println("-----rm-----")
# rm_obs!(fg, anal)

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
