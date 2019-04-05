#########
# Tests #
#########
module tests

# include("catalan_test.jl")

# include("GeneralizedChartparsing\\src\\GeneralizedChartparsing.jl")
# using .GeneralizedChartparsing
# using .GeneralizedChartparsing: run_chartparser
# using LogProbs

include("FragmentGrammars.jl")
using .FragmentGrammars
using .FragmentGrammars: ApproxRule, update_approx_probs!

# TODO:
# -

# Test FG with base grammar:
# S -> S T | T S | T
# T -> a

println("-----start-----")
@time fg = FragmentGrammar(["S"], ["S"], [BaseRule("S", ("S", "T")), BaseRule("S", ("T", "S")), BaseRule("S", ("T",))], ["a"], [BaseRule("T", ("a",))], 0.2, 0.5)

# @show collect(fg.startstate)
# collect(fg.startstate)
# ---------- Parsing ------------
for j in 1:1
    for i in 1:10
       @time anal = Analysis(sample(fg, "S")...)
       @time add_obs!(fg, anal)
       # @time rm_obs!(fg, anal)
    end
    @time update_approx_probs!(fg)
    @time forest = run_chartparser(["a" for i in 1:10], fg)
    @time sampled_approx_tree = sample_tree(forest)
    @time approx_sampled_rule = sample(sampled_approx_tree.data[2]) # sample from ApproxRule. Seems like this method is sometimes extremely slow for some reason.
    # marg = zero(LogProb)
    # for state in fg.startstate
    #     for comp in state.comp
    #         if comp[1] == "S"
    #             marg += @show sum(r -> r[2], comp[2].rules)
    #         end
    #     end
    # end
    # @show marg
end

# @show fg
# @time for i in 1:1
#     anal = Analysis(sample(fg, "S")...)
#     add_obs!(fg, anal)
#     # println(fg.CRP)
#     println("----before----")
#     for state in fg.startstate
#         println("----new state----")
#         for comp in state.comp
#             println("$(comp[2].rules)")
#         end
#     end
#     rm_obs!(fg, anal)
#     # println(fg.CRP)
#     println("----after----")
#     for state in fg.startstate
#         println("----new state----")
#         for comp in state.comp
#             println("$(comp[2].rules)")
#         end
#     end
# end
# This is specifically for the parser + translating from approx PCFG to an analysis
# It should always be called before doing anything ApproxRule-related.
# for state in fg.startstate
#     for comp in state.comp
#         println(comp[2].rhs, length(comp[2].rules))
#     end
# end

# @time update_approx_probs!(fg)

# @time sampled_approx_tree = (run_chartparser(["a" for i in 1:10], fg) |> sample_tree)
# for tr in sampled_approx_tree
#     if tr.data[2] isa ApproxRule
#         for i in 1:10
#         @show sample(tr.data[2]) # sample from ApproxRule
#         end
#         break
#     end
# end

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
