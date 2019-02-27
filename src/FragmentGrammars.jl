module FragmentGrammars

using GeneralizedChartParsing

include("CompoundDists.jl")
include("parse_a_tree.jl")

using .parse_a_tree
using .CompoundDists

mutable struct FragmentGrammar
    baseGrammar :: Grammar
    restaurants :: Dict{String,ChineseRest}
    pi :: Dict{Tuple{String, Int}, Int} = Dict()
    psi :: Dict{Int} = Dict()
end

function FragmentGrammar(
        g :: Grammar
    )
    
end

println(FragmentGrammar(parse_a_tree.g))
# println("test")

end
