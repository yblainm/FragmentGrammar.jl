function catalan_test(n_max)
    # grammar = Grammar(["0.5 S S S"], ["0.5 S a"], ["S"], BigInt)
    for n in 1:n_max
        # println(n, "\t", number_parses(n, grammar), "\t", catalan_number(n-1))
        # @show g = Grammar([["0.5", "S", "S", "S"]], [["0.5", "S", "a"]], ["S"], BigInt)
        g = Grammar("0.5 S S S", "0.5 S a", ["S"], BigInt)
        @show [(typeof(i), i) for i in keys(g.terminal_dict)]
        # @show g.terminal_dict["a"]
        # @show terminal_rule_type(g)
        if number_parses(n, g) != catalan_number(n-1)
            return false
        end
    end
    return true
end

input = Union{String, SubString{String}}["a" for i in 1:10]
@show input
@show [(typeof(i), i) for i in input]
number_parses(n::Int, grammar) = score(run_chartparser(Union{String, SubString{String}}["a" for i in 1:n], grammar))
catalan_number(n::Int) = div(factorial(BigInt(2*n)), factorial(BigInt(n+1)) * factorial(BigInt(n)))
