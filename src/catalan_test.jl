function catalan_test(n_max)
    # grammar = Grammar(["0.5 S S S"], ["0.5 S a"], ["S"], BigInt)
    for n in 1:n_max
        # println(n, "\t", number_parses(n, grammar), "\t", catalan_number(n-1))
        g = Grammar([["0.5", "S", "S", "S"]], [["0.5", "S", "a"]], ["S"], BigInt)
        # g = Grammar("0.5 S S S", "0.5 S a", ["S"], BigInt)
        nump = number_parses(n, g)
        catn = catalan_number(n-1)
        if nump != catn
            return false
        end
    end
    return true
end

number_parses(n::Int, grammar) = score(run_chartparser(["a" for i in 1:n], grammar))
catalan_number(n::Int) = div(factorial(BigInt(2*n)), factorial(BigInt(n+1)) * factorial(BigInt(n)))
