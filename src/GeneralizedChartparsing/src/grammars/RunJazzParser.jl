# using FNVHash

include("..\\generalized-parser\\source\\LogProbs.jl")
include("..\\generalized-parser\\source\\ProductSemiRings.jl")
include("..\\chinese-restaurants\\ChineseRestaurants.jl")
# include("JazzGrammars3.jl")
include("JazzGrammars5.jl")


# include("..\\generalized-parser\\source\\ChartParser2.jl")
# include("..\\generalized-parser\\source\\ChartParser5.jl")
include("..\\generalized-parser\\source\\ChartParser6.jl")

include("read_goldstandard.jl")
include("..\\tree-annotation-tool\\treebank_functions.jl")

###################
### Simple Test ###
###################

grammar = make_jazz_grammar()
# grammar = make_plain_PCFG_jazz_grammar()
@time forest = run_chartparser(map(Terminal, ["0maj","1maj","7maj","0maj"]), grammar)
# @time forest = run_chartparser(map(Terminal, ["0maj","0maj","0maj"]), grammar)
println(score(forest))
println(best_tree(forest))

####################
### Catalan Test ###
####################

# chords(n::Int) = [Terminal("0maj") for i in 1:n]
# number_parses(n::Int) = score(run_chartparser(chords(n), grammar))
# catalan_number(n::Int) = div(factorial(BigInt(2*n)), factorial(BigInt(n+1)) * factorial(BigInt(n)))
#
# for n in 1:20
#     println(n, "\t", number_parses(n), "\t", catalan_number(n-1))
# end

######################
### Profiling Test ###
######################

# corpus_dir = "C:\\Users\\Daniel\\Documents\\Bitbucket\\musical-grammars\\data\\Jazz-selection\\"
# testfile = "'Round Midnight_id_05848_midicons.txt"
# testfile2 = "After You've Gone_id_06924_allanah.txt"
#
# function translate_corpus_notation(chord::AbstractString)
#     m = match(r"([A-G]b*#*)(.*)(_)", chord)
#     string(pitch_class(m[1]), m[2])
# end
#
# function read_chords(file::AbstractString)
#     chords = map(x->x[2], map(split, map(strip, readlines(file)[2:end-1])))
#     map(translate_corpus_notation, chords)
# end

# chords = read_chords(corpus_dir*testfile2)
# @time forest = run_chartparser(chords, grammar)
# println(score(forest))
#
# using ProfileView
# Profile.clear()
# @profile run_chartparser(chords, grammar)
# ProfileView.view()


# workspace()
