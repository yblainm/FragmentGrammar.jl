function read_treebank_grammar(number_sentences::Int, Score=LogProb)
    @assert number_sentences in (1, 5, 50, 500, 972)
    grammarfile = joinpath(dirname(@__FILE__), "WSJ_data", "WSJ.$number_sentences.grammar.txt")
    Grammar(readstring(grammarfile), ["START"], Score)
end

function compute_treebank_scores(number_sentences::Int, grammar=read_treebank_grammar(number_sentences))
    @assert number_sentences in (1, 5, 50, 500, 972)
    sentences = map(split, readlines(joinpath(dirname(@__FILE__), "WSJ_data", "WSJ.$number_sentences.sentences.txt")))
    scores = zeros(GeneralizedChartparsing.score_type(grammar), length(sentences))
    for (i,s) in enumerate(sentences)
        scores[i] = score(run_chartparser(s, grammar))
    end
    scores
end

function treebank_goldstandard_scores(number_sentences::Int)
    lines = readlines(joinpath(dirname(@__FILE__), "WSJ_data", "WSJ.$number_sentences.scores.txt"))
    map(parse, lines)
end
