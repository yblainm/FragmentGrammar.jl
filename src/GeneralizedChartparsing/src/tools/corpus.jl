# Tools for processing data from the Universal Dependencies corpus
# format CoNLL-U
# http://universaldependencies.org/format.html
#
# NOTE I am not including the whole corpus in this repo, because it is big.
# Instead these scripts will access a Julia file src/tools/corpus_data.jl
# which specifies the following string constants:
# CORPUS_DIR (full path to directory of the corpus)
# CORPUS_DEV (name of dev set)
# CORPUS_TEST (name of test set)
# CORPUS_TRAIN (name of training set)
# This file is not tracked by git (it is in the .gitignore), because each
# person will have to set the contents of this file differently.
# You need only to put this file in the specified location and define the
# constants therein for this code to work.
#
# The UD corpora can be downloaded from the website: http://universaldependencies.org/
# The English corpus is here: https://github.com/UniversalDependencies/UD_English
#
# -chris

include("corpus_data.jl")


function get_filename(set_name :: String)
    if set_name == "dev"
        joinpath(CORPUS_DIR, CORPUS_DEV)
    elseif set_name == "test"
        joinpath(CORPUS_DIR, CORPUS_TEST)
    elseif set_name == "train"
        joinpath(CORPUS_DIR, CORPUS_TRAIN)
    else
        error("set_name argument must be \"dev\", \"test\", or \"train\"")
    end
end

type CoNLLUWord
    id       :: Int
    form     :: String
    lemma    :: String
    upos_tag :: String
    xpos_tag :: String
    feats    :: Vector{Tuple{String, Union{Int, AbstractString}}}
    head     :: Int
    dep_rel  :: String
    deps     :: Vector{Tuple{Int, AbstractString}}
    misc     :: String
end
id(cword :: CoNLLUWord) = csent.id
form(cword :: CoNLLUWord) = csent.form
lemma(cword :: CoNLLUWord) = csent.lemma
upos_tag(cword :: CoNLLUWord) = csent.upos_tag
xpos_tag(cword :: CoNLLUWord) = csent.xpos_tag
feats(cword :: CoNLLUWord) = csent.feats
head(cword :: CoNLLUWord) = csent.head
dep_rel(cword :: CoNLLUWord) = csent.dep_rel
deps(cword :: CoNLLUWord) = csent.deps
misc(cword :: CoNLLUWord) = csent.misc

type CoNLLUSentence
    sent_id :: String
    text :: Vector{String}
    words :: Vector{CoNLLUWord}
end
sent_id(csent :: CoNLLUSentence) = csent.sent_id
text(csent :: CoNLLUSentence) = csent.text
words(csent :: CoNLLUSentence) = csent.words
CoNLLUSentence() = CoNLLUSentence("", [], [])

type CoNLLUCorpus
    doc_id :: String
    sents :: Vector{CoNLLUSentence}
end
doc_id(ccorpus :: CoNLLUCorpus) = ccorpus.doc_id
sents(ccorpus :: CoNLLUCorpus) = ccorpus.sents

function read_corpus(set_name :: String)
    filename = get_filename(set_name)
    doc_id = ""
    sents = Vector{CoNLLUSentence}()
    bad_sent_indices = Vector{Int}() # for sentences that give errors when parsing

    # helper functions used to parse some of the fields
    parse_field(s) = s == "_" ? "" : s
    function parse_feats(s)
        if s == "_"
            Vector{Tuple{String, Union{Int, String}}}()
        else
            map(featpair -> all(isnumber, featpair[2]) ? (featpair[1], parse(featpair[2])) : featpair,
                map(Tuple,
                    map(s -> split(s, '='),
                        split(s, '|'))))
        end
    end
    function parse_deps(s)
        if s == "_"
            Vector{Tuple{Int, String}}()
        else
            map(deppair -> (parse(deppair[1]), deppair[2]),
                map(Tuple,
                    map(s -> split(s, ':'),
                        split(s, '|'))))
        end
    end

    sent = CoNLLUSentence()
    bad_sent = false
    open(filename) do f
        for l in eachline(f)
            fields = split(l, ' ')
            # last line of a sentence
            if l == ""
                if !bad_sent
                    push!(sents, sent)
                end
                sent = CoNLLUSentence()
                bad_sent = false
            # first line of the corpus
            elseif length(fields) == 5 && fields[2] == "newdoc"
                doc_id = fields[5]
            # sent_id line of a sentence
            elseif length(fields) == 4 && fields[2] == "sent_id"
                sent.sent_id = fields[4]
            # text line of a sentence
            elseif length(fields) >= 4 && fields[2] == "text"
                sent.text = fields[4:end]
            # middle lines of sentence
            else
                sentfields = split(fields[1], '\t')
                try
                    word = CoNLLUWord(
                        parse(sentfields[1]),
                        parse_field(sentfields[2]),
                        parse_field(sentfields[3]),
                        parse_field(sentfields[4]),
                        parse_field(sentfields[5]),
                        parse_feats(sentfields[6]),
                        parse(sentfields[7]),
                        parse_field(sentfields[8]),
                        parse_deps(sentfields[9]),
                        parse_field(sentfields[10])
                        )
                    push!(sent.words, word)
                catch e
                    warn("Failed to process sent_id $(sent.sent_id) at word $(sentfields[1])")
                    warn("Threw error $e")
                    bad_sent = true
                end
            end
        end
    end
    CoNLLUCorpus(doc_id, sents)
end

"""Returns a collection over sentences. Each sentences is represented as a vector of strings"""
function read_sentences(set_name :: String)
    filename = get_filename(set_name)
    sents = []
    open(filename) do f
        for l in eachline(f)
            if l != ""
                fields = split(l, ' ')
                if length(fields) >= 4 && fields[2] == "text"
                    push!(sents, fields[4:end])
                end
            end
        end
    end
    return sents
end

get_sentences(corpus :: CoNLLUCorpus) = [[word.form for word in csent.words]
                                         for csent in corpus.sents]

"""
Construct a MinimalistGrammar from the dependencies in the provided corpus.
If probdist is "ml" then the probability dist. is estimated according to maximum likelihood.
If probdist is "uniform" then the prob. dist. is a Dirichlet with uniform counts (DirCatVar).
"""
function MinimalistGrammar(corpus :: CoNLLUCorpus, probdist="ml", max_selectors=0)
    lexicon = Dict{String, Vector{LexicalItem{String}}}()
    starts = Vector{Vector{Feature}}()
    counts = Dict{Feature, Dict{String, Int}}()
    function add_lexical_item!(lexicon, li)
        if haskey(lexicon, phon(li))
            if !(li in lexicon[phon(li)])
                push!(lexicon[phon(li)], li)
            end
        else
            lexicon[phon(li)] = [li]
        end
    end
    function add_count!(counts, li)
        cat = catfeature(features(li))
        if haskey(counts, cat)
            if haskey(counts[cat], id(li))
                counts[cat][id(li)] += 1
            else
                counts[cat][id(li)] = 1
            end
        else
            counts[cat] = Dict(id(li) => 1)
        end
    end

    for csent in corpus.sents
        for cword in csent.words
            try
                deps = [cw.id for cw in csent.words if cw.head == cword.id]
                lfeats = reverse(["L=$(csent.words[i].upos_tag)" for i in deps if i < cword.id && i != 0])
                rfeats = ["R=$(csent.words[i].upos_tag)" for i in deps if i > cword.id && i != 0]
                # oddly, these prev. 2 lines sometimes fail, because sometimes the index i is beyond bounds of the list
                # must be an error in the corpus files

                if !isempty([lfeats; rfeats])
                    li = LexicalItem(cword.form, [lfeats; rfeats; cword.upos_tag])
                    if max_selectors > 0 && length(li.features) - 1 <= max_selectors
                        add_lexical_item!(lexicon, li)
                        add_count!(counts, li)
                    end
                end

                li = LexicalItem(cword.form, [cword.upos_tag])
                add_lexical_item!(lexicon, li)
                add_count!(counts, li)

                if cword.head == 0 && !([cword.upos_tag] in starts)
                    push!(starts, [cword.upos_tag])
                end
            end
        end
    end

    lexitem_cond = if probdist == "ml"
        SimpleCond(Dict(
            cat => CatDist(Dict(
                id => LogProb(counts[cat][id]/sum(values(counts[cat]))) 
                for id in keys(counts[cat])) )
            for cat in keys(counts)))
    else
        SimpleCond(Dict(cat => DirCatVar(Dict(id => 1.0 for id in keys(counts[cat])))
                        for cat in keys(counts)))
    end
    MinimalistGrammar(lexicon, lexitem_cond, starts)
end

"""Converts the dependency structure in a CoNLLUSentence into a DiGraph"""
function conlludeps_to_graph(csent :: CoNLLUSentence, directed=true)
    g = directed ? DiGraph(length(csent.words)) : Graph(length(csent.words))
    for (i, cword) in enumerate(csent.words)
        add_edge!(g, cword.head, i)
    end
    g
end

precision(proposed_deps :: AbstractGraph, gold_deps :: AbstractGraph) = 
    Float64(length(intersect(edges(proposed_deps), edges(gold_deps))) / length(edges(proposed_deps)))
recall(proposed_deps :: AbstractGraph, gold_deps :: AbstractGraph) = 
    Float64(length(intersect(edges(proposed_deps), edges(gold_deps))) / length(edges(gold_deps)))

# TODO do the same for undirected graphs
"Returns a a 2-Tuple of Float64s (precision, recall)"
function evaluate_dependencies(depgraph :: AbstractGraph, csent :: CoNLLUSentence)
    @assert length(vertices(depgraph)) == length(csent.words)
    goldgraph = conlludeps_to_graph(csent, is_directed(depgraph))
    prec = length(edges(depgraph)) > 0 ? precision(depgraph, goldgraph) : 1.0
    rec = length(edges(goldgraph)) > 0 ? recall(depgraph, goldgraph) : 1.0
    prec, rec
end

"""
Given a corpus, create all the possible MG lexical items using the POS tags in the corpus, with up to 2 selectors. So, given a word W, we generate lexical items
    W :: ({R,L}=X) ({R,L}=Y) Z
for all POS tags X,Y,Z.
Returns tuple (lexicon, startsymbols)
"""
function make_lexicon_from_corpus_all_combinations(corpus :: CoNLLUCorpus, cats_type="pos", k=2)
    words = unique(cw.form for cs in corpus.sents for cw in cs.words)
    cats = if cats_type == "pos"
        unique(cw.upos_tag for cs in corpus.sents for cw in cs.words)
    elseif cats_type == "minimalist"
        ["n", "v", "adj", "adv", "d", "i"]
    end
    make_lex_given_cat(c) = if k == 1
        [[LexicalItem(w, [c]) for w in words];
         [LexicalItem(w, ["R=$c1", "$c"]) for w in words for c1 in cats];
         [LexicalItem(w, ["L=$c1", "$c"]) for w in words for c1 in cats]]
    elseif k == 2
        [[LexicalItem(w, [c]) for w in words];
         [LexicalItem(w, ["R=$c1", "$c"]) for w in words for c1 in cats];
         [LexicalItem(w, ["L=$c1", "$c"]) for w in words for c1 in cats];
         [LexicalItem(w, ["R=$c2", "R=$c1", "$c"]) for w in words 
                                                   for c1 in cats
                                                   for c2 in cats];
         [LexicalItem(w, ["L=$c2", "L=$c1", "$c"]) for w in words 
                                                   for c1 in cats
                                                   for c2 in cats];
         [LexicalItem(w, ["L=$c2", "R=$c1", "$c"]) for w in words 
                                             for c1 in cats
                                             for c2 in cats]]
    else
        error("only k<=2 supported")
    end
    lexicons = pmap(make_lex_given_cat, cats)
    lexicon = vcat(lexicons...)
    lexicon, [[c] for c in cats]
end

function unkify!(train_corpus :: CoNLLUCorpus, test_corpus :: CoNLLUCorpus)
    words = Set(w.form for sent in train_corpus.sents for w in sent.words)
    for sent in test_corpus.sents, word in sent.words
        if !(word.form in words)
            word.form = "UNK"
        end
    end
end

function unkify!(grammar :: MinimalistGrammar, corpus :: CoNLLUCorpus)
    for sent in corpus.sents, word in sent.words
        if !(haskey(grammar.lexicon, word.form))
            word.form = "UNK"
        end
    end
end


function parse_given_dependencies(grammar, csent :: CoNLLUSentence, binary_branching=true)
    s = [w.form for w in csent.words]
    g = conlludeps_to_graph(csent, false)
    m = graph_to_dependency_matrix(g)
    if binary_branching
        m = binarize_dependency_matrix(m)
    end
    run_chartparser(s, grammar, m)
end

function corpus_likelihood(forests, epsilon=false, parallel=true)
    score_forest(f) = length(heads(f)) == 0 ? LogProb(0.0) : score(f)
    scores = parallel ? pmap(score_forest, forests) : map(score_forest, forests)
    prod(scores)
end

function kullback_leibler(g :: MinimalistGrammar,
                          h :: MinimalistGrammar)
    # this calculates one term in the KL summation for a single lexical item (lexid)
    klterm(p, q, lexid) = exp(logscore(p, lexid).value) * (logscore(p, lexid).value / 
                                                           logscore(q, lexid).value)
    # this sums f over the lexids in the given conditional distributions
    klsum(cond1, cond2, cat) = sum(map(tup -> klterm(tup...),
                                   [(cond1.dists[cat], cond2.dists[cat], lexid)
                                    for lexid in support(cond1.dists[cat])]))
    # finally, we sum g over all the conditional distributions
    cats = keys(g.lexitem_cond.dists)
    sum(map(tup -> klsum(tup...), [(g.lexitem_cond, h.lexitem_cond, cat) 
                                   for cat in cats]))
end

function iscrossing(dep1 :: Tuple{Int,Int}, dep2 :: Tuple{Int,Int})
    i = min(dep1...); j = max(dep1...)
    k = min(dep2...); l = max(dep2...)
    (i < k < j && j < l) || (k < i && i < l < j)
end

"O(n^2)"
function has_crossing_dependencies(cs :: CoNLLUSentence)
    deps = [(i, cw.head) for (i, cw) in enumerate(cs.words)]
    filter!(tup -> tup[1] != 0 && tup[2] != 0, deps)
    any(iscrossing((i, j), (k, l)) for (i, j) in deps, (k, l) in deps)
end

"""
memoized haspath function. provide an n*n mutable matrix (n = |V|) in the store argument
whose arguments are initialized to -1.
g must not have cycles (otherwise this will go into an infinite loop
runs at most n*n steps if nothing is memoized.
"""
function haspath(g :: AbstractGraph{Int}, 
                 i :: Int, 
                 j :: Int, 
                 store :: Matrix{Int} = Matrix{Int}(0,0),
                 start_i=i)
    n = length(vertices(g))
    if start_i == i # found a cycle
        false
    elseif size(store) != (n, n) || any(x -> !(x in [1, -1, 0]), store)
        error("store does not have the correct specifications")
    end

    if has_edge(g, i, j) || store[i, j] == 1
        true
    else
        store[i, j] = any(k -> haspath(g, k, j, store, start_i), outneighbors(g, i))
    end
end

"""
A dependency i --> j is contiguous iff for every k, i < k < j, there is a path
i --> ... --> k.
A dependency graph over a sentence is non-contiguous iff there exists a dependency which is not contiguous.
I think this is O(n^2) (would be O(n^4) if it weren't for memoization)
"""
function has_noncontiguous_dependencies(cs :: CoNLLUSentence)
    n = length(cs.words)
    g = conlludeps_to_graph(cs)

    store = [-1 for i in 1:n, j in 1:n]
    isnoncontiguous(i, j) = any(k -> !haspath(g, i, k, store), i+1:j-1)
    any(tup -> isnoncontiguous(tup[1], tup[2]),
        [(edge.src, edge.dst) for edge in edges(g)])
end


function tikzdependency(cs :: CoNLLUSentence)
    s1 = """
    \\usepackage{tikz-dependency}
    \\begin{document}
    \\begin{dependency}[theme = simple]
    """
    s2 = join([cw.form for cw in cs.words], " \\& ")
    s3 = """\\begin{deptext}[column sep=1em] 
            $s2 \\\\
            \\end{deptext}"""
    deplines = String[]
    for (i, cw) in enumerate(cs.words)
        h = cw.head
        if h == 0
            push!(deplines, "\\deproot{$i}{ROOT}")
        else
            # dep h --> i
            push!(deplines, "\\depedge{$h}{$i}{$(cw.dep_rel)}")
        end
    end
    s4 = join(deplines, "\n")
    s5 = "\\end{dependency}\n\\end{document}"
    "$s1\n$s3\n$s4\n$s5"
end


########################################
## evaluation functions ################
## previously in test/corpus_tests.jl ##
########################################

## methods for parsing and calculating metrics.
## each method after the first one is a batch methods, and calls previous methods

"Parse the CoNLLU sentence and return a tuple whose components are
    1. (precision, recall) on the unlabelled directed dependency arcs.
    2. number of parses
    3. probability of map parse
If the sentence is unparseable, all these values are zero"
function parse_and_evaluate_sentence(mg :: MinimalistGrammar, 
                                     csent :: CoNLLUSentence, 
                                     epsilon :: Bool,
                                     transitive :: Bool,
                                     directed :: Bool)
    sent = [w.form for w in csent.words]
    forest = if epsilon
        run_chartparser(sent, mg, epsilon="")
    else
        run_chartparser(sent, mg)
    end
    if is_complete(forest)
        tree = best_tree(forest)
        deps = dependencies(tree, closed_under_transitivity=transitive, directed=directed)
        evaluate_dependencies(deps, csent), length(forest.heads), maximum(map(prob, forest.heads))
    else
        (0.0, 0.0), 0, LogProb(0)
    end
end


"""
Parse the corpus in batches of size k, calculate unlabelled dependency arc recall.
Returns a tuple consisting of the following components:
    1. mean (precision, recall) [given MAP parse of each sentence]
    2. (min, mean, max) number of parses per sentence
    3. (min, mean, max) probability of MAP parses
    4. number of sentences parsed
"""
function parse_and_evaluate_corpus(mg :: MinimalistGrammar, 
                                   corpus :: CoNLLUCorpus, 
                                   epsilon :: Bool, 
                                   transitive :: Bool,
                                   directed :: Bool,
                                   parallel=true)
    print("parse and evaluate $(length(corpus.sents)) sentences ")
    println("$(!parallel ? "non-" : "")parallel.")
    f(cs) = parse_and_evaluate_sentence(mg, cs, epsilon, transitive, directed)
    @time values = parallel ? pmap(f, corpus.sents) : map(f, corpus.sents)
    scores, parses, probs = [[triple[i] for triple in values] for i in 1:3]
    prec_mean = mean([pair[1] for pair in scores])
    rec_mean = mean([pair[2] for pair in scores])
    parses_stats = minimum(parses), mean(parses), maximum(parses)
    probs_stats = minimum(probs), mean(probs), maximum(probs)
    num_parsed = sum([n > 0 for n in parses])
    (prec_mean, rec_mean), parses_stats, probs_stats, num_parsed
end

#############################
## compactness evaluations ##
#############################

"Sort lexical items in minimalist grammar within each word (phon) by the probability assigned to lexical item. Highest probability first."
function sort!(mg :: MinimalistGrammar)
    map(lis -> sort!(lis, by = li -> prob(mg, li), lt = (x,y) -> x.value < y.value, rev=true),
        values(mg.lexicon))
end

"Get a portion of the minimalist grammar only including lexical items up to and including the given rank. Assumes the grammar is sorted"
function uptorank(mg :: MinimalistGrammar, rank :: Int)
    sublist(l, i, j) = j > length(l) ? l[i:end] : l[i:j]
    new_lexicon = Dict(word => sublist(mg.lexicon[word], 1, rank) for word in keys(mg.lexicon))
    new_lexitem_cond = deepcopy(mg.lexitem_cond)
    for cat in keys(mg.lexitem_cond.dists)
        dist = mg.lexitem_cond.dists[cat]
        new_dist = new_lexitem_cond.dists[cat]
        for lexid in support(dist)
            lexitem = LexicalItem(lexid)
            word = lexitem.phon
            if !(lexitem in new_lexicon[word])
                if isa(dist, DirCatVar)
                    delete!(new_dist.counts, lexid)
                else
                    delete!(new_dist.probs, lexid)
                end
            end
        end
        if isa(new_dist, CatDist)
            norm_const = sum(values(new_dist.probs))
            for lexid in keys(new_dist.probs)
                new_dist.probs[lexid] = new_dist.probs[lexid] / norm_const
            end
        end
    end
    MinimalistGrammar(new_lexicon, new_lexitem_cond, mg.start_symbols)
end

"Returns (as a dict): cumulative counts of unique lexical items, average # of nodes per tree, coverage of the corpus.
Uses best tree of each forest, if forest is complete."
function compactness_metrics(forests)
    count_unique_lexical_items(trees) = length(unique(vcat(map(lexids, trees)...)))
    count_nodes(trees, parallel=true) = parallel ? sum(pmap(length, trees)) : sum(map(length, trees))
    n = length(forests)
    filter!(is_complete, forests)
    trees = pmap(best_tree, forests)
    @time cumu_counts = count_unique_lexical_items(trees)
    println("cumu counts: $cumu_counts")
    @time avg_nodes = count_nodes(trees) / length(forests)
    println("avg nodes:   $avg_nodes")
    @time coverage = length(forests) / n
    println("coverage:    $coverage")
    Dict("cumu_counts" => cumu_counts,
         "avg_nodes"   => avg_nodes,
         "coverage"    => coverage)
end

"Returns compactness metrics over all ranks up to max_rank, return type is Dict{String, Vector}, a dictionary from metric names to a list of the given metric over all ranks"
function compare_compactness(mg_1, mg_2, max_rank, corpus, ϵ_1 :: Bool, ϵ_2 :: Bool)
    sort!(mg_1)
    sort!(mg_2)
    function metrics_for_rank(r)
        println("metrics for rank $r")
        println("-------------------")
        println("construct mgs")
        @time mg_1_r = uptorank(mg_1, r)
        @time mg_2_r = uptorank(mg_2, r)
        flush(STDOUT); println("parse corpus")
        parse_1(s) = ϵ_1 ? run_chartparser(s, mg_1_r, epsilon="") : run_chartparser(s, mg_1_r)
        parse_2(s) = ϵ_2 ? run_chartparser(s, mg_2_r, epsilon="") : run_chartparser(s, mg_2_r)
        @time fs_1 = pmap(parse_1, corpus)
        @time fs_2 = pmap(parse_2, corpus)
        flush(STDOUT); println("compute metrics")
        @time m1, m2 = compactness_metrics(fs_1), compactness_metrics(fs_2)
        flush(STDOUT)
        m1, m2
    end
    metrics_dicts = pmap(metrics_for_rank, 1:max_rank)
    field_names = keys(metrics_dicts[1][1])
    Dict(field => [metrics_dicts[r][i][field] for r in 1:max_rank, i in 1:2] for field in field_names)
end
    

########################
## EVALUATION SCRIPTS ##
########################

function parse_and_show_results(mg, 
                                corpus, 
                                epsilon=true, 
                                transitive=false, 
                                directed=true, 
                                parallel=true)
    corpus_size = length(corpus.sents)
    mean_scores, n_parses, p_parses, n_parsed = parse_and_evaluate_corpus(mg, corpus, epsilon, transitive, directed, parallel)
    println("RESULTS")
    println("=======")
    println("mean dependency precision: $(mean_scores[1])")
    println("mean dependency recall: $(mean_scores[2])")
    println("number of parses per sentence (min, mean, max): $(n_parses)")
    println("probabilities of MAP parses (min, mean, max): $(p_parses)")
    println("total number of sentences parsed: $n_parsed out of $corpus_size")
end

function vblearn_parse_and_show_results(
        mg :: MinimalistGrammar,
        train_corpus :: CoNLLUCorpus,
        test_corpus :: CoNLLUCorpus,
        cores :: Int,
        iterations :: Int,
        result_dir :: String,
        epsilon=true, # whether the grammar contains null heads
        transitive=false, # whether dependencies are transitive
        directed=true, # whether dependencies are directed
        saveprogress=true,
        exact_counts=true,
        parallel=true,
        samples_multiplier=0.5)

    println("unkify grammar and corpora")
    unkify!(mg, train_corpus)
    unkify!(mg, test_corpus)
    unkify!(mg)

    flush(STDOUT)

    println("running variational inference.")
    prev_counts = Dict()
    corpus = [[w.form for w in s.words] for s in test_corpus.sents]
    params = (iterations, prev_counts, mg, corpus, cores, result_dir, epsilon, transitive, directed, saveprogress, exact_counts, samples_multiplier)
    Profile.init(n=100000000)
    @time trained_mg = if parallel
        variational_inference_full_corpus_parallel(params...)
    else
        variational_inference_full_corpus(params...)
    end

    println("save trained grammar")
    open("$result_dir.mg", "w") do f
        write(f, GeneralizedChartparsing.show_counts(trained_mg))
    end

    println("evaluating on test set"); flush(STDOUT)
    @time parse_and_show_results(trained_mg, test_corpus, epsilon, transitive, directed, parallel)

    trained_mg
end


