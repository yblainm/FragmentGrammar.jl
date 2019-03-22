

# Dict (lexicalItem, count) to keep track of previous expected counts for vb_update
#type PrevCounts
#    prev_counts :: Dict{ Vector{T} , Dict{LexicalItem{T}, Float64}}
#end
#function PrevCounts()
#    prev_counts
#    PrevCounts(dict())
#end

#function getcatfeat(lex :: LexicalItem{T})
#    return lex.features[length(features)]
#end

function vb_update(counts, prev_counts, grammar)
    # for each lexical item with new expected count
    for (s, dict) in counts
        if !(s in keys(prev_counts))
            prev_counts[s] = Dict()
        end
        for (id, p) in dict
            #get previous count if subtracting it to new count
            prev=0.0
            if id in keys(prev_counts[s])
                prev = prev_counts[s][id]
            end
            #update previous counts with new counts for next iterations
            prev_counts[s][id] = p
            #get category of idical item (will need to change this line when movement is introduced)
            cat = catfeature(features(LexicalItem(id)))
            # update count in dirichlet categorical which will automatically update idical items score
            grammar.lexitem_cond.dists[cat].counts[id] += (p - prev)
        end
    end
    return (prev_counts, grammar)
end


function variational_inference(subset_size, corpus_size, iterations, prev_counts, grammar, corpus, epsilon=true)
    # iterate as many times as desired for updates
    for i in (1:iterations)

        # random sample of subset_size (eg. 20) sentences from corpus called subset
        subset_ids = rand(1:corpus_size, subset_size)
        subset = corpus[subset_ids]
        # Get expected counts for all lexical items in parses of subset
        counts = expected_lexitem_counts_by_sentence(subset, grammar, epsilon)
        # variational Bayesian update given counts
        prev_counts, grammar = vb_update(counts, prev_counts, grammar)

        # write a safety function that saves parameters every so many iterations in case computer crashes
        n = string(i)
        name = string(n,"_MG.txt")
        open(name, "w") do f
            for cat in keys(grammar.lexitem_cond.dists)
                for id in keys(grammar.lexitem_cond.dists[cat].counts)
                    item = grammar.lexitem_cond.dists[cat].counts[id]
                    lex = LexicalItem(id)
                    write(f, "$(lex.phon) ::")
                    map(x -> write(f, " $x"), lex.features)
                    prob = string(grammar.lexitem_cond.dists[cat].counts[id])
                    write(f, " ; $prob")
                    write(f, "\n")
                end
            end
        end
    end
    return grammar
end

##################################
###### FULL CORPUS 1 CORE   ######
##################################

function variational_inference_full_corpus(iterations, prev_counts, grammar, corpus, result_dir, epsilon=true)
    # iterate as many times as desired for updates
    for i in (1:iterations)

        # Get expected counts for all lexical items in parses of corpus
        counts = expected_lexitem_counts_by_sentence(corpus, grammar, epsilon)
        # variational Bayesian update given counts
        prev_counts, grammar = vb_update(counts, prev_counts, grammar)

        # write a safety function that saves parameters every so many iterations in case computer crashes
        n = string(i)
        name = string(result_dir,"/", n,"_MG.txt")
        open(name, "w") do f
            for cat in keys(grammar.lexitem_cond.dists)
                for id in keys(grammar.lexitem_cond.dists[cat].counts)
                    item = grammar.lexitem_cond.dists[cat].counts[id]
                    write(f, id)
                    prob = string(grammar.lexitem_cond.dists[cat].counts[id])
                    write(f, " ; $prob")
                    write(f, "\n")
                end
            end
        end
    end
    return grammar
end




##################################
###### PARALLEL COMPUTATION ######
##################################

"give ranges to partition an array of lengh nb_data into subarrays of length nb_data_per_chunk https://stackoverflow.com/a/43767567"
function partition_array_indices(nb_data::Int, nb_data_per_chunk::Int)
    nb_chunks = ceil(Int, nb_data / nb_data_per_chunk)
    ids = UnitRange{Int}[]
    for which_chunk = 1:nb_chunks
        id_start::Int = 1 + nb_data_per_chunk * (which_chunk - 1)
        id_end::Int = id_start - 1 + nb_data_per_chunk
        if id_end > nb_data
            id_end = nb_data
        end
        push!(ids, id_start:id_end)
    end
    return ids
end

## non-parallel version
#function parse_and_calculate(grammar, corpus :: Vector{Vector{String}}, epsilon :: Bool, exact_counts :: Bool, samples_multiplier=0.5 :: Float64)
#    counts_by_sent = Dict{Vector{String}, Dict{String, Float64}}() # Dict sent => Dict lexid => float exp. count
#    cl = LogProb(1.0) # corpus likelihood
#
#    function p_c(s::Vector{String})
#        f = epsilon ? run_chartparser(s, grammar, epsilon="") : run_chartparser(s, grammar)
#        c = exact_counts ? expected_lexitem_counts(f, grammar, epsilon) : sample_counts(f, grammar, epsilon, true, samples_multiplier) :: Dict{String, Float64}
#        l = length(heads(f)) == 0 ? LogProb(0.0) : score(f)
#        l, c
#    end
#
#    for (j, (l, c)) in enumerate(map(p_c, corpus))
#        cl = cl * l
#        counts_by_sent[corpus[j]] = c
#    end
#    (cl, counts_by_sent)
#end

function parse_and_calculate(grammar, corpus :: Vector{Vector{String}}, epsilon :: Bool, exact_counts :: Bool, cores :: Int, samples_multiplier=0.5 :: Float64)
    counts_by_sent = Dict{Vector{String}, Dict{String, Float64}}() # Dict sent => Dict lexid => float exp. count
    cl = LogProb(1.0) # corpus likelihood

    function p_c(s::Vector{String})
        f = epsilon ? run_chartparser(s, grammar, epsilon="") : run_chartparser(s, grammar)
        c = exact_counts ? expected_lexitem_counts(f, grammar, epsilon) : sample_counts(f, length(s), grammar, epsilon, true, samples_multiplier) :: Dict{String, Float64}
        l = length(heads(f)) == 0 ? LogProb(0.0) : score(f)
        l, c
    end

    println("parse and calculate for each sentence")
    @time likelihoods_counts = pmap(p_c, corpus)
    flush(STDOUT); println("assemble calculations")
    @time cl = prod(c_l[1] for c_l in likelihoods_counts)
    #@time counts_by_sent = merge((c_l[2] for c_l in likelihoods_counts)...)
    @time counts_by_sent = Dict(corpus[i] => likelihoods_counts[i][2] for i in 1:length(corpus))
    flush(STDOUT); println("finished parse and calculate step"); flush(STDOUT)
    (cl, counts_by_sent)
end

## parallel version, spawns non parallel version on each core (simultaneously)
#function parse_and_calculate(grammar, corpus :: Vector{Vector{String}}, epsilon :: Bool, exact_counts :: Bool, cores :: Int, samples_multiplier=0.5 :: Float64)
#    counts_by_sent = Dict() # Dict sent => Dict lexid => float exp. count
#    cl = LogProb(1.0) # corpus likelihood
#    batch_ranges = partition_array_indices(length(corpus), Int(ceil(length(corpus)/cores)))
#    batches = [corpus[r] for r in batch_ranges]
#    p_c(b) = parse_and_calculate(grammar, b, epsilon, exact_counts, samples_multiplier)
#    counts_likelihoods = pmap(p_c, batches)
#    cl = prod(c_l[1] for c_l in counts_likelihoods)
#    counts_by_sent = merge((c_l[2] for c_l in counts_likelihoods)...)
#    println("finished parse and calculate step")
#    (cl, counts_by_sent)
#end



function variational_inference_full_corpus_parallel(iterations,
                                                    prev_counts,
                                                    grammar,
                                                    corpus,
                                                    cores,
                                                    result_dir,
                                                    epsilon=true,
                                                    transitive=true,
                                                    directed=true,
                                                    saveprogress=true,
                                                    exact_counts=true,
                                                    samples_multiplier=0.5)
    # stats to save
    kls = []
    likelihoods = []

    # iterate as many times as desired for updates
    #corpus = [[word.form for word in csent.words] for csent in traincorpus.sents]
    #subset = Int(floor(length(corpus)/cores))
    #println("Parse with grammar 0")
    #@time forests = epsilon ? pmap(s -> run_chartparser(s, grammar, epsilon=""), corpus) : pmap(s -> run_chartparser(s, grammar), corpus)
    #println("Calculate corpus likelihood")
    #@time cl = corpus_likelihood(forests, epsilon, true)
    #push!(likelihoods, cl)
    #@time println("likelihood of the training corpus: $cl\n")
    for i in 1:iterations
        println("BEGIN ITERATION $i")
        println("get counts and corpus likelihood for grammar $(i-1)")
        #@time counts = exact_counts ? expected_lexitem_counts_by_sentence(forests, grammar, epsilon) : sample_counts_by_sentence(forests, grammar, epsilon)
        @time cl, counts = parse_and_calculate(grammar, corpus, epsilon, exact_counts, cores, samples_multiplier)
        println("update counts")
    	@time prev_counts, new_grammar = vb_update(counts, prev_counts, grammar)

        # write a safety function that saves parameters every so many iterations in case computer crashes
        if saveprogress
            n = string(i)
            name = string(result_dir,"/", n,"_MG.txt")
            open(name, "w") do f
                for cat in keys(grammar.lexitem_cond.dists)
                    for id in keys(grammar.lexitem_cond.dists[cat].counts)
                        item = grammar.lexitem_cond.dists[cat].counts[id]
                        write(f, id)
                        prob = string(grammar.lexitem_cond.dists[cat].counts[id])
                        write(f, " ; $prob")
                        write(f, "\n")
                    end
                end
            end
        end

        #println("Parse with grammar $i")
        #@time forests = epsilon ? pmap(s -> run_chartparser(s, new_grammar, epsilon=""), corpus) : pmap(s -> run_chartparser(s, new_grammar), corpus)

        # calculate accuracy at the current iteration
        println("END ITERATAION $i: ")

        println("Calculate KL")
        @time kl = kullback_leibler(grammar, new_grammar)
        push!(kls, kl)
        println("KL divergence: $kl")

        #println("Calculate corpus likelihood")
        #@time cl = corpus_likelihood(forests, epsilon, true)
        push!(likelihoods, cl)
        @time println("likelihood of the training corpus: $cl")

        println("")
        flush(STDOUT)
        grammar = new_grammar
    end

    # print summary of stats
    println("")
    println("END VB: SUMMARY")
    println("===============")
    print("KL:")
    for x in kls
        print("\t$x")
    end
    println("")
    print("likelihoods:")
    for x in likelihoods
        print("\t$x")
    end
    println("")
    print("likelihoods:")
    exp_likelihoods = map(exp, likelihoods)
    for x in exp_likelihoods
        print("\t$x")
    end
    println("")
    flush(STDOUT)

    return grammar
end


##################################
###### LOAD SAVED LEXICON   ######
##################################
function load_lexicon(filename)
    lexicon = ""
    f = open(filename)
        lines = readlines(f)
        for l in lines
            lexicon = string(lexicon, l, "\n")
        end

    close(f)
    return lexicon
end
