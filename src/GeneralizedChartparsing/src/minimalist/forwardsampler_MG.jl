

type SampleGrammar{T, S, Cond}
    MG :: MinimalistGrammar{T, S, Cond}
    lexicon :: Dict{String, LexicalItem{T}}
    selectees :: Vector{Feature}
end

function SampleGrammar{T, S, Cond}(MG :: MinimalistGrammar{T, S, Cond})
    selectees = append!([], keys(MG.lexitem_cond.dists))
    lexicon = Dict{String, LexicalItem{String}}()
    for word in keys(MG.lexicon)
        for lex in MG.lexicon[word]
            lexicon[lex.id] = lex
        end
    end
    SampleGrammar{T, S, Cond}(MG, lexicon, selectees)
end

#####################################################
#                                                   #
#                   FORWARD SAMPLER                 #
#                                                   #
#####################################################

#################
## Sample Type ##
#################

#Contains a sampled derivation tree and the probability of that tree
type SampleDerivation{T}
        d :: Tree{T}
        prob :: LogProb
end

function SampleDerivation(d, prob::LogProb)
    SampleDerivation{String}(d, prob)
end

################################
## Helper methods for Sampler ##
################################

###Finds selector features to branch children
function find_selectors(lex)
    selectors = Feature[]
    for f in lex.features
        if is_selector(f)
            push!(selectors,f[3:length(f)])
        end
    end
    return selectors
end

#function findLexicalItem(MG :: , id :: String)
#    ind = search(id, ' ')
#    word = id[1,(ind-1)]
#    for lex in MG.lexicon[word]
#        if lex.id == id
#            return lex
#        end
#    end
#end


#### Recursively builds a sample tree rooted at category C
function treeRootC(L, c, parent)
    id = sample(L.MG.lexitem_cond.dists[c])
    lex = L.lexicon[id]
    node = TreeNode(id, parent)
    subroots = find_selectors(lex)
    if length(subroots) > 0
        for sel in subroots
            insert_child!(node, treeRootC(L,sel,node))
        end
        return node
    else
        return node
    end
end

####Recursively calculates probability of a tree
function probDerivation(d, L)
    if isterminal(d)
        lex = L.lexicon[d.data]
        prob = logscore(L.MG.lexitem_cond.dists[catfeature(lex.features)],lex.id)
        return prob
    else
        probs = []
        lex = L.lexicon[d.data]
        prob = logscore(L.MG.lexitem_cond.dists[catfeature(lex.features)],lex.id)
        push!(probs,prob)
        for child in d.children
            push!(probs, probDerivation(child, L))
        end
        return prod(probs)
    end
end

###################
##Forward Sampler##
###################

function samplerMGderivation(L::SampleGrammar{String, LogProb, SimpleCond})
    i = rand(1:length(L.selectees))
    c = L.selectees[i]
    root = EmptyTree{String}()
    d = treeRootC(L,c,root)
    prob = probDerivation(d, L)
    return SampleDerivation(d,prob)
end


####################
## Make a corpus  ##
####################


function getsent_fromderiv(d, L)
    #d = deriv.d
    sent = ""
    if ! isterminal(d)
        lex = L.lexicon[d.data]
        sent = lex.phon
        sels = lex.features
        for i in 1:(length(sels) - 1)
            f = sels[i]
            if contains(f, "L")
                sent = string(getsent_fromderiv(d.children[i], L), " ", sent)
            else
                sent = string(sent, " ", getsent_fromderiv(d.children[i], L))
            end
        end
    else
        lex = L.lexicon[d.data]
        sent = lex.phon
    end
    return sent
end


function sample_corpus(corpus_size, L, result_dir)
    corpus = Array{Any}(corpus_size)
    for i in 1:corpus_size
        corpus[i] = samplerMGderivation(L)
    end
    name1 = string(result_dir,"/corpus.txt")
    name2 = string(result_dir,"/corpus_probs.txt")
    open(name1, "w") do f
        for deriv in corpus
            sent = getsent_fromderiv(deriv.d, L)
            write(f, sent)
            write(f, "\n")
        end
    end
    open(name2, "w") do f
        for deriv in corpus
            sent = getsent_fromderiv(deriv.d, L)
            prob = deriv.prob
            p = string(exp(prob))
            write(f, sent)
            write(f, string(";", p))
            write(f, "\n")
        end
    end
end

function corpus_without_duplicates(corpus_dir)
    sentences = []
    open(string(corpus_dir , "/corpus.txt"), "r") do f
        lines = readlines(f)
        sentences = unique(lines)
    end
    open(string(corpus_dir , "/corpus_noduplicates.txt"), "w") do f
        for sent in sentences
            write(f, sent)
            write(f, "\n")
        end
    end
    sentences_probs = []
    open(string(corpus_dir , "/corpus_probs.txt"), "r") do h
        lines = readlines(h)
        sentences_probs = unique(lines)
    end
    open(string(corpus_dir , "/corpus_probs_noduplicates.txt"), "w") do h
        for sent in sentences_probs
            write(h, sent)
            write(h, "\n")
        end
    end
    return (length(sentences), length(sentences_probs))
end


#####################################################
###### WRITE PROBABILITIES FOR LEARNT GRAMMARS ######
#####################################################
function write_probs(grammar, result_dir)
    name = string(result_dir,"/probabilities_MG.txt")
    probs = Dict()
    open(name, "w") do f
        for cat in keys(grammar.lexitem_cond.dists)
            for id in keys(grammar.lexitem_cond.dists[cat].counts)
                item = grammar.lexitem_cond.dists[cat].counts[id]
                write(f, id)
                logprb = GeneralizedChartparsing.CompoundDistributions.logscore(grammar.lexitem_cond.dists[cat], id)
                p = exp(logprb)
                prob = string(p)
                probs[id]= p
                write(f, " ; $prob")
                write(f, "\n")
            end
        end
    end
    return probs
end

function write_sentence_probs(grammar, corpus, result_dir)
    name = string(result_dir,"/sentence_probabilities_MG.txt")
    open(name, "w") do f
        for sent in corpus
            forest = run_chartparser(sent, grammar, epsilon="")
            logprb = score(forest)
            for word in sent
                write(f, string(word, " "))
            end
            p = exp(logprb)
            prob = string(p)
            write(f, ";$prob")
            write(f, "\n")
        end
    end
end

function write_sentence_probs_ranked(grammar, corpus, result_dir)
    name = string(result_dir,"/ranked_sentence_probabilities_MG.txt")
    sentences = []
    ranking_order = []
    index = 1
    for sent in corpus
        forest = run_chartparser(sent, grammar, epsilon="")
        prob = exp(score(forest))
        push!(sentences, (sent, prob, index))
        index+=1
    end
    sort!(sentences, by = x -> x[2])
    open(name, "w") do f
        for sent in sentences
            for word in sent[1]
                write(f, string(word, " "))
            end
            prob = string(sent[2])
            write(f, ";$prob")
            write(f, "\n")
            push!(ranking_order, (sent[2], sent[3]))
        end
    end
    return ranking_order
end

##########################################
