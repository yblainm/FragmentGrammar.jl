
################################
## Helper methods for Sampler ##
################################

is_licensor(f :: Feature) = length(f) > 1 && f[1] == '+'
is_licensee(f :: Feature) = length(f) > 1 && f[1] == '-'

###Finds selector features to branch children
function find_features(lex)
    selectors = Feature[]
    licensors = Feature[]
    licensees = Feature[]
    for f in lex.features
        if is_selector(f)
            push!(selectors,f[3:length(f)])
        elseif is_licensor(f)
            push!(licensors,f[2:length(f)])
        elseif is_licensee(f)
            push!(licensees,f[2:length(f)])
        end
    end
    return (selectors, licensors, licensees)
end


#### Recursively builds a sample tree rooted at category C
function treeRootC_move(L, c, parent, prev_licensors)
    move_feat_mismatch = true
    move_feats = []
    id = ""
    selectors = []
    licensors = []
    licensees = []
    while move_feat_mismatch
        move_feats = copy(prev_licensors)
        id = sample(L.MG.lexitem_cond.dists[c])
        lex = L.lexicon[id]
        selectors, licensors, licensees = find_features(lex)
        if licensees != []
            for licensee in licensees
                if licensee in move_feats
                     deleteat!(move_feats ,findfirst(move_feats,licensee))
                else
                     break
                end
                move_feat_mismatch = false
            end
        end
        if selectors == []
            if move_feats != []
                move_feat_mismatch = true
            else
                move_feat_mismatch = false
            end
        else
            move_feat_mismatch = false
        end
    end
    node = TreeNode(id, parent)
    if length(selectors) > 0
        len = length(subroots)
        for sel in subroots[1:(len-1)]
            insert_child!(node, treeRootC(L,sel,node, []))
        end
        insert_child!(node, treeRootC(L,subroots[len],node, append!(move_feats,licensors)))
        return node
    else
        return node
    end
end

###################
##Forward Sampler##
###################

function samplerMGderivation_move(L::SampleGrammar{String, LogProb, SimpleCond})
    i = rand(1:length(L.selectees))
    c = L.selectees[i]
    root = EmptyTree{String}()
    d = treeRootC_move(L,c,root,[])
    prob = probDerivation(d, L)
    return SampleDerivation(d,prob)
end


####################
## Make a corpus  ##
####################

function getchain_fromderiv_move(d, L)
    chain = []
    if ! isterminal(d)
        lex = L.lexicon[d.data]
        selectors, licensors, licensees = find_features(lex)
        for i in 1:(length(selectors))
            f = selectors[i]
            if contains(f, "L")
                chain = append!(getchain_fromderiv_move(d.children[i], L), [lex])
            else
                chain = append!([lex], getchain_fromderiv_move(d.children[i], L))
            end
        end
    else
        lex = L.lexicon[d.data]
        chain = [lex]
    end
    return chain
end

function apply_move(chain)
    sent = []
    while chain != []
        selectors, licensors, licensees = find_features(chain[1])
        while licensors != []
            for i in (2:length(chain))
                next_selectors, next_licensors, next_licensees = find_features(chain[i])
                if next_licensees != []
                    if licensors[1] == next_licensees[1]
                        append!(sent, [chain[i]])
                        deleteat!(licensors,1)
                        deleteat!(chain, i )
                        break
                    end
                end
            end
        end
        push!(sent, chain[1])
        deleteat!(chain, 1)
    end
    return sent
end


function getsent_fromderiv_move(d, L)
    chain = apply_move(getchain_fromderiv_move(d, L))
    sent = ""
    for lex in chain
        sent = string(sent, lex.phon , " ")
    end
    return sent
end


function sample_corpus_move(corpus_size, L, result_dir)
    corpus = Array{Any}(corpus_size)
    for i in 1:corpus_size
        corpus[i] = samplerMGderivation_move(L)
    end
    name1 = string(result_dir,"/corpus.txt")
    name2 = string(result_dir,"/corpus_probs.txt")
    open(name1, "w") do f
        for deriv in corpus
            sent = getsent_fromderiv_move(deriv.d, L)
            write(f, sent)
            write(f, "\n")
        end
    end
    open(name2, "w") do f
        for deriv in corpus
            sent = getsent_fromderiv_move(deriv.d, L)
            prob = deriv.prob
            p = string(exp(prob))
            write(f, sent)
            write(f, string(";", p))
            write(f, "\n")
        end
    end
end
