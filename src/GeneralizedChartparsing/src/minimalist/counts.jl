###########################################
# Expected counts for Minimalist Grammars #
###########################################

"""
Given a forest, this gives the expected lexical item counts over the derivations in that forest, conditioned by category feature.
Returns a dictionary from category features to a dictionary from lexical item ids to counts.
"""
function expected_lexitem_counts_by_category(forest, grammar :: MinimalistGrammar, epsilon=true)
    if is_complete(forest)
        d = calculate_outside_probs_and_expected_rule_usages!(forest, grammar)
        lexitem_id(rule) = "$(rule[2]) :: $(rule[1])"
        ld = Dict(c[1] => Dict(lexitem_id(r) => s
                               for (r, s) in dd 
                               if typeof(r) == terminal_rule_type(grammar))
                  for (c, dd) in d)
        # transform dictionary so that a key is the category feature
        Dict(cat => merge([ld[key] for key in Iterators.filter(fs -> catfeature(fs) == cat, 
                                                               keys(ld))]...)
             for cat in unique([catfeature(fs) for fs in keys(ld)]))
    else
        Dict{Feature, Dict{String, Float64}}()
    end
end

"""
Given a forest, give the expected counts over lexical items in all the derivation trees in the forest.
Returns a dictionary from lexical item ids to counts.
"""
function expected_lexitem_counts(forest, grammar :: MinimalistGrammar, epsilon=true)
    if is_complete(forest)
        d = calculate_outside_probs_and_expected_rule_usages!(forest, grammar)
        # build corresponding dictionary over lexical items (keys as their feature vectors)
        lexitem_id(rule) = "$(rule[2]) :: $(rule[1])"
        ld = Dict(c[1] => Dict(lexitem_id(r) => s
                               for (r, s) in dd 
                               if typeof(r) == terminal_rule_type(grammar))
                  for (c, dd) in d)
        # combine all the inner dictionaries
        merge([d for d in values(ld)]...)
    else
        Dict{String, Float64}()
    end
end

"""
Given a collection of forests, return the counts over lexical items in all the derivation trees the grammar assigns to each sentence.
Return type is Dict{Vector{T}, Dict{String, Float64}}.
"""
function expected_lexitem_counts_by_sentence(forests, grammar, epsilon=true)
    println("Parse and calculate counts for $(length(forests)) sentences")
    @time Dict(f.terminals => expected_lexitem_counts(f, grammar, epsilon) for f in forests)
end

function sample_counts(forest, n, grammar :: MinimalistGrammar, epsilon=true, parallel=true, samples_multiplier=0.5)
    #n = length(forest.terminals)
    # lowest upper bound on nodes in a tree over a string of length n
    nodes = 2^(log(n) + 1) - 1
    # amt of samples to break even given a string of length n
    break_even = n^3 / nodes
    samples = Int(ceil(break_even * samples_multiplier))
    #samples = samples < 1 ? 1 : samples
    println("sample $samples trees for length $n string")

    counts = Dict{String, Float64}()
    if is_complete(forest)
        @time trees = [sample_tree(forest) for i in 1:samples]
        probs = map(t -> prob(grammar, t), trees)
        normalizing_const = sum(map(exp, probs))

        @time for (i, t) in enumerate(trees)
            ids = lexids(t)
            for lexid in unique(ids)
                lexcount = length(filter(x -> x == lexid, ids))
                c = lexcount * exp(probs[i]) / normalizing_const
                if haskey(counts, lexid)
                    counts[lexid] += c
                else
                    counts[lexid]  = c
                end
            end
        end
    end
    counts
end

function sample_counts_by_sentence(forests, grammar, epsilon=true, parallel=true)
    sents = map(f -> f.terminals, forests)
    counts = parallel ? pmap(f -> sample_counts(f, grammar, epsilon), forests) : map(f -> sample_counts(f, grammar, epsilon), forests)
    Dict(sents[i] => counts[i] for i in 1:length(sents))
end
