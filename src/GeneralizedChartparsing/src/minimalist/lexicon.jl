### Make lexicon file from Corpus to then make MinimalistGrammar
#


##################CREATE POSSIBLE FEATURE SETS GIVE 1 CAT###########################
function add_feats(feature_list, sub_list,category_feats, k)
    append!(feature_list, sub_list)
    if k == 0
        return feature_list
    end
    new_sub_list = []
    for list in sub_list
        for c in category_feats
            Rfeats = string(list[1:(end-1)], "R=", c, " ", list[end])
            Lfeats = string("L=", c, " ", list)
            push!(new_sub_list, Rfeats)
            push!(new_sub_list, Lfeats)
        end
    end
    k-=1
    return add_feats(feature_list,new_sub_list,category_feats,k)
end

function add_feats_Rightfirst(feature_list, sub_list,category_feats, k)
    append!(feature_list, sub_list)
    if k == 0
        return feature_list
    end
    new_sub_list = []
    for list in sub_list
        for c in category_feats
            Rfeats = string("R=", c, " ", list)
            push!(new_sub_list, Rfeats)
        end
    end
    k-=1
    return add_feats_Rightfirst(feature_list,new_sub_list,category_feats,k)
end

function add_feats_Leftsecond(feature_list, sub_list,category_feats, k, m)
    append!(feature_list, sub_list)
    if k == 0
        return feature_list
    end
    new_sub_list = []
    for list in sub_list
        if length(list) < 4*m+1
            for c in category_feats
                Lfeats = string("L=", c, " ", list)
                push!(new_sub_list, Lfeats)
            end
        end
    end
    k-=1
    return add_feats_Leftsecond(feature_list,new_sub_list,category_feats,k, m)
end

function add_feats_Balanced(feature_list, sub_list,category_feats, k, m)
    right_list = add_feats_Rightfirst(feature_list, sub_list, category_feats, k)
    left_list = add_feats_Leftsecond([], right_list, category_feats, k, m)
    return left_list
end



###################CREATE FEATURE SET DICT FOR ALL CATEGORIES###################
function make_feature_list_by_cat_dict(categories)
    feature_list = Dict()
    cats = []
    for (c, sels, k) in categories
        push!(cats, c)
        sub_list = [c]
        feature_list[c] = add_feats_Balanced([], sub_list , sels , k, k)
    end
    #Add start symbol category
    feature_list["c"] = add_feats_Rightfirst([], ["c"] , cats, 1)
    return feature_list
    #feature_list_start = add_feats_Balanced([], ["c"] , cats, 2, 2)
    #return (feature_list, feature_list_start)
end

function make_feature_list_by_cat(categories, k)
    feature_list = Dict()
    for c in categories
        sub_list = [c]
        feature_list[c] = add_feats([], sub_list , categories, k)
    end
    # Add start symbol category
    feature_list_start = add_feats([], ["c"] , categories, k)
    return (feature_list, feature_list_start)
end

function make_feature_list(categories, k)
    feature_list = add_feats([], categories , categories, k)
    # Add start symbol category
    feature_list_start = add_feats([], ["c"] , categories, k)
    return (feature_list, feature_list_start)
end
######################TRANSFORM POS TO MINIMALIST CAT###########################

function getcat_fromPOS(pos)
    cat = ""
    if in(pos, ["NOUN", "PROPN"])
        cat = "n"
    elseif in(pos, ["DET", "PRON"])
        cat = "d"
    elseif in(pos, ["VERB"])
        cat = "v"
    elseif in(pos, ["ADJ"])
        cat = "a"
    elseif in(pos, ["ADV"])
        cat = "x"
    elseif in(pos, ["ADP"])
        cat = "p"
    elseif in(pos, ["AUX"])
        cat = "t"
    elseif in(pos, ["SCONJ"])
        cat = "s"
    elseif in(pos, ["PART"])
        cat = "r"
    end
    return cat
end

## In the case of English, this splits the POS PART into three category types:
## infinitive-> t, possessive-> d, negation-> neg
function getcat_fromPOS_EnglishOnly(lemma, pos)
    cat = ""
    if in(pos, ["NOUN", "PROPN"])
        cat = "n"
    elseif in(pos, ["DET", "PRON"])
        cat = "d"
    elseif in(pos, ["VERB"])
        cat = "v"
    elseif in(pos, ["ADJ"])
        cat = "a"
    elseif in(pos, ["ADV"])
        cat = "x"
    elseif in(pos, ["ADP"])
        cat = "p"
    elseif in(pos, ["AUX"])
        cat = "t"
    elseif in(pos, ["SCONJ"])
        cat = "s"
    elseif in(pos, ["PART"])
        if contains(lemma, "n")
            cat = "neg"
        elseif contains(lemma, "t")
            cat = "t"
        else
            cat = "d"
        end
    end
    return cat
end

######################CREATE A LEXICON FROM CORPUS##############################

## for testing, corpus is a list of sentences, which is itself a list of tuples,
## (word, cat)
function makeLexicon_test(corpus, categories, k)

    feature_list, feature_list_start = make_feature_list_by_cat(categories, k)
    words = []
    lexicon = ""
    for featlist in feature_list_start
        item = string( ":: ", featlist, " ; 1")
        lexicon = string(lexicon,"\n", item)
    end

    for sent in corpus
        for (word,cat) in sent
            if !(word in words)
                push!(words, word)
                for featlist in feature_list[cat]
                    item = string(word, " :: ", featlist, " ; 1")
                    lexicon = string(lexicon,"\n", item)
                end
            end
        end
    end
    open("lexicon.txt", "w") do f
        write(f, lexicon)
    end
    return lexicon
end


## Makes a lexicon and filtered corpus from UD data. If using the english only
## category mapping add eng_only=true to call.
function makeLexicon_andCorpus(UDcorpus, categories, eng_only=false)

    feature_list = make_feature_list_by_cat_dict(categories)
    words = []
    lexicon = ""
    corpus = []
    counts = Dict()
    for (cat, sels, n) in categories
        counts[cat] = 1
    end
    for (cat,featlist) in feature_list
        for flist in featlist
            item = string( ":: ", flist, " ; 1")
            lexicon = string(lexicon,"\n", item)
        end
    end
    for sent in UDcorpus.sents
        sentence = []
        goodsent = true
        for word in sent.words
            w = lowercase(word.form)
            ##remove sentences which contain one of these POS tags
            if in(word.upos_tag, ["INTJ", "CONJ","SYM","X"])
                goodsent = false
                break
            elseif ';' in word.form
                goodsent = false
                break
            elseif in(word.upos_tag, ["ADJ","ADV","ADP","AUX","DET", "NOUN", "PART", "PRON", "PROPN", "SCONJ", "VERB"])
                if !(string(w,word.upos_tag) in words)
                    push!(words, string(w,word.upos_tag))
                    if eng_only
                        cat = getcat_fromPOS_EnglishOnly(word.lemma, word.upos_tag)
                    else
                        cat = getcat_fromPOS(word.upos_tag)
                    end
                    counts[cat] +=1
                    for featlist in feature_list[cat]
                        item = string(w, " :: ", featlist, " ; 1")
                        lexicon = string(lexicon,"\n", item)
                    end
                    sentence = push!(sentence, w)
                else
                    sentence = push!(sentence, w)
                end

            ### ADDING SECTION TO INCLUDE NUMERALS
            elseif in(word.upos_tag, ["NUM"])
                if !(string(w,word.upos_tag) in words)
                    push!(words, string(w,word.upos_tag))
                    cats = ["d","n","a"]
                    for cat in cats
                        counts[cat] +=1
                        for featlist in feature_list[cat]
                            item = string(w, " :: ", featlist, " ; 1")
                            lexicon = string(lexicon,"\n", item)
                        end
                    end
                    sentence = push!(sentence, w)
                else
                    sentence = push!(sentence, w)
                end
            end
        end
        if goodsent && sentence != []
            push!(corpus, sentence)
        end
    end
    open("lexicon.txt", "w") do f
        write(f, lexicon)
    end
    open("corpus.txt", "w") do f
        for sentence in corpus
            for word in sentence
                write(f, string(word, " "))
            end
            write(f, "\n")
        end
    end
    corpus = convert(Array{Array{String,1},1},corpus)
    return (lexicon, corpus, counts)
end

########## FUNCTIONS FOR FIXING GRAMMARS ##########

# remove duplicate lexical items
function remove_duplicates!(grammar)
    for word in keys(grammar.lexicon)
        ids = unique(map(GeneralizedChartparsing.id, grammar.lexicon[word]))
        if length(ids) != length(grammar.lexicon[word])
            grammar.lexicon[word] = map(LexicalItem, ids)
        end
    end
end

# remove null heads of the form =x x (vacuous heads)
function remove_vacuous_nullheads!(grammar)
    function nonvacuous(li)
        feats = li.features
        if length(feats) == 2
            GeneralizedChartparsing.name(feats[1]) != GeneralizedChartparsing.name(feats[2])
        else true
        end
    end
    grammar.lexicon[""] = filter(nonvacuous, grammar.lexicon[""])
end
