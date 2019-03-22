import Base.==

#############
## Feature ##
#############

# features are strings of the following forms: "R=f", "L=f", "f"
# where f is any string of lowercase or capital letters of the standard alphabet
Feature = String
is_feature(s :: String) = match(r"(R=|L=|\+|-)?[a-zA-Z]+", s).offset == 1
is_selector(f :: Feature) = length(f) > 2 && f[2] == '='
selects_right(f :: Feature) = is_selector(f) && f[1] == 'R'
selects_left(f :: Feature) = is_selector(f) && f[1] == 'L'
is_selectee(f :: Feature) = !is_selector(f)
name(f :: Feature) = if is_selector(f)
                        f[3:end]
                     else f[1:end]
                     end

"extracts the category (selectee) feature of a feature vector"
function catfeature(fs :: Vector{Feature})
    if isempty(fs)
        error("no category feature found.")
    elseif is_selectee(fs[1])
        fs[1]
    else
        catfeature(fs[2:end])
    end
end

#################
## LexicalItem ##
#################

type LexicalItem{T}
    phon :: T
    features :: Vector{Feature}
    id :: String # this is just the string representation of the LI
end

#function LexicalItem(s :: AbstractString)
#    splititem = map(String, map(strip, split(s, "::")))
#    (phon, features) = (String(splititem[1]), map(String, split(splititem[2])))
#    if !all(map(is_feature, features))
#        error(s, "contains a non feature. Check formatting.")
#    end
#    LexicalItem{String}(phon, map(Feature, features), String(s))
#end

function LexicalItem(s :: AbstractString)
    splititem = map(String, map(strip, split(strip(s), "::")))
    (phon, features) = (String(splititem[1]), map(String, split(splititem[2])))
    if !all(map(is_feature, features))
        error(s, "contains a non feature. Check formatting.")
    end
    features = map(Feature, features)
    id = ""
    for f in features
        id = string(id, " ", f)
    end
    id = string(phon, " ::", id)
    LexicalItem{String}(phon, features, id)
end


function LexicalItem{T}(phon :: T, features :: Vector{Feature})
    id = ""
    for f in features
        id = string(id, " ", f)
    end
    id = string(phon, " ::", id)
    LexicalItem(phon, features, id)
end

phon{T}(lex :: LexicalItem{T}) = lex.phon
features{T}(lex :: LexicalItem{T}) = lex.features
id{T}(lex :: LexicalItem{T}) = lex.id

function show{T}(io :: IO, lex :: LexicalItem{T})
    print(io, "$(lex.phon) ::")
    map(x -> print(io, " $x"), lex.features)
end
show(lex :: LexicalItem) = show(STDOUT, lex)
==(l1 :: LexicalItem, l2 :: LexicalItem) = id(l1) == id(l2)
isequal(l1 :: LexicalItem, l2 :: LexicalItem) = isequal(id(l1), id(l2))

#########################################
## helper methods for minimalist rules ##
#########################################

match(f :: Vector{Feature}, g :: Vector{Feature}) =
    name(f[1]) == name(g[1]) && (
        (selects_right(f[1]) && is_selectee(g[1])) ||
        (selects_left(g[1]) && is_selectee(f[1])))

is_finalfeat(f :: Vector{Feature}) = length(f) == 1


adjacent(r :: Tuple{Int, Int}, s :: Tuple{Int, Int}) =  r[1] <= s[2] && r[2] == s[1]

######################
## minimalist rules ##
######################

function merge(f :: Vector{Vector{Feature}}, g :: Vector{Vector{Feature}})
    @assert match(f[1], g[1])
    if selects_right(f[1][1]) && is_selectee(g[1][1]) && is_finalfeat(g[1])
        merge_right(f, g)
    elseif selects_left(g[1][1]) && is_selectee(f[1][1]) && is_finalfeat(f[1])
        merge_left(f, g)
    elseif selects_right(f[1][1]) && is_selectee(g[1][1])
        merge_move_right(f, g)
    elseif selects_left(g[1][1]) && is_selectee(f[1][1])
        merge_move_left(f, g)
    end
end

function merge_right(f :: Vector{Vector{Feature}}, g :: Vector{Vector{Feature}})
    [[[f[1][2:end]]; f[2:end]]; g[2:end]]
end

function merge_left(f :: Vector{Vector{Feature}}, g :: Vector{Vector{Feature}})
    [[[g[1][2:end]]; g[2:end]]; f[2:end]]
end

#######################
## MinimalistGrammar ##
#######################

MinimalistCategoryID = String
MinimalistRule = Tuple{MinimalistCategoryID,  Tuple{MinimalistCategoryID, MinimalistCategoryID}}
MinimalistState = State{MinimalistCategoryID,  MinimalistRule}
id(feats :: Vector{Feature}) = join(feats, ' ') # convert features to category id
features(id :: String) = map(String, split(id, ' ')) # convert category id to features

type MinimalistGrammar{T, S, Cond}
    lexicon :: Dict{T, Vector{LexicalItem{T}}}
    start_symbols :: Vector{Vector{Feature}}
    lexitem_cond :: Cond
    startstate :: MinimalistState
end
show(mg :: MinimalistGrammar) = string(("$(id(li)) ; $(exp(prob(mg, li)))\n"
                                        for (w,lexitems) in mg.lexicon
                                        for li in lexitems)...)
show_counts(mg :: MinimalistGrammar) =
    string(("$(id(li)) ; $(mg.lexitem_cond.dists[catfeature(features(li))].counts[id(li)])\n"
            for (w,lexitems) in mg.lexicon
            for li in lexitems)...)

function getlexicalitem(lexid :: String, g :: MinimalistGrammar)
    li = LexicalItem(lexid)
    @assert haskey(g.lexicon, li.phon) "lexicon has no lexical items for $(li.phon)"
    lis = g.lexicon[li.phon]
    try
        filter(li -> id(li) == lexid, lis)[1]
    catch
        error("lexicon has no lexical items matching $lexid")
    end
end

function construct_minimalist_states!(lexicon_features :: Vector{Vector{Feature}}, startstate :: MinimalistState)
    i = 1; while i <= length(lexicon_features)
        fs = lexicon_features[i]
        if is_selector(fs[1])
            cats = if selects_right(fs[1])
                [[fs], [[name(fs[1])]]]
            else
                [[[name(fs[1])]], [fs]]
            end
            merged = merge(cats...)

            cats_ids = Tuple(map(x -> join(x[1], ' '), cats))
            merged_id = join(merged[1], ' ')
            rule = (merged_id, cats_ids)
            add_rule!(startstate, rule, merged_id, cats_ids)

            if is_selector(merged[1][1])
                push!(lexicon_features, merged[1])
            end
        end
        #construct_minimalist_states!(new_lexicon_features, startstate)
        i+= 1
    end
end

"Given a lexicon as a Vector, constructs a MinimalistGrammar with a uniformly initialized Dirichlet distribution, using the DirCatVar type."
function MinimalistGrammar{T}(lexicon :: Vector{LexicalItem{T}}, start_symbols=[["c"]])
    lexitem_cond = SimpleCond(Dict(
        cat => DirCatVar(map(id, filter(li -> catfeature(features(li)) == cat, lexicon)))
        for cat in unique([catfeature(features(li)) for li in lexicon])))
    startstate = State(MinimalistCategoryID,  MinimalistRule)
    construct_minimalist_states!(unique(map(features, lexicon)), startstate)
    lex_dict = Dict(word => filter(li -> phon(li) == word, lexicon)
                    for word in unique(map(phon, lexicon)))
    MinimalistGrammar{T,LogProb,SimpleCond}(lex_dict, start_symbols, lexitem_cond, startstate)
end

"Given a lexicon as a Dict, constructs a MinimalistGrammar with a uniformly initialized Dirichlet distribution, using the DirCatVar type."
function MinimalistGrammar{T}(lexicon :: Dict{T, Vector{LexicalItem{T}}}, start_symbols=[["c"]])
    lex_list = vcat(values(lexicon)...)
    lexitem_cond = SimpleCond(Dict(
        cat => DirCatVar(map(id, filter(li -> catfeature(features(li)) == cat, lex_list)))
        for cat in unique([catfeature(features(li)) for li in lex_list])))
    MinimalistGrammar{T, LogProb, SimpleCond}(lexicon, start_symbols, lexitem_cond)
end

"Given a lexicon and a conditional distritbution, constructs a MinimalistGrammar."
function MinimalistGrammar{T,Cond}(lexicon :: Dict{T, Vector{LexicalItem{T}}}, lexitem_cond :: Cond, start_symbols=[["c"]], Prob=LogProb)
    lex_list = vcat(values(lexicon)...)
    startstate = State(MinimalistCategoryID,  MinimalistRule)
    construct_minimalist_states!(unique(map(features, lex_list)), startstate)
    MinimalistGrammar{T,Prob,Cond}(lexicon, start_symbols, lexitem_cond, startstate)
end

"""
Given a string containing lines of the form
    word :: feat1 feat2 ... featN ; prob
construct a MinimalistGrammar with a distribution populated with the specified values.
Default distribution is dirichlet distribution.
"""
function MinimalistGrammar(s :: AbstractString, start_symbols=[["c"]], Dist=DirCatVar{String,Float64}, logprob=true)
    pairs = map(x -> split(strip(x), ";"), split(strip(s), "\n"))
    lexicon_probs = [(LexicalItem(li), float(parse(p))) for (li,p) in pairs]
    if logprob
        lexitem_cond = SimpleCond(Dict(
            cat => Dist(Dict(
                id(lexitem) => prob
                for (lexitem,prob) in filter(
                    pair -> catfeature(features(pair[1])) == cat,
                    lexicon_probs)))
            for cat in unique([catfeature(features(li)) for (li,p) in lexicon_probs])))
    else
        lexitem_cond = SimpleCond(Dict(
            cat => Dist(Dict(
                id(lexitem) => LogProb(prob)
                for (lexitem,prob) in filter(
                    pair -> catfeature(features(pair[1])) == cat,
                    lexicon_probs)))
            for cat in unique([catfeature(features(li)) for (li,p) in lexicon_probs])))
    end
    startstate = State(MinimalistCategoryID, MinimalistRule)
    lexicon = map(first, lexicon_probs)
    construct_minimalist_states!(unique(map(features, lexicon)), startstate)
    lex_dict = Dict(word => filter(li -> phon(li) == word, lexicon)
                    for word in unique(map(phon, lexicon)))
    MinimalistGrammar{String, LogProb, SimpleCond}(lex_dict, start_symbols, lexitem_cond, startstate)
end

lexicon{T,S,C}(g :: MinimalistGrammar{T,S,C}) = g.lexicon
start_symbols{T,S,C}(g :: MinimalistGrammar{T,S,C}) = g.start_symbols
lexitem_cond{T,S,C}(g :: MinimalistGrammar{T,S,C}) = g.lexitem_cond
startstate{T,S,C}(g :: MinimalistGrammar{T,S,C}) = g.startstate

"""
This function is needed for calculate_outside_probs_and_expected_rule_usages!
Gives the probability that grammar g assigns to the rule.
"""
prob(g :: MinimalistGrammar, lexitem :: LexicalItem) =
    logscore(g.lexitem_cond.dists[catfeature(features(lexitem))], id(lexitem))
function prob(g :: MinimalistGrammar, cat_id :: MinimalistCategoryID, rule)
    if typeof(rule) == terminal_rule_type(g)
        word = rule[2]
        lexitem = filter(li -> features(li) == features(cat_id), lexicon(g)[word])[1]
        logscore(g.lexitem_cond.dists[catfeature(features(cat_id))], id(lexitem))
    elseif typeof(rule) == category_rule_type(g)
        LogProb(1)
    end
end

# type interface functions. needed by the parser.
terminal_type{T, Score, Cond}(g :: MinimalistGrammar{T, Score, Cond}) = T
category_type{T, Score, Cond}(g :: MinimalistGrammar{T, Score, Cond}) = MinimalistCategoryID
category_rule_type{T, Score, Cond}(g :: MinimalistGrammar{T, Score, Cond}) = MinimalistRule
terminal_rule_type{T, Score, Cond}(g :: MinimalistGrammar{T, Score, Cond}) =
Tuple{MinimalistCategoryID, T}
score_type{T, Score, Cond}(g :: MinimalistGrammar{T, Score, Cond}) = Score
state_type{T, Score, Cond}(g :: MinimalistGrammar{T, Score, Cond}) = MinimalistState

function unkify!(g :: MinimalistGrammar)
    feature_seqs = unique(Tuple(features(li)) for (word, lexitems) in lexicon(g) for li in lexitems)
    unks = [LexicalItem("UNK", [f for f in fs]) for fs in feature_seqs]
    g.lexicon["UNK"] = unks
    try
        for unkli in unks
            g.lexitem_cond.dists[catfeature(features(unkli))].counts[id(unkli)] = 1
        end
    catch y
        error("unkify! only works with MGs specified with Dirichlet distributions. Error thrown:")
        throw(y)
    end
end

##############################
## parser-grammar interface ##
##############################

is_final{T,Sc}(g :: MinimalistGrammar{T,Sc}, s :: MinimalistState) = isfinal(s)

is_possible_transition{T,Sc}(g :: MinimalistGrammar{T,Sc}, s :: MinimalistState, c :: MinimalistCategoryID) = !isfinal(s) && haskey(s.trans, c)

transition{T,Sc}(g :: MinimalistGrammar{T,Sc}, s :: MinimalistState, c :: MinimalistCategoryID) = s.trans[c]

startsymbols{T,Sc}(g :: MinimalistGrammar{T,Sc}) = map(x -> join(x, ' '), start_symbols(g))

completions{T,Sc}(g :: MinimalistGrammar{T,Sc}, s :: MinimalistState) = [
    (c, r, prob(g, c, r)) for (c, r) in s.comp]

function completions{T,Sc}(g :: MinimalistGrammar{T,Sc}, t :: T)
    comps = Vector{Tuple{category_type(g), terminal_rule_type(g), score_type(g)}}()
    for li in lexicon(g)[t]
        cat = id(features(li))
        rule = (cat, t)
        push!(comps, (cat, rule, prob(g, cat, rule)))
    end
    comps
end

#############################
# Converting from CFG to MG #
#############################

# FIXME these might not work! they haven't been updated since I overhauled the
# code in this file for MGs and LexicalItems. -chris

"""
Removes unary rules from lists of CFG rules in place, giving a weakly-equivalent
grammar.
"""
function remove_unaries!{C,T}(category_rules::Vector{ContextFreeRule{C,C}}, terminal_rules::Vector{ContextFreeRule{C,T}})
    i = 1
    while i <= length(category_rules)
        if length(rhs(category_rules[i])) == 1
            unary_rule = splice!(category_rules, i)
            f(x::ContextFreeRule{C,C}) = lhs(x) == rhs(unary_rule)[1]
            for rules in [category_rules, terminal_rules]
                I = find(f, rules)
                for j in I
                    old_rule = rules[j]
                    new_rule = ContextFreeRule(lhs(unary_rule), rhs(old_rule))
                    rules[j] = new_rule
                end
            end
        else i += 1
        end
    end
end

"""
Convert a CFG encoded as lists of rules into an MG.
"""
# FIXME: For now this assigns a score (probability) of 1 to each lexical item
# So the resulting grammar may not have valid probability distributions
function cfg2mg{C<:AbstractString, T<:AbstractString, D<:AbstractString}(
        category_rules::Vector{ContextFreeRule{C,C}},
        terminal_rules::Vector{ContextFreeRule{C,T}},
        startsymbols::Vector{D},
        Score=LogProb)

    if !all(length(rhs(rule)) > 1 for rule in category_rules)
        error("Category rules cannot be unary!")
    end
    if !all(length(rhs(rule)) == 1 for rule in terminal_rules)
        error("Terminal rules must be unary!")
    end

    terminals = [[rhs(rule)[1] for rule in terminal_rules]; ""]
    terminal_map = Dict{String, Set{Vector{Feature}}}(t => Set() for t in terminals)
    complex_map = Dict{String, Set{Vector{Feature}}}(t => Set() for t in terminals)

    for rule in terminal_rules
        t = rhs(rule)[1]
        cat = lhs(rule)
        push!(terminal_map[t], [cat])
    end

    for rule in category_rules
        # find categories in the RHS of the rule which are mapped to by terminals
        f(x) = [x] in union(values(terminal_map)...)
        I = find(f, rhs(rule))
        # create a complex category for the first matching terminal found
        if !isempty(I)
            i = I[1] # let the first matching category in the RHS be the head
            head_cat = rhs(rule)[i]
            for t in keys(filter((x,y)->[head_cat] in y, terminal_map))
                feats = [["L=$B" for B in rhs(rule)[i-1:-1:1]];
                    ["R=$B" for B in rhs(rule)[i+1:end]];
                    ["$(lhs(rule))"]]
                    push!(complex_map[t], feats)
            end
            # if no matching terminals found, create a complex category for the empty string
        else
            feats = [["R=$B" for B in rhs(rule)]; ["$(lhs(rule))"]]
            push!(complex_map[""], feats)
        end
    end

    map = merge(union, terminal_map, complex_map)
    lexicon = [LexicalItem(t,fs,Score(1)) for (t,featsset) in map for fs in featsset]
    return MinimalistGrammar(lexicon, [[x] for x in startsymbols], 1)
end

########

# helpers for getting information from trees

getrule(node :: TreeNode) = node.data[2]

function getlexid(l :: TreeNode)
    r = getrule(l)
    "$(r[2]) :: $(r[1])"
end

"Give list of lexical item ids used in tree in order, preserves duplicates."
lexids(t :: TreeNode) = map(getlexid, leafs(t))

"Probability of a MG derivation tree"
function prob(g :: MinimalistGrammar, t :: TreeNode)
    f(r) = prob(g, r[1], r)
    prod(map(f, map(getrule, leafs(t))))
end

