####################
### Rule classes ###
####################

immutable ContextFreeRule{C1,C2}
    lhs :: C1                # left hand side
    rhs :: Tuple{Vararg{C2}} # right hand side
end

lhs(r::ContextFreeRule) = r.lhs
rhs(r::ContextFreeRule) = r.rhs

function can_create_as_prefix(r::ContextFreeRule, cats)
    @assert length(cats) <= length(r.rhs)
    for (i,cat) in enumerate(cats)
        if cat != r.rhs[i]
            return false
        end
    end
    return true
end

function can_create(r::ContextFreeRule, cats)
    length(r.rhs) == length(cats) && can_create_as_prefix(r,cats)
end
can_create(r::ContextFreeRule, cats...) = can_create(r, cats)

function completion(r::ContextFreeRule, cats)
    @assert cats == r.rhs
    r.lhs
end

isapplicable(r::ContextFreeRule, cat) = lhs(r) == cat
(r::ContextFreeRule)(cat) = rhs(r)

domain(r::ContextFreeRule) = r.lhs

# R is the rule Type
immutable MetaRule{R}
    name  :: String
    rules :: Vector{R}
end
name(r::MetaRule) = r.name
show(io::IO, r::MetaRule) = print(io, name(r))

start(mr::MetaRule) = start(mr.rules)
next(mr::MetaRule, state) = next(mr.rules, state)
done(mr::MetaRule, state) = done(mr.rules, state)
length(mr::MetaRule) = length(mr.rules)
eltype(mr::MetaRule) = eltype(mr.rules)

function isapplicable(mr::MetaRule, cat)
    for r in mr.rules
        if isapplicable(r, cat)
            return true
        end
    end
    return false
end

function (mr::MetaRule)(cat)
    for r in mr.rules
        if isapplicable(r, cat) # only one cf-rule can be isapplicable at a time
            return r(cat)
        end
    end
end

function can_create_as_prefix(mr::MetaRule, cats)
    for r in mr.rules
        if can_create_as_prefix(r, cats)
            return true
        end
    end
    return false
end

function can_create(mr::MetaRule, cats)
    for r in mr.rules
        if can_create(r, cats)
            return true
        end
    end
    return false
end

can_create(r::MetaRule, cats...) = can_create(r, cats)

function completion(mr::MetaRule, cats)
    for r in mr.rules
        if can_create(r,cats)
            return completion(r, cats)
        end
    end
    error("No completion possible.")
end

domain(mr::MetaRule) = [domain(r) for r in mr]

###################
### State class ###
###################

# States are hashed by their object ID
type State{C, CR} # category and rule
    comp    :: Vector{Tuple{C, CR}}  # completions: tuples of categories and rules
    trans   :: Dict{C, State{C, CR}} # possible transitions
    isfinal :: Bool
end

function show(io::IO, state::State)
    print(io ,"State(")
    for comp in state.comp print(io, comp, ", ") end
    for t in state.trans print(io, t, ", ") end
    println(io, state.isfinal, ")")
end

State(C, CR) = State(Vector{Tuple{C, CR}}(), Dict{C, State{C, CR}}(), true)

isfinal(state::State) = state.isfinal
completions(state::State) = state.comp
is_possible_transition(state::State, cat) = !isfinal(state) && haskey(state.trans, cat)
is_possible_transition(grammar, state, cat) = is_possible_transition(state, cat)
transition(state::State, cat) = state.trans[cat]
transition(grammar, state, cat) = transition(state, cat)

function add_rule!{C,CR}(state::State{C,CR}, rule, head, cats)
    s = state
    for c in cats
        if is_possible_transition(s, c)
            s = transition(s, c)
        else
            s.isfinal = false
            s = s.trans[c] = State(C, CR)
        end
    end
    push!(s.comp, (head, rule))
end

#####################
### Grammar Class ###
#####################

type Grammar{C, T, CR, TR, S, Cond}
    startstate    :: State{C, CR}
    startsymbols  :: Vector{C}
    terminal_dict :: Dict{T, Vector{Tuple{C, TR}}}
    rule_cond     :: Cond
    prior_cond    :: Cond # needed for GibbsSampling
end

Grammar{C, T, CR, TR, Cond}(startstate    :: State{C, CR},
                            startsymbols  :: Vector{C},
                            terminal_dict :: Dict{T, Vector{Tuple{C, TR}}},
                            rule_cond     :: Cond,
                            prior_cond    :: Cond,
                            Score         :: DataType) =
    Grammar{C, T, CR, TR, Score, Cond}(startstate,
                                       startsymbols,
                                       terminal_dict,
                                       rule_cond,
                                       prior_cond)

category_type{C, T, CR, TR, S, Cond}(grammar::Grammar{C, T, CR, TR, S, Cond}) = C
terminal_type{C, T, CR, TR, S, Cond}(grammar::Grammar{C, T, CR, TR, S, Cond}) = T
category_rule_type{C, T, CR, TR, S, Cond}(grammar::Grammar{C, T, CR, TR, S, Cond}) = CR
terminal_rule_type{C, T, CR, TR, S, Cond}(grammar::Grammar{C, T, CR, TR, S, Cond}) = TR
score_type{C, T, CR, TR, S, Cond}(grammar::Grammar{C, T, CR, TR, S, Cond}) = S
state_type(grammar) = typeof(startstate(grammar))

show{C, T, CR, TR, Cond}(io::IO, grammar::Grammar{C, T, CR, TR, Cond}) =
    print("Grammar{$C, $T, $(CR), $(TR), $(Cond)}()")

# terminal rules must be unary
function Grammar{C,T}(category_rules::Vector{MetaRule{ContextFreeRule{C,C}}},
                      terminal_rules::Vector{MetaRule{ContextFreeRule{C,T}}},
                      startsymbols,
                      Score,
                      dependent_components::Function)
    CR = MetaRule{ContextFreeRule{C,C}}
    TR = MetaRule{ContextFreeRule{C,T}}

    # rule_cond  = UrnModelCond(MetaRule[category_rules;terminal_rules], Tuple{ScaleDeg})
    # prior_cond = UrnModelCond(MetaRule[category_rules;terminal_rules], Tuple{ScaleDeg})
    meta_rules = MetaRule[category_rules;terminal_rules]
    cats = union(map(domain, meta_rules)...)
    rule_cond = SimpleCond(
        Dict(
            dependent_components(cat) => DirCat(
                Dict(
                    mr => Float64(isapplicable(mr, cat))
                    for mr in meta_rules
                )
            )
            for cat in cats
        )
    )
    prior_cond = SimpleCond(
        Dict(
            dependent_components(cat) => DirCat(
                Dict(
                    mr => Float64(isapplicable(mr, cat))
                    for mr in meta_rules
                )
            )
            for cat in cats
        )
    )

    terminal_dict = Dict{T, Vector{Tuple{C, TR}}}()
    for mr in terminal_rules
        for r in mr
            t = rhs(r)[1]
            if haskey(terminal_dict, t)
                push!(terminal_dict[t], (lhs(r), mr))
            else
                terminal_dict[t] = [(lhs(r), mr)]
            end
        end
    end

    startstate = State(C, CR)
    for mr in category_rules
        for r in mr
            add_rule!(startstate, mr, lhs(r), rhs(r))
        end
    end

    Grammar(startstate, startsymbols, terminal_dict, rule_cond, prior_cond, Score)
end

function Grammar{C,T}(category_rules::Vector{ContextFreeRule{C,C}},
                      terminal_rules::Vector{ContextFreeRule{C,T}},
                      startsymbols,
                      Score)
    CR = ContextFreeRule{C,C}
    TR = ContextFreeRule{C,T}

    # rule_cond  = PYPBackoffConditional(0.0, 1.0, ContextFreeRule[category_rules;terminal_rules])
    # prior_cond = PYPBackoffConditional(0.0, 1.0, ContextFreeRule[category_rules;terminal_rules])

    rule_cond =  UrnModelCond(ContextFreeRule[category_rules;terminal_rules], Tuple{ScaleDeg})
    prior_cond = UrnModelCond(ContextFreeRule[category_rules;terminal_rules], Tuple{ScaleDeg})

    terminal_dict = Dict{T, Vector{Tuple{C, TR}}}()
    for r in terminal_rules
        t = rhs(r)[1]
        if haskey(terminal_dict, t)
            push!(terminal_dict[t], (lhs(r), r))
        else
            terminal_dict[t] = [(lhs(r), r)]
        end
    end

    startstate = State(C, CR)
    for r in category_rules
        add_rule!(startstate, r, lhs(r), rhs(r))
    end

    Grammar(startstate, startsymbols, terminal_dict, rule_cond, prior_cond, Score)
end

function Grammar(
        category_rules_string::AbstractString,
        terminal_rules_string::AbstractString,
        startsymbols,
        Score
    )
    Grammar(
        map(split, split(category_rules_string, "\n")),
        map(split, split(terminal_rules_string, "\n")),
        startsymbols,
        Score
    )
end

function split_category_from_terminal_rules(rules_string::AbstractString)
    rule_string_lists = map(split, split(rules_string, "\n"))
    terminal_rule_stringlists = Vector{String}[]
    category_rule_stringlists = Vector{String}[]
    for lst in rule_string_lists
        if !isempty(lst)
            if lst[end][1] == '_'
                corrected_lst = [s[1] == '_' ? s[2:end] : s for s in lst]
                push!(terminal_rule_stringlists, corrected_lst)
            else
                push!(category_rule_stringlists, lst)
            end
        end
    end
    category_rule_stringlists, terminal_rule_stringlists
end

function Grammar(rules_string::AbstractString, startsymbols, Score=LogProb)
    category_rules, terminal_rules = split_category_from_terminal_rules(rules_string)
    Grammar(category_rules, terminal_rules, startsymbols, Score)
end

function Grammar{S<:AbstractString, T<:AbstractString}(
        category_rules_stringlists::Vector{Vector{S}},
        terminal_rules_stringlists::Vector{Vector{T}},
        startsymbols,
        Score
    )
    if parse(category_rules_stringlists[1][1]) isa Symbol
        category_rules = [
            ContextFreeRule{String, String}(s[1], (s[2:end]...))
            for s in category_rules_stringlists
        ]
        terminal_rules = [
            ContextFreeRule{String, String}(s[1], (s[2:end]...))
            for s in terminal_rules_stringlists
        ]
        all_rules = [category_rules; terminal_rules]
        nonterminal_cats = unique(lhs.(all_rules))
        rule_cond = SimpleCond(
            Dict(
                cat => DirCat(
                    Dict(
                        rule => 1.0
                        for rule in all_rules
                        if lhs(rule) == cat
                    )
                )
            for cat in nonterminal_cats
            )
        )
        prior_cond = SimpleCond(
            Dict(
                cat => DirCat(
                    Dict(
                        rule => 1.0
                        for rule in all_rules
                        if lhs(rule) == cat
                    )
                )
            for cat in nonterminal_cats
            )
        )
        terminal_dict = Dict{String, Vector{Tuple{String, ContextFreeRule{String,String}}}}()
        for r in terminal_rules
            t = rhs(r)[1]
            if haskey(terminal_dict, t)
                push!(terminal_dict[t], (lhs(r), r))
            else
                terminal_dict[t] = [(lhs(r), r)]
            end
        end

        startstate = State(String, ContextFreeRule{String,String})
        for r in category_rules
            add_rule!(startstate, r, lhs(r), rhs(r))
        end
    else
        category_rules_with_probs = [
            (ContextFreeRule{String,String}(s[2],(s[3:end]...)), LogProb(eval(parse(s[1]))))
            for s in category_rules_stringlists
        ]
        terminal_rules_with_probs = [
            (ContextFreeRule{String,String}(s[2],(s[3:end]...)), LogProb(eval(parse(s[1]))))
            for s in terminal_rules_stringlists
        ]
        all_rules_with_probs = [category_rules_with_probs;terminal_rules_with_probs]
        rule_cond = SimpleCond(Dict(
            cat => CatDist(Dict(
                r => p for (r,p) in all_rules_with_probs if lhs(r) == cat
            ))
            for cat in unique(map(x->lhs(x[1]), all_rules_with_probs))
        ))
        prior_cond = SimpleCond(Dict(
            cat => CatDist(Dict(
                r => p for (r,p) in all_rules_with_probs if lhs(r) == cat
            ))
            for cat in unique(map(x->lhs(x[1]), all_rules_with_probs))
        ))

        terminal_dict = Dict{String, Vector{Tuple{String, ContextFreeRule{String,String}}}}()
        for (r,p) in terminal_rules_with_probs
            t = rhs(r)[1]
            if haskey(terminal_dict, t)
                push!(terminal_dict[t], (lhs(r), r))
            else
                terminal_dict[t] = [(lhs(r), r)]
            end
        end

        startstate = State(String, ContextFreeRule{String,String})
        for (r,p) in category_rules_with_probs
            add_rule!(startstate, r, lhs(r), rhs(r))
        end
    end

    Grammar(startstate, startsymbols, terminal_dict, rule_cond, prior_cond, Score)
end

dependent_components(str::String) = str

# Score == LogProb
completions{C, T, CR, TR, Cond}(grammar::Grammar{C, T, CR, TR, LogProb, Cond}, state::State) =
    ((cat, mr, prob(grammar, cat, mr)) for (cat, mr) in completions(state))
completions{C, T, CR, TR, Cond}(grammar::Grammar{C, T, CR, TR, LogProb, Cond}, t::T) =
    ((cat, rule, prob(grammar, cat, rule)) for (cat, rule) in grammar.terminal_dict[t])

# Score is a Integer
completions{C, T, CR, TR, S <: Integer, Cond}(grammar::Grammar{C, T, CR, TR, S, Cond}, state::State) =
    ((cat, mr, one(S)) for (cat, mr) in completions(state))
completions{C, T, CR, TR, S <: Integer, Cond}(grammar::Grammar{C, T, CR, TR, S, Cond}, t::T) =
    ((cat, rule, one(S)) for (cat, rule) in grammar.terminal_dict[t])

# Score == Product{LogProb,BigInt}
completions{C, T, CR, TR, Cond}(grammar::Grammar{C, T, CR, TR, Product{LogProb,BigInt}, Cond}, state::State) =
    ((cat, mr, Product(prob(grammar, cat, mr),BigInt(1))) for (cat, mr) in completions(state))
completions{C, T, CR, TR, Cond}(grammar::Grammar{C, T, CR, TR, Product{LogProb,BigInt}, Cond}, t::T) =
    ((cat, rule, Product(prob(grammar, cat, rule),BigInt(1))) for (cat, rule) in grammar.terminal_dict[t])

startstate(grammar::Grammar) = grammar.startstate
startsymbols(grammar::Grammar) = grammar.startsymbols
prob(grammar::Grammar, cat, rule) =
    logscore(grammar.rule_cond, rule, dependent_components(cat))
priorprob(grammar::Grammar, cat, rule) =
    logscore(grammar.prior_cond, rule, dependent_components(cat))
add_obs!(grammar::Grammar, rule, cat) =
    add_obs!(grammar.rule_cond, rule, dependent_components(cat))
remove_obs!(grammar::Grammar, obs) =
    remove_obs!(grammar.rule_cond, obs)
add_obs!(grammar::Grammar, parse_tree) =
    # Customer[add_obs!(grammar, rule, cat) for (cat, rule) in cats_and_rules(parse_tree)]
    [add_obs!(grammar, rule, cat) for (cat, rule) in cats_and_rules(parse_tree)]
remove_obs!(grammar::Grammar, tree_obs::Vector) =
    for obs in tree_obs remove_obs!(grammar, obs) end
logscore(grammar::Grammar, parse_tree) =
    sum(prob(grammar, cat, rule) for (cat,rule) in cats_and_rules(parse_tree))
priorlogscore(grammar::Grammar, parse_tree) =
    sum(priorprob(grammar, cat, rule) for (cat,rule) in cats_and_rules(parse_tree))
