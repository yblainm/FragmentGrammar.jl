# include("newtrees.jl")
# using Trees
#
# using AccessMacro
# using Missings
# using LogProbs
# using DataStructures
#
# using StatsFuns.RFunctions: gammarand
#
# using LightGraphs: Graph, add_vertex!, add_edge!, nv
#
# import TikzGraphs: plot
# import Base: show, hash, ==
# import Base: +, -, *, /, zero, one, <
# import Base: start, length, insert!, isempty, convert, getindex, promote_rule, range
# import DataStructures: enqueue!, dequeue!

prob(p::LogProb) = p
# prob(s::Product) = first(s)

##############
### ModInt ###
##############

struct ModInt{n} <: Number
  val::Int
  ModInt{n}(val) where {n} = new(mod(val,n))
end

show{n}(io::IO, a::ModInt{n}) = print(io, "$(a.val) mod $n")

+{n}(a::ModInt{n}, b::ModInt{n}) = ModInt{n}(a.val + b.val)
-{n}(a::ModInt{n}) = n - a.val
-{n}(a::ModInt{n}, b::ModInt{n}) = ModInt{n}(a.val - b.val)
*{n}(a::ModInt{n}, b::ModInt{n}) = ModInt{n}(a.val * b.val)
/{n}(a::ModInt{n}, b::ModInt{n}) = a * invmod(b, n)

<{n}(a::ModInt{n}, b::ModInt{n}) = a.val < b.val

one{n}(a::ModInt{n}) = ModInt{n}(1)
zero{n}(a::ModInt{n}) = ModInt{n}(0)

convert{n}(::Type{ModInt{n}}, x::Int) = ModInt{n}(x)
convert{n}(::Type{Int}, x::ModInt{n}) = x.val

getindex{n}(t::Union{Tuple, Array}, i::ModInt{n}) = getindex(t, i.val + 1)

promote_rule{n}(::Type{ModInt{n}}, ::Type{Int}) = ModInt{n}

############################################
### Generic Categorical Sampling Methods ###
############################################

function categorical_sample(tokens, weights)
    x = rand() * sum(weights)
    cum_weights = zero(eltype(weights))
    for (t, w) in zip(tokens, weights)
        cum_weights += w
        if cum_weights >= x
            return t
        end
    end
    error("categorical sample failed with length(tokens)=$(length(tokens)), weights=$weights")
end

categorical_sample(d::Dict) = categorical_sample(keys(d), values(d))
categorical_sample(v::Vector) = categorical_sample(1:length(v), v)

################################
### Conditional Distribution ###
################################

struct SimpleCond{C, D, S} # context, distribution, support
    dists   :: Dict{C, D}
    support :: S
    SimpleCond(dists::Dict{C, D}, support::S) where {C, D, S} =
        new{C, D, S}(dists, unique(support))
end

function SimpleCond(dists::Associative)
    SimpleCond(
        dists,
        vcat([collect(support(dist)) for dist in values(dists)]...)
    )
end

sample(sc::SimpleCond, context, args...) = sample(sc.dists[context], args...)
logscore(sc::SimpleCond, obs, context) = logscore(sc.dists[context], obs)
rm_obs!(sc::SimpleCond, obs, context) = rm_obs!(sc.dists[context], obs)

score_type(::SimpleCond) = LogProb

function add_obs!{C,D,S}(cond::SimpleCond{C,D,S}, obs, context)
    if !haskey(cond.dists, context)
        cond.dists[context] = D(cond.support)
    end
    add_obs!(cond.dists[context], obs)
end

#############################
### Dirichlet Multinomial ###
#############################

abstract type Distribution{T} end

mutable struct DirCat{T, C} <: Distribution{T}
    counts :: Dict{T, C}
end

DirCat(support, priors) = DirCat(Dict(x => p for (x,p) in zip(support, priors)))
support(dc::DirCat) = keys(dc.counts)

function sample(dc::DirCat)
    weights = [gammarand(c, 1) for c in values(dc.counts)]
    categorical_sample(keys(dc.counts), weights)
end

function logscore(dc::DirCat, obs)
    LogProb(lbeta(sum(values(dc.counts)), 1) - lbeta(dc.counts[obs], 1))
end

function add_obs!(dc::DirCat, obs)
    dc.counts[obs] += 1
end

function rm_obs!(dc::DirCat, obs)
    dc.counts[obs] -= 1
end

##############
### CFRule ###
##############

mutable struct RunningCounter
    n :: Int
end

RunningCounter() = RunningCounter(0)
count!(c::RunningCounter) = c.n += 1

rule_counter = RunningCounter()

struct CFRule{LHS, RHS} # left hand side and right hand side of the rule
    mappings ::Dict{LHS, Vector{RHS}}
    name :: Symbol
end

==(r1::CFRule, r2::CFRule) = r1.name == r2.name
hash(r::CFRule, h::UInt) = hash(hash(CFRule, hash(r.name)), h)

Base.show(io::IO, r::CFRule) = print(io, "CFRule($(r.name))")

CFRule(pairs::Pair...) =
    CFRule(Dict(pairs...), Symbol("rule", count!(rule_counter)))
CFRule(g::Base.Generator) =
    CFRule(Dict(g), Symbol("rule", count!(rule_counter)))
CFRule(f::Function, lhss, name) =
    CFRule(Dict(lhs => f(lhs) for lhs in lhss), name)
CFRule(f::Function, lhss) =
    CFRule(Dict(lhs => f(lhs) for lhs in lhss), Symbol("rule", count!(rule_counter)))

lhss(r::CFRule) = keys(r.mappings) # aka domain
isapplicable(r::CFRule, lhs) = haskey(r.mappings, lhs)
(r::CFRule)(lhs) = r.mappings[lhs]

###############
### CFState ###
###############

mutable struct CompletionAutomaton{Cat,Comp} # category, completion
    transitions :: Vector{Dict{Cat, Int}}
    completions :: Vector{Vector{Comp}}
end

CompletionAutomaton(Cat::Type, Comp::Type) =
    CompletionAutomaton([Dict{Cat, Int}()], [Vector{Comp}()])

number_of_states(ca::CompletionAutomaton) = length(ca.transitions)
isfinal(ca::CompletionAutomaton, s) = isempty(ca.transitions[s])
is_possible_transition(ca::CompletionAutomaton, s, c) = haskey(ca.transitions[s], c)
transition(ca::CompletionAutomaton, s, c) = ca.transitions[s][c]
completions(ca::CompletionAutomaton, s) = ca.completions[s]

function add_completion!{Cat,Comp}(ca::CompletionAutomaton{Cat,Comp}, comp, categories)
    s = 1
    for c in categories
        if is_possible_transition(ca, s, c)
            s = transition(ca, s, c)
        else
            push!(ca.transitions, Dict{Cat,Int}())
            push!(ca.completions, Vector{Comp}())
            s = ca.transitions[s][c] = number_of_states(ca)
        end
    end
    push!(ca.completions[s], comp)
end

function add_rule!(ca::CompletionAutomaton, r::CFRule)
    for lhs in lhss(r)
        add_completion!(ca, (lhs, r), r(lhs))
    end
end

# mutable struct CFState{C, R} # category and rule
#     completions   :: Vector{Tuple{C, R}}
#     transitions   :: Dict{C, CFState{C, R}}
#     hastransition :: Bool
# end
#
# CFState(C::Type, R::Type) =
#     CFState(Vector{Tuple{C, R}}(), Dict{C, CFState{C, R}}(), false)
#
# hastransition(s::CFState) = s.hastransition
# isfinal(s::CFState) = !s.hastransition
# isfinal(s::Ptr) = !unsafe_pointer_to_objref(s).hastransition ::Bool
#
# is_possible_transition(s::CFState, c) = hastransition(s) && haskey(s.transitions, c)
# transition(s::CFState, c) = s.transitions[c]
#
# function add_rule!(state::CFState{C, R}, r::R) where {C, R}
#     for lhs in lhss(r)
#         s = state
#         for c in r(lhs)
#             if is_possible_transition(s, c)
#                 s = transition(s, c)
#             else
#                 s.hastransition = true
#                 s = s.transitions[c] = CFState(C, R)
#             end
#         end
#         push!(s.completions, (lhs, r))
#     end
# end

#################
### CFGrammar ###
#################

struct CFGrammar{C, T, Cond, F}
    comp_automtn  :: CompletionAutomaton{C, Tuple{C, CFRule{C, C}}}
    startsymbols  :: Vector{C}
    terminal_dict :: Dict{T, Vector{Tuple{C, CFRule{C, T}}}}
    cond          :: Cond # conditional scoring
    dependent_components::F
end

function CFGrammar{C, T}(
        category_rules::Vector{CFRule{C, C}},
        terminal_rules::Vector{CFRule{C, T}},
        startsymbols  ::Vector{C},
        dependent_components=identity::Function
    )
    comp_automtn = CompletionAutomaton(C, Tuple{C, CFRule{C, C}})
    for r in category_rules
        add_rule!(comp_automtn, r)
    end

    terminal_dict = Dict{T, Vector{Tuple{C, CFRule{C, T}}}}()
    for r in terminal_rules
        for lhs in lhss(r)
        t = r(lhs)[1]
            if haskey(terminal_dict, t)
                push!(terminal_dict[t], (lhs, r))
            else
                terminal_dict[t] = [(lhs, r)]
            end
        end
    end

    applicable_rules = Dict{C, Vector{CFRule}}()
    for r in CFRule[category_rules; terminal_rules]
        for c in lhss(r)
            if haskey(applicable_rules, c)
                push!(applicable_rules[c], r)
            else
                applicable_rules[c] = CFRule[r]
            end
        end
    end

    cond = SimpleCond(
        Dict(
            dependent_components(c) => let rules = applicable_rules[c]
                n = length(rules)
                k = count(isa.(rules, CFRule{C, T})) # number terminal rules
                DirCat(rules, [fill(1.0, n-k); fill(1/k, k)])
            end
            for c in keys(applicable_rules)
        )
    )

    CFGrammar(comp_automtn, startsymbols, terminal_dict, cond, dependent_components)
end

dependent_components(g::CFGrammar, c) = g.dependent_components(c)

startstate(g::CFGrammar) = 1
startsymbols(g::CFGrammar) = g.startsymbols

isfinal(g::CFGrammar, s) = isfinal(g.comp_automtn, s)
is_possible_transition(g::CFGrammar, s, c) = is_possible_transition(g.comp_automtn, s, c)
transition(g::CFGrammar, s, c) = transition(g.comp_automtn, s, c)

completions(g::CFGrammar, s::Int) =
    ((c, r, score(g, c, r)) for (c, r) in completions(g.comp_automtn, s))
completions(g::CFGrammar, t) =
    ((c, r, score(g, c, r)) for (c, r) in g.terminal_dict[t])

score(g::CFGrammar, c, r) = logscore(g.cond, r, dependent_components(g, c))

@inline function types{C, T, Cond}(grammar::CFGrammar{C, T, Cond})
    C, T, CFRule{C, C}, CFRule{C, T}, Int, LogProb
end

#############
### Range ###
#############

abstract type ItemRange end

ItemRange(s::Int, e::Int, n::Int, cyclic::Bool) =
    cyclic ? CyclicRange(s, e, n) : IntervalRange(s, e)

start(r::ItemRange) = r.start
_end(r::ItemRange)  = r._end

struct IntervalRange <: ItemRange
    start :: Int
    _end  :: Int
end

length(r::IntervalRange) = _end(r) - start(r)
concatenable(r1::IntervalRange, r2::IntervalRange) = _end(r1) == start(r2)

function *(r1::IntervalRange, r2::IntervalRange)
    @assert concatenable(r1, r2)
    IntervalRange(start(r1), _end(r2))
end

struct CyclicRange{n} <: ItemRange
    start  :: ModInt{n}
    _end   :: ModInt{n}
    length :: Int
end
CyclicRange{n}(s::ModInt{n}, e::ModInt{n}) = CyclicRange(s, e, Int(e-s))
CyclicRange(s::Int, e::Int, n::Int) = CyclicRange(ModInt{n}(s), ModInt{n}(e))

length(r::CyclicRange) = r.length

@inline function concatenable(r1::CyclicRange{n}, r2::CyclicRange{n}) where n
    _end(r1) == start(r2) && length(r1) + length(r2) <= n
end

function *(r1::CyclicRange, r2::CyclicRange)
    @assert concatenable(r1, r2)
    CyclicRange(start(r1), _end(r2), length(r1) + length(r2))
end

# tests
# isbits(CyclicRange{10})
# length(CyclicRange(4, 2, 10)) == 8
# concatenable(CyclicRange(2, 4, 7), CyclicRange(4, 5, 7))
# CyclicRange(2, 4, 7) * CyclicRange(4, 5, 7) == CyclicRange(2, 5, 7)
#
# !concatenable(CyclicRange(2, 4, 7), CyclicRange(5, 6, 7))
# concatenable(CyclicRange(2, 4, 7), CyclicRange(4, 1, 7))
# concatenable(CyclicRange(2, 4, 7), CyclicRange(4, 2, 7))
# !concatenable(CyclicRange(2, 4, 7), CyclicRange(4, 3, 7))

###################
### Completions ###
###################

@access struct TerminalCompletion{T,TR,S}
    terminal :: T
    rule     :: TR
    score    :: S
end

@access struct EdgeCompletion{E,CR,S}
    edge   :: E
    rule   :: CR
    score  :: S
    inloop :: Bool
end
EdgeCompletion(edge, rule, score) = EdgeCompletion(edge, rule, score, false)
@inline function ==(c1::EdgeCompletion, c2::EdgeCompletion)
    c1.edge == c2.edge && c1.rule == c2.rule
end

#################
### Traversal ###
#################

struct Traversal{E,CO,S}
    edge   :: Nullable{E}
    cont   :: CO
    score  :: S
    inloop :: Bool
end
Traversal(edge, cont, score) = Traversal(Nullable(edge), cont, score, false)
Traversal(edge, cont) = Traversal(Nullable(edge), cont, score(edge) * score(cont), false)
hasedge(trav::Traversal) = !isnull(trav.edge)
edge(trav::Traversal) = get(trav.edge)
cont(trav::Traversal) = trav.cont
score(trav::Traversal) = trav.score
inloop(trav::Traversal) = trav.inloop

@inline function ==(t1::Traversal, t2::Traversal)
    if hasedge(t1)
        if hasedge(t2)
            get(t1.edge) == get(t2.edge) && t1.cont == t2.cont
        else
            false
        end
    else
        if hasedge(t2)
            false
        else
            t1.cont == t2.cont
        end
    end
end

###############
### ItemKey ###
###############

abstract type ItemKey{R} end

range(k::ItemKey)  =          k.range
start(k::ItemKey)  =  start(range(k))
_end(k::ItemKey)   =   _end(range(k))
length(k::ItemKey) = length(range(k))

# ==(k1::ItemKey, k2::ItemKey) =
#     getfield(k1,1) == getfield(k2,1) && getfield(k1,2) == getfield(k2,2)
# hash(k::K) where K <: ItemKey =
#     hash(K, hash(getfield(k,1), hash(getfield(k,2))))

struct ConstituentKey{R,C} <: ItemKey{R}
    range    :: R
    category :: C
end
category(k::ConstituentKey) = k.category

struct EdgeKey{R,ST} <: ItemKey{R}
    range :: R
    state :: ST
end
state(k::EdgeKey) = k.state

############
### Item ###
############

abstract type Item end

Item(key, trav::Traversal) = Edge(key, trav)
Item(key, comp::EdgeCompletion) = Constituent(key, comp)

key(item::Item) = item.key
range(item::Item) = range(key(item))
start(item::Item) = start(range(item))
_end(item::Item) = _end(range(item))
length(item::Item) = length(range(item))
isfinished(item::Item) = !(isnull(item.score))
lastpopscore(item::Item) = item.lastpopscore
insidepopnumber(item::Item) = item.insidepopnumber

############
### Edge ###
############

mutable struct Edge{R,ST,S,CO} <: Item
    key             :: EdgeKey{R,ST}
    score           :: Nullable{S}
    lastpopscore    :: S
    insidepopnumber :: Int
    traversals      :: Vector{Traversal{Edge{R,ST,S,CO},CO,S}}
end

@inline function Edge{E,CO,S}(key, trav::Traversal{E,CO,S})
    Edge(key, Nullable{S}(), zero(S), 0, [trav])
end

state(edge::Edge) = state(key(edge))
traversals(edge::Edge) = edge.traversals

@inline function score(edge::Edge)
    if isfinished(edge)
        get(edge.score)
    else
        sum(score(trav) for trav in edge.traversals)
    end
end

function add!(edge::Edge, trav)
    i = findfirst(t->t==trav, edge.traversals)
    if i != 0
        edge.traversals[i] = trav
    else
        push!(edge.traversals, trav)
    end
    nothing
end

###################
### Constituent ###
###################

mutable struct Constituent{R,C,T,CR,TR,ST,S} <: Item
    key                 :: ConstituentKey{R,C}
    score               :: Nullable{S}
    lastpopscore        :: S
    insidepopnumber     :: Int
    terminal_completion :: Nullable{TerminalCompletion{T,TR,S}}
    completions         :: Vector{EdgeCompletion{Edge{R,ST,S,Constituent{R,C,T,CR,TR,ST,S}},CR,S}}
end

@inline function Constituent{R,C}(
        key::ConstituentKey{R,C}, comp::TerminalCompletion, grammar
    )
    C_, T, CR, TR, ST, S = types(grammar)
    Constituent(
        key, Nullable{S}(), zero(S), 0, Nullable(comp),
        Vector{EdgeCompletion{Edge{R,ST,S,Constituent{R,C,T,CR,TR,ST,S}},CR,S}}()
    )
end

@inline function Constituent{R,C,T,CR,TR,ST,S}(
        key,
        comp :: EdgeCompletion{Edge{R,ST,S,Constituent{R,C,T,CR,TR,ST,S}},CR,S}
    )
    Constituent(
        key, Nullable{S}(), zero(S), 0,
        Nullable{TerminalCompletion{T,TR,S}}(),
        [comp]
    )
end

category(cont::Constituent) = category(key(cont))
completions(cont::Constituent) = cont.completions

hasterminal(cont::Constituent) = !(isnull(cont.terminal_completion))
terminal_completion(cont::Constituent) = get(cont.terminal_completion)
terminal(cont::Constituent) = get(cont.terminal_completion).terminal

@inline function score(cont::Constituent)
    if isfinished(cont)
        get(cont.score)
    else
        if hasterminal(cont)
            if isempty(completions(cont))
                score(terminal_completion(cont))
            else
                +(
                    score(terminal_completion(cont)),
                    sum(score(comp) for comp in completions(cont))
                )
            end
        else
            sum(score(comp) for comp in completions(cont))
        end
    end
end

function add!(cont::Constituent, comp)
    i = findfirst(c->c==comp, cont.completions)
    if i != 0
        cont.completions[i] = comp
    else
        push!(cont.completions, comp)
    end
    nothing
end

@inline function Traversal{R,C,T,CR,TR,ST,S}(cont::Constituent{R,C,T,CR,TR,ST,S})
    Traversal(Nullable{Edge{R,ST,S,Constituent{R,C,T,CR,TR,ST,S}}}(), cont, score(cont), false)
end

#####################
### ParserLogbook ###
#####################

struct ParserLogbook{R,C,T,CR,TR,ST,S}
    edges :: Dict{EdgeKey{R,ST}, Edge{R,ST,S,Constituent{R,C,T,CR,TR,ST,S}}}
    conts :: Dict{ConstituentKey{R,C}, Constituent{R,C,T,CR,TR,ST,S}}
end

@inline function ParserLogbook(grammar, n::Int, cyclic::Bool)
    R = cyclic ? CyclicRange{n} : IntervalRange
    C,T,CR,TR,ST,S = types(grammar)
    ParserLogbook(
        Dict{EdgeKey{R,ST}, Edge{R,ST,S,Constituent{R,C,T,CR,TR,ST,S}}}(),
        Dict{ConstituentKey{R,C}, Constituent{R,C,T,CR,TR,ST,S}}()
    )
end
discover!(logbook::ParserLogbook, edge::Edge) =
    logbook.edges[key(edge)] = edge
discover!(logbook::ParserLogbook, cont::Constituent) =
    logbook.conts[key(cont)] = cont
isdiscovered(logbook, key::EdgeKey) = haskey(logbook.edges, key)
isdiscovered(logbook, key::ConstituentKey) = haskey(logbook.conts, key)
getitem(logbook, key::EdgeKey) = logbook.edges[key]
getitem(logbook, key::ConstituentKey) = logbook.conts[key]

##################
### ParseChart ###
##################

struct ChartCell{R,C,T,CR,TR,ST,S}
    edges :: Dict{ST, Vector{Edge{R,ST,S,Constituent{R,C,T,CR,TR,ST,S}}}}
    conts :: Dict{C, Vector{Constituent{R,C,T,CR,TR,ST,S}}}
end
@inline function ChartCell(grammar, n::Int, cyclic::Bool)
    R = cyclic ? CyclicRange{n} : IntervalRange
    C,T,CR,TR,ST,S = types(grammar)
    ChartCell(
        Dict{ST, Vector{Edge{R,ST,S,Constituent{R,C,T,CR,TR,ST,S}}}}(),
        Dict{C, Vector{Constituent{R,C,T,CR,TR,ST,S}}}()
    )
end

struct ParseChart{R,C,T,CR,TR,ST,S}
    cells :: Vector{ChartCell{R,C,T,CR,TR,ST,S}}
end
# vector indices begin with 1
# item   indices begin with 0

edges(chart::ParseChart, edge::Edge)        = chart.cells[ _end(edge)+1].edges
edges(chart::ParseChart, cont::Constituent) = chart.cells[start(cont)+1].edges
conts(chart::ParseChart, edge::Edge)        = chart.cells[ _end(edge)+1].conts
conts(chart::ParseChart, cont::Constituent) = chart.cells[start(cont)+1].conts

@inline function push_or_init!(d::Dict, k, v)
    if haskey(d, k)
        push!(d[k], v)
    else
        d[k] = [v]
    end
end
insert!(chart::ParseChart, edge::Edge) =
    push_or_init!(edges(chart, edge), state(edge), edge)
insert!(chart::ParseChart, cont::Constituent) =
    push_or_init!(conts(chart, cont), category(cont), cont)

####################
### InsideAgenda ###
####################

struct InsideAgenda{R,C,T,CR,TR,ST,S}
    edge_queue :: PriorityQueue{Edge{R,ST,S,Constituent{R,C,T,CR,TR,ST,S}}, Int, Base.Order.ForwardOrdering}
    cont_queue :: PriorityQueue{Constituent{R,C,T,CR,TR,ST,S}, Int, Base.Order.ForwardOrdering}
end
function InsideAgenda(grammar, n::Int, cyclic)
    R = cyclic ? CyclicRange{n} : IntervalRange
    C,T,CR,TR,ST,S = types(grammar)
    InsideAgenda(
        PriorityQueue{Edge{R,ST,S,Constituent{R,C,T,CR,TR,ST,S}}, Int}(),
        PriorityQueue{Constituent{R,C,T,CR,TR,ST,S}, Int}()
    )
end
@inline function enqueue!(agenda::InsideAgenda, edge::Edge, just_used)
    agenda.edge_queue[edge] = priority(edge, just_used)
end
@inline function enqueue!(agenda::InsideAgenda, cont::Constituent, just_used)
    agenda.cont_queue[cont] = priority(cont, just_used)
end
@inline function next_is_edge(agenda::InsideAgenda)
    isempty(agenda.cont_queue) || !isempty(agenda.edge_queue) && peek(agenda.edge_queue)[2] < peek(agenda.cont_queue)[2]
end
dequeue_edge!(agenda::InsideAgenda)    = dequeue!(agenda.edge_queue)
dequeue_cont!(agenda::InsideAgenda)    = dequeue!(agenda.cont_queue)
isempty(agenda::InsideAgenda)          = isempty(agenda.edge_queue) && isempty(agenda.cont_queue)
priority(edge::Edge, just_used)        = 4 * length(edge) - 2*!(just_used) - 1
priority(cont::Constituent, just_used) = 4 * length(cont) - 2*!(just_used)

###################
### ParseForest ###
###################

@access struct ParseForest{R,C,T,CR,TR,ST,S}
    heads     :: Vector{Constituent{R,C,T,CR,TR,ST,S}}
    terminals :: Vector{T}
end
function ParseForest(chart::ParseChart, terminals, grammar, cyclic)
    if cyclic
        ParseForest(
            vcat(
                [
                    vcat(
                        map(keys(cell.conts)) do category
                            filter(cell.conts[category]) do cont
                                length(cont) == length(terminals) &&
                                    category in startsymbols(grammar)
                            end
                        end...
                    )
                    for cell in chart.cells
                ]...
            )
            ,
            terminals
        )
    else
        ParseForest(
            vcat(
                map(keys(chart.cells[1].conts)) do category
                    filter(chart.cells[1].conts[category]) do cont
                        length(cont) == length(terminals) &&
                            category in startsymbols(grammar)
                    end
                end...
            ),
            terminals
        )
    end
end
iscomplete(forest::ParseForest) = !isempty(forest.heads)
score(forest::ParseForest) = sum(score(h) for h in forest.heads)

probscore = prob ∘ score

function sample_tree(forest::ParseForest)
    @assert iscomplete(forest)
    head = categorical_sample(heads(forest), probscore.(heads(forest)))
    ParseTree(head, sample_tree(head))
end

function sample_conts(edge::Edge)
    trav = categorical_sample(traversals(edge), probscore.(traversals(edge)))
    if hasedge(trav)
        [sample_conts(get(trav.edge)); trav.cont]
    else
        [cont(trav)]
    end
end

function sample_tree{R,C,T,CR,TR,ST,S}(cont::Constituent{R,C,T,CR,TR,ST,S})
    if hasterminal(cont)
        if isempty(completions(cont))
            comp = terminal_completion(cont)
            [(category(cont), rule(comp))]
        else
            comp = categorical_sample(
                [terminal_completion(cont); completions(cont)],
                [probscore(terminal_completion(cont)); probscore.(completions(cont))]
            )
            if comp isa TerminalCompletion
                [(category(cont), rule(comp))]
            else
                conts = sample_conts(edge(comp))
                [(category(cont), rule(comp)); vcat([sample_tree(cont) for cont in conts]...)]
            end
        end
    else
        comp = categorical_sample(completions(cont), probscore.(completions(cont)))
        conts = sample_conts(edge(comp))
        [(category(cont), rule(comp)); vcat([sample_tree(cont) for cont in conts]...)]
    end
end

best_choice(tokens, weights) = tokens[findmax(weights)[2]]

function best_tree(forest::ParseForest)
    @assert iscomplete(forest)
    head = best_choice(heads(forest), probscore.(heads(forest)))
    ParseTree(head, best_tree(head))
end

function best_conts(edge::Edge)
    trav = best_choice(traversals(edge), probscore.(traversals(edge)))
    if hasedge(trav)
        [best_conts(get(trav.edge)); trav.cont]
    else
        [cont(trav)]
    end
end

function best_tree{R,C,T,CR,TR,ST,S}(cont::Constituent{R,C,T,CR,TR,ST,S})
    if hasterminal(cont)
        if isempty(completions(cont))
            comp = terminal_completion(cont)
            [(category(cont), rule(comp))]
        else
            comp = best_choice(
                [terminal_completion(cont); completions(cont)],
                [probscore(terminal_completion(cont)); probscore.(completions(cont))]
            )
            if comp isa TerminalCompletion
                [(category(cont), rule(comp))]
            else
                conts = best_conts(edge(comp))
                [(category(cont), rule(comp)); vcat([best_tree(cont) for cont in conts]...)]
            end
        end
    else
        comp = best_choice(completions(cont), probscore.(completions(cont)))
        conts = best_conts(edge(comp))
        [(category(cont), rule(comp)); vcat([best_tree(cont) for cont in conts]...)]
    end
end

#################
### ParseTree ###
#################

struct ParseTree{C,T,CR,TR,ST,S,R}
    head  :: Constituent{C,T,CR,TR,ST,S}
    rules :: Vector{R}
end

# function add_obs!(g::CFGrammar, t::ParseTree)
#     for (c, r) in t.rules
#         add_obs!(g.cond, r, dependent_components(g, c))
#     end
# end
#
# function rm_obs!(g::CFGrammar, t::ParseTree)
#     for (c, r) in t.rules
#         rm_obs!(g.cond, r, dependent_components(g, c))
#     end
# end

function show(io::IO, tree::ParseTree)
    print(io, map(first, convert(Tree, tree), uniform_type=false))
end

function convert{R,C,T,CR,TR,ST,S}(::Type{Tree}, tree::ParseTree{R,C,T,CR,TR,ST,S})
    function foo!(nodes, rules)
        if isempty(rules)
            nothing
        else
            c, rule = rules[1]
            node = nodes[1]
            category = node.data[1]
            if isapplicable(rule, category)
                for c in rule(category)
                    add_child!(node, (c, Nullable{typejoin(CR,TR)}()))
                end
                node.data = (node.data[1], Nullable(rule))
                foo!([children(node); nodes[2:end]], rules[2:end])
            else
                foo!(nodes[2:end], rules)
            end
        end
    end
    root = Tree(
        (category(tree.head), Nullable{typejoin(CR,TR)}()),
        Tuple{typejoin(C,T), Nullable{typejoin(CR,TR)}}
    )
    foo!([root], tree.rules)
    root
end

latex_replacements = Dict(
    "^" => "^\\triangle",
    "#" => "\#"
)

function latex_replace(str)
    s = str
    for (k, v) in latex_replacements
        s = replace(s, k, v)
    end
    s
end

function tree_to_graph_and_labels(tree)
    graph = Graph(1)
    labels = [string('$', latex_replace(string(tree.data)), '$')]
    for (i, c) in enumerate(children(tree))
        add_vertex!(graph)
        add_edge!(graph, 1, nv(graph))
        tree_to_graph_and_labels!(c, graph, labels)
    end
    graph, labels
end

function tree_to_graph_and_labels!(tree, graph, labels)
    n = nv(graph)
    push!(labels, string('$', latex_replace(string(tree.data)), '$'))
    for (i, c) in enumerate(children(tree))
        add_vertex!(graph)
        add_edge!(graph, n, nv(graph))
        tree_to_graph_and_labels!(c, graph, labels)
    end
    graph, labels
end

plot(tree::Tree) = plot(tree_to_graph_and_labels(tree)...)
plot(tree::ParseTree) = plot(map(first, convert(Tree, tree), uniform_type=false))

######################
### Parser Methods ###
######################

function create_or_update!(key, trav_or_comp, agenda, logbook)
    if isdiscovered(logbook, key)
        item = getitem(logbook, key)
        add!(item, trav_or_comp)
    else
        item = Item(key, trav_or_comp)
        discover!(logbook, item)
    end
    enqueue!(agenda, item, false)
    nothing
end

@noinline function initialize(terminals, grammar, epsilon, cyclic)
    n       = length(terminals)
    chart   = ParseChart([ChartCell(grammar, n, cyclic) for i in 0:(cyclic ? n-1 : n)])
    agenda  = InsideAgenda(grammar, n, cyclic)
    logbook = ParserLogbook(grammar, n, cyclic)

    for (i, terminal) in enumerate(terminals)
        for (category, rule, score) in completions(grammar, terminal)
            cont = Constituent(
                ConstituentKey(ItemRange(i-1, i, n, cyclic), category),
                TerminalCompletion(terminal, rule, score),
                grammar
            )
            discover!(logbook, cont)
            enqueue!(agenda, cont, false)
        end
        if !ismissing(epsilon)
            for (category, rule, score) in completions(grammar, epsilon)
                cont = Constituent(
                    ConstituentKey(ItemRange(i-1, i-1, n, cyclic), category),
                    TerminalCompletion(epsilon, rule, score),
                    grammar
                )
                discover!(logbook, cont)
                enqueue!(agenda, cont, false)
            end
        end
    end
    if !ismissing(epsilon) && !cyclic
        for (category, rule, score) in completions(grammar, epsilon)
            cont = Constituent(
                ConstituentKey(ItemRange(n, n, n, cyclic), category),
                TerminalCompletion(epsilon, rule, score),
                grammar
            )
            discover!(logbook, cont)
            enqueue!(agenda, cont, false)
        end
    end
    chart, agenda, logbook
end

@inline function do_fundamental_rule!(
        edge::Edge, chart, agenda, logbook, grammar, cyclic
    )
    for category in keys(conts(chart, edge))
        if is_possible_transition(grammar, state(edge), category)
            for cont in conts(chart, edge)[category]
                if !cyclic || concatenable(range(edge), range(cont))
                    trav      = Traversal(edge, cont)
                    new_state = transition(grammar, state(edge), category)
                    key       = EdgeKey(range(edge) * range(cont), new_state)
                    create_or_update!(key, trav, agenda, logbook)
                end
            end
        end
    end
    nothing
end

@inline function do_fundamental_rule!(
        cont::Constituent, chart, agenda, logbook, grammar, cyclic
    )
    for state in keys(edges(chart, cont))
        if is_possible_transition(grammar, state, category(cont))
            for edge in edges(chart, cont)[state]
                if !cyclic || concatenable(range(edge), range(cont))
                    trav      = Traversal(edge, cont)
                    new_state = transition(grammar, state, category(cont))
                    key       = EdgeKey(range(edge) * range(cont), new_state)
                    create_or_update!(key, trav, agenda, logbook)
                end
            end
        end
    end
    nothing
end

@inline function introduce_edge!(cont, agenda, logbook, grammar)
    if is_possible_transition(grammar, startstate(grammar), category(cont))
        state = transition(grammar, startstate(grammar), category(cont))
        key   = EdgeKey(range(cont), state)
        create_or_update!(key, Traversal(cont), agenda, logbook)
    end
    nothing
end

@noinline function complete_edge!{R,C,T,CR,TR,ST,S}(
        edge, agenda, logbook::ParserLogbook{R,C,T,CR,TR,ST,S}, grammar
    )
    for (category::C, rule::CR, s::S) in completions(grammar, state(edge))
        key  = ConstituentKey(range(edge), category)
        comp = EdgeCompletion(edge, rule, score(edge) * s)
        create_or_update!(key, comp, agenda, logbook)
    end
    nothing
end

@noinline function process_edge!(
        edge, chart, agenda, logbook, grammar, max_pop_num, cyclic
    )
    s = score(edge)
    edge.insidepopnumber += 1
    if s ≈ lastpopscore(edge) || insidepopnumber(edge) == max_pop_num
        if !isfinal(grammar, state(edge))
            insert!(chart, edge)
        end
        edge.score = Nullable(s) # finish the edge
        do_fundamental_rule!(edge, chart, agenda, logbook, grammar, cyclic)
    else
        complete_edge!(edge, agenda, logbook, grammar)
        edge.lastpopscore = s
        enqueue!(agenda, edge, true)
    end
    nothing
end

@noinline function process_cont!(
        cont, chart, agenda, logbook, grammar, max_pop_num, cyclic
    )
    s = score(cont)
    cont.insidepopnumber += 1
    if s ≈ lastpopscore(cont) || insidepopnumber(cont) == max_pop_num
        insert!(chart, cont)
        cont.score = Nullable(s) # finish the constituent
        do_fundamental_rule!(cont, chart, agenda, logbook, grammar, cyclic)
    else
        introduce_edge!(cont, agenda, logbook, grammar)
        cont.lastpopscore = s
        enqueue!(agenda, cont, true)
    end
    nothing
end

@noinline function loop!(chart, agenda, args...)
    while !isempty(agenda)
        if next_is_edge(agenda)
            process_edge!(dequeue_edge!(agenda), chart, agenda, args...)
        else
            process_cont!(dequeue_cont!(agenda), chart, agenda, args...)
        end
    end
end

@noinline function run_chartparser(
        terminals, grammar; epsilon=missing, max_pop_num=4, cyclic=false
    )
    C, T, CR, TR, ST, S = types(grammar)
    chart, agenda, logbook = initialize(
        T.(terminals), grammar, epsilon, cyclic)
    loop!(chart, agenda, logbook, grammar, max_pop_num, cyclic)
    ParseForest(chart, T.(terminals), grammar, cyclic)
end

############
### Test ###
############

# ascend = CFRule(1:9) do i
#     [i, i+1]
# end
# double = CFRule(1:10) do i
#     [i, i]
# end
# terminate = CFRule(1:10) do i
#     [string(i)]
# end
# grammar = CFGrammar([ascend, double], [terminate], [1])
# exp(score(grammar, 9, terminate))
#
# using BenchmarkTools
# @btime score(run_chartparser(["1" for i in 1:10], grammar))
# @btime score(run_chartparser(["1" for i in 1:60], grammar))
# @btime score(run_chartparser(["1" for i in 1:30], grammar))

# @time forest = run_chartparser(["1" for i in 1:3], grammar);
# t = sample_tree(forest)
#
# using TikzPictures
# save(PDF("test"), TikzGraphs.plot(t))

# chart, agenda, logbook = initialize(["1", "1", "2"], grammar, missing)
# next_is_edge(agenda)
# c = process_cont!(dequeue_cont!(agenda), chart, agenda, logbook, grammar, 4)
# e = dequeue_edge!(agenda)
# t = e.traversals[1]

# @time score(run_chartparser(["1" for i in 1:60], grammar, cyclic=false))
# forest = run_chartparser(["1" for i in 1:60], grammar, cyclic=false);
# sample_tree(forest)
