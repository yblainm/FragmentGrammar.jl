prob(p::LogProb) = p
prob(s::Product) = first(s)

#####################################################
### Classes of Traversal, Completion, and ItemKey ###
#####################################################

struct Traversal{S}
    edgeid  :: Int
    consid  :: Int
    score   :: S
    inloop  :: Bool
end
make_accesses(Traversal)

Traversal(edgeid, consid, score) = Traversal(edgeid, consid, score, false)
prob(trav::Traversal) = prob(score(trav))

abstract type Completion{S} end
prob(comp::Completion) = prob(score(comp))

struct EdgeCompletion{CR,S} <: Completion{S}
    edgeid :: Int
    rule   :: CR
    score  :: S
    inloop :: Bool
end
make_accesses(EdgeCompletion)

EdgeCompletion(edgeid, rule, score) = EdgeCompletion(edgeid, rule, score, false)

struct TerminalCompletion{T,TR,S} <: Completion{S}
    terminal :: T
    rule     :: TR
    score    :: S
end
make_accesses(TerminalCompletion)

# is not a bitstype
@auto_hash_equals struct EdgeKey{St}
    start :: Int
    tail  :: Int
    state :: St
end
make_accesses(EdgeKey)

# is a bitstype
struct ConsKey{C}
    start :: Int
    tail  :: Int
    cat   :: C
end
make_accesses(ConsKey)

function ==(k1::ConsKey{T}, k2::ConsKey{T}) where {T <: AbstractString}
    k1.start == k2.start && k1.tail == k2.tail && k1.cat == k2.cat
end

function Base.hash(k::ConsKey{T}) where {T <: AbstractString}
    hash(k.start, hash(k.tail, hash(k.cat)))
end

##################
### Item class ###
##################

abstract type Item end
Item(key::EdgeKey, trav, id, grammar) = Edge(key, trav, id, grammar)
Item(key::ConsKey, comp, id, grammar) = Constituent(key, comp, id, grammar)
start(item::Item) = item.start
tail(item::Item) = item.tail
length(item::Item) = item.tail - item.start
id(item::Item) = item.id
lastpopscore(item::Item) = item.lastpopscore
insidepopnumber(item::Item) = item.insidepopnumber
outsidepopnumber(item::Item) = item.outsidepopnumber

##################
### Edge class ###
##################

mutable struct Edge{St,S} <: Item
    start              :: Int
    tail               :: Int
    state              :: St
    score              :: S
    isfinished         :: Bool
    traversals         :: Vector{Traversal{S}}
    id                 :: Int # edges have positive ids
    lastpopscore       :: S
    lastoutsidepopscore:: S
    insidepopnumber    :: Int
    outsidepopnumber   :: Int
    outside_finished   :: Bool
    trav_outs_summands :: Dict{Tuple{Int, Int}, LogProb}
    comp_outs_summands :: Dict{Int, LogProb}
end
Edge(start, tail, state, traversals::Vector, id, score) =
    Edge(start, tail, state, score, false, traversals, id, score, score, 0, 0, false, Dict{Tuple{Int, Int}, LogProb}(), Dict{Int, LogProb}())
Edge(start, tail, state, traversal::Traversal{S}, id) where S =
    Edge(start, tail, state, [traversal], id, zero(S))
Edge(key::EdgeKey, traversal::Traversal, id, grammar) =
    Edge(start(key), tail(key), state(key), traversal, id)

score(edge::Edge) = if edge.isfinished
    edge.score
else
    sum(score(trav) for trav in traversals(edge))
end
prob(edge::Edge) = prob(score(edge))
outside(edge::Edge) = sum(values(edge.trav_outs_summands)) + sum(values(edge.comp_outs_summands))
state(edge::Edge) = edge.state
cat_or_state(edge::Edge) = edge.state
key(edge::Edge) = EdgeKey(edge.start, edge.tail, edge.state)
traversals(edge::Edge) = edge.traversals
show(io::IO, edge::Edge) = print(io, state(edge), "(", start(edge), ",", tail(edge), ")")

function add!(edge::Edge, traversal)
    for (i, trav) in enumerate(traversals(edge))
        if edgeid(trav) == edgeid(traversal) && consid(trav) == consid(traversal)
            traversals(edge)[i] = @set traversal.inloop = true
            return edge
        end
    end
    push!(traversals(edge), traversal)
    edge
end

#########################
### Constituent class ###
#########################

mutable struct Constituent{C,T,CR,TR,S} <: Item
    start               :: Int
    tail                :: Int
    cat                 :: C
    score               :: S
    isfinished          :: Bool
    completions         :: Vector{EdgeCompletion{CR,S}}
    terminal_completion :: Union{Nothing, Some{TerminalCompletion{T,TR,S}}}
    id                  :: Int # constituents have negative ids
    lastpopscore        :: S
    lastoutsidepopscore :: S
    insidepopnumber     :: Int
    outsidepopnumber    :: Int
    outside_finished    :: Bool
    trav_outs_summands  :: Dict{Tuple{Int, Int}, LogProb}
end
Constituent(st,ta,ca,co,te,id,sc) =
    Constituent(st,ta,ca,sc,false,co,te,id,sc,sc,0,0,false,Dict{Tuple{Int, Int}, LogProb}())
function Constituent(start, tail, cat, comp::EdgeCompletion, id, grammar)
    T, TR, S = terminal_type(grammar), terminal_rule_type(grammar), score_type(grammar)
    Constituent(start, tail, cat, [comp], Nullable{TerminalCompletion{T, TR, S}}(), id, zero(S))
end
function Constituent(start, tail, cat, comp::TerminalCompletion, id, grammar)
    CR, S = category_rule_type(grammar), score_type(grammar)
    Constituent(start, tail, cat, Vector{EdgeCompletion{CR,S}}(), Nullable(comp), id, zero(S))
end
Constituent(key::ConsKey, comp, id, grammar) =
    Constituent(start(key), tail(key), cat(key), comp, id, grammar)

score(cons::Constituent) = if cons.isfinished
    cons.score
else
    if has_terminal(cons)
        if isempty(completions(cons))
            score(get(cons.terminal_completion))
        else
            sum(score(comp) for comp in completions(cons)) +
                score(get(cons.terminal_completion))
        end
    else
        sum(score(comp) for comp in completions(cons))
    end
end

prob(cons::Constituent) = prob(score(cons))
outside(cons::Constituent) = sum(values(cons.trav_outs_summands))
cat(cons::Constituent) = cons.cat
cat_or_state(cons::Constituent) = cons.cat
key(cons::Constituent) = ConsKey(cons.start, cons.tail, cons.cat)
completions(cons::Constituent) = cons.completions
has_terminal(cons::Constituent) = !isnull(cons.terminal_completion)
terminal_completion(cons::Constituent) = get(cons.terminal_completion)
terminal(cons::Constituent) = terminal(terminal_completion(cons))

function add!(cons::Constituent, completion::EdgeCompletion)
    for (i, comp) in enumerate(completions(cons))
        if edgeid(comp) == edgeid(completion)
            completions(cons)[i] = @set completion.inloop = true
            return cons
        end
    end
    push!(completions(cons), completion)
    nothing
end

function show(io::IO, c::Constituent)
    print(io, cat(c), "(", start(c), ",", tail(c), ")")
    if has_terminal(c)
        print(io, "[", terminal(c) ,"]")
    end
end

###########################
### ParserLogbook class ###
###########################

mutable struct ParserLogbook{C,T,CR,TR,St,S}
    edges :: Vector{Edge{St,S}}
    conss :: Vector{Constituent{C,T,CR,TR,S}}
    edgeids :: Dict{EdgeKey{St}, Int}
    consids :: Dict{ConsKey{C}, Int}
end
ParserLogbook(C, T, CR, TR, St, S) =
    ParserLogbook(Vector{Edge{St,S}}(),
                  Vector{Constituent{C,T,CR,TR,S}}(),
                  Dict{EdgeKey{St}, Int}(),
                  Dict{ConsKey{C}, Int}())

function discover!(logbook, edge::Edge)
    push!(logbook.edges, edge)
    id = length(logbook.edges)
    logbook.edgeids[key(edge)] = id
    edge.id = id
    nothing
end
function discover!(logbook, cons::Constituent)
    push!(logbook.conss, cons)
    id = -length(logbook.conss)
    logbook.consids[key(cons)] = id
    cons.id = id
    nothing
end
isdiscovered(logbook, key::EdgeKey) = haskey(logbook.edgeids, key)
isdiscovered(logbook, key::ConsKey) = haskey(logbook.consids, key)
get_edge(logbook, id::Int) = logbook.edges[id]
get_cons(logbook, id::Int) = logbook.conss[-id]
get_edge(logbook, key::EdgeKey) = get_edge(logbook, logbook.edgeids[key])
get_cons(logbook, key::ConsKey) = get_cons(logbook, logbook.consids[key])
get_item(logbook, key::EdgeKey) = get_edge(logbook, logbook.edgeids[key])
get_item(logbook, key::ConsKey) = get_cons(logbook, logbook.consids[key])

###################
### Chart class ###
###################

# maybe be used in a future implementation, maybe not
"like normal merge, but value vectors with the same key will be concatenated"
function Base.merge(dicts::Dict{K,Vector{V}}...) where {K,V}
    result = Dict{K,Vector{V}}()
    for d in dicts
        for (key,valvec) in d # key and value vector
            if haskey(result, key)
                append!(result[key], valvec)
            else
                result[key] = valvec
            end
        end
    end
    result
end

mutable struct ChartCell{C,St}
    edgeids :: Dict{St, Vector{Int}}
    consids :: Dict{C, Vector{Int}}
end
ChartCell(C, St) =
    ChartCell(Dict{St, Vector{Int}}(), Dict{C, Vector{Int}}())

mutable struct Chart{C,St}
    cells :: Vector{ChartCell{C,St}}
end
edgeids(chart, edge::Edge) = chart.cells[tail(edge)].edgeids
consids(chart, edge::Edge) = chart.cells[tail(edge)].consids
edgeids(chart, cons::Constituent) = chart.cells[start(cons)].edgeids
consids(chart, cons::Constituent) = chart.cells[start(cons)].consids
# dict(chart, edge::Edge) = chart.cells[tail(edge)].edgeids
# dict(chart, cons::Constituent) = chart.cells[start(cons)].consids
function insert!(chart::Chart, edge::Edge)
    dict = edgeids(chart, edge)
    if haskey(dict, state(edge))
        push!(dict[state(edge)], id(edge))
    else
        dict[state(edge)] = [id(edge)]
    end
    nothing
end
function insert!(chart::Chart, cons::Constituent)
    dict = consids(chart, cons)
    if haskey(dict, cat(cons))
        push!(dict[cat(cons)], id(cons))
    else
        dict[cat(cons)] = [id(cons)]
    end
    nothing
end

####################
### Agenda class ###
####################

# fast viterbi parsing does not work yet

mutable struct Agenda{T}
    pqueue :: PriorityQueue{Int, T}
end

function Agenda(parsing_method)
    @assert parsing_method in (:full, :viterbi)
    if parsing_method == :full
        Agenda(PriorityQueue{Int, Int}())
    else
        Agenda(PriorityQueue{Int, Float64}())
    end
end

function enqueue!(agenda::Agenda, item, just_used)
    agenda.pqueue[id(item)] = priority(agenda, item, just_used) ::Int
    nothing
end

dequeue!(agenda::Agenda) = dequeue!(agenda.pqueue)
isempty(agenda::Agenda) = isempty(agenda.pqueue)

priority(agenda::Agenda{Int}, edge::Edge, just_completed::Bool) =
    4 * length(edge) - 2*!just_completed - 1
priority(agenda::Agenda{Int}, cons::Constituent, just_introduced_edge::Bool) =
    4 * length(cons) - 2*!just_introduced_edge

priority(agenda::Agenda{Float64}, item, just_used) =
    - prob(item).value # negative logprob, agenda favours high probs but is min orientated

##########################
### ParseForest class ###
##########################

mutable struct ParseForest{C,T,CR,TR,St,S}
    heads :: Vector{Constituent{C,T,CR,TR,S}}
    logbook :: ParserLogbook{C,T,CR,TR,St,S}
    terminals :: Vector{T}
end

function ParseForest(chart::Chart, logbook::ParserLogbook{C,T,CR,TR,St,S}, terminals, grammar) where {C,T,CR,TR,St,S}
    heads = Constituent{C,T,CR,TR,S}[]
    cell = chart.cells[1]
    for cat in startsymbols(grammar)
        if haskey(cell.consids, cat)
            for consid in cell.consids[cat]
                cons = get_cons(logbook, consid)
                if tail(cons) == length(terminals)+1
                    push!(heads, cons)
                end
            end
        end
    end
    ParseForest(heads, logbook, terminals)
end

heads(f::ParseForest) = f.heads
is_complete(f::ParseForest) = !isempty(f.heads)
score(f::ParseForest) = sum(map(score, f.heads))

### OutsideAgenda ###

mutable struct OutsideAgenda
    pqueue :: PriorityQueue{Int, Int}
end

function OutsideAgenda()
    OutsideAgenda(PriorityQueue{Int, Int}())
end

function enqueue!(agenda::OutsideAgenda, item, just_used)
    agenda.pqueue[id(item)] = priority(agenda, item, just_used)
    agenda
end

dequeue!(agenda::OutsideAgenda) = dequeue!(agenda.pqueue)
isempty(agenda::OutsideAgenda) = isempty(agenda.pqueue)

priority(agenda::OutsideAgenda, edge::Edge, just_used::Bool) =
    - 4 * length(edge) - 2*!just_used -1
priority(agenda::OutsideAgenda, cons::Constituent, just_used::Bool) =
    - 4 * length(cons) - 2*!just_used

######################

inside = prob

@inline function acc_or_init!(d::Dict, k, v)
    if haskey(d, k)
        d[k] += v
    else
        d[k] = v
    end
end

function calculate_outside_probs_and_expected_rule_usages!(f::ParseForest{C,T,CR,TR,St,S}, grammar) where {C,T,CR,TR,St,S}
    @assert is_complete(f)
    outside_agenda = OutsideAgenda()
    # mapping categories to rules to (startindex,endindex) to probabilities
    rule_usage_probs = Dict{C, Dict{typejoin(CR,TR), Dict{Tuple{Int,Int}, LogProb}}}()
    for head in heads(f)
        # traversal outside score summands
        head.trav_outs_summands[0,0] = one(LogProb)
        enqueue!(outside_agenda, head, false)
    end
    while !isempty(outside_agenda)
        id = dequeue!(outside_agenda)
        if id > 0
            parent_edge = get_edge(f.logbook, id)
            parent_edge.outsidepopnumber += 1
            # @show parent_edge.id
            # println()
            o = outside(parent_edge)
            if !(outside(parent_edge) ≈ parent_edge.lastoutsidepopscore) &&
                outsidepopnumber(parent_edge) < MAX_POP_NUMBER
                for trav in traversals(parent_edge)
                    if trav.inloop
                        cons = get_cons(f.logbook, consid(trav))
                        if edgeid(trav) == 0
                            acc_or_init!(cons.trav_outs_summands, (id,0), o)
                            # cons.trav_outs_summands[id, 0] = o
                            enqueue!(outside_agenda, cons, false)
                        end
                    end
                end
                parent_edge.lastoutsidepopscore = o
                enqueue!(outside_agenda, parent_edge, true)
            elseif !(parent_edge.outside_finished)
                parent_edge.outside_finished = true
                for trav in traversals(parent_edge)
                    if true # !(trav.inloop)
                        # println("edge")
                        cons = get_cons(f.logbook, consid(trav))
                        if edgeid(trav) == 0
                            # acc_or_init!(cons.trav_outs_summands, (id,0), o)
                            cons.trav_outs_summands[id, 0] = o
                            enqueue!(outside_agenda, cons, false)
                        else
                            edge = get_edge(f.logbook, edgeid(trav))
                            edge.trav_outs_summands[id,cons.id] = o * inside(cons)
                            # acc_or_init!(edge.trav_outs_summands, (id, cons.id), o * inside(cons))
                            enqueue!(outside_agenda, edge, false)
                            cons.trav_outs_summands[id,edge.id] = o * inside(edge)
                            # acc_or_init!(cons.trav_outs_summands, (id, edge.id), o * inside(edge))
                            enqueue!(outside_agenda, cons, false)
                        end
                    end
                end
            end
        else
            parent_cons = get_cons(f.logbook, id)
            parent_cons.outsidepopnumber += 1
            # @show parent_cons
            # println(map(c->c.edgeid, parent_cons.completions))
            # println(exp(score(parent_cons)))
            # println(parent_cons.trav_outs_summands)
            # println(outside(parent_cons) ≈ parent_cons.lastoutsidepopscore)
            # println()
            o = outside(parent_cons)
            if !(outside(parent_cons) ≈ parent_cons.lastoutsidepopscore) &&
                outsidepopnumber(parent_cons) < MAX_POP_NUMBER
                for comp in completions(parent_cons)
                    if comp.inloop
                        edge = get_edge(f.logbook, edgeid(comp))
                        p = prob(grammar, cat(parent_cons), rule(comp))
                        acc_or_init!(edge.comp_outs_summands, id, o * p)
                        # edge.comp_outs_summands[id] = o * p
                        enqueue!(outside_agenda, edge, false)
                    end
                end
                parent_cons.lastoutsidepopscore = o
                enqueue!(outside_agenda, parent_cons, true)
            else#if !(parent_cons.outside_finished)
                parent_cons.outside_finished = true
                for comp in completions(parent_cons)
                    if true # !(comp.inloop)
                        # println("cons")
                        edge = get_edge(f.logbook, edgeid(comp))
                        p = prob(grammar, cat(parent_cons), rule(comp))
                        # acc_or_init!(edge.comp_outs_summands, id, o * p)
                        edge.comp_outs_summands[id] = o * p
                        enqueue!(outside_agenda, edge, false)
                        if haskey(rule_usage_probs, cat(parent_cons))
                            if haskey(rule_usage_probs[cat(parent_cons)], rule(comp))
                                # if haskey(rule_usage_probs[cat(parent_cons)][rule(comp)], (start(parent_cons),tail(parent_cons)))
                                #     rule_usage_probs[cat(parent_cons)][rule(comp)][(start(parent_cons),tail(parent_cons))] += o * p * inside(edge)
                                # else
                                #     rule_usage_probs[cat(parent_cons)][rule(comp)][(start(parent_cons),tail(parent_cons))] = o * p * inside(edge)
                                # end
                                rule_usage_probs[cat(parent_cons)][rule(comp)][(start(parent_cons),tail(parent_cons))] = o * p * inside(edge)
                            else
                                rule_usage_probs[cat(parent_cons)][rule(comp)] = Dict((start(parent_cons),tail(parent_cons)) => o * p * inside(edge))
                            end
                        else
                            rule_usage_probs[cat(parent_cons)] = Dict(rule(comp)=>Dict((start(parent_cons),tail(parent_cons)) => o * p * inside(edge)))
                        end
                    end
                end
                if has_terminal(parent_cons)
                    comp = terminal_completion(parent_cons)
                    p = prob(grammar, cat(parent_cons), rule(comp))
                    if haskey(rule_usage_probs, cat(parent_cons))
                        if haskey(rule_usage_probs[cat(parent_cons)], rule(comp))
                            # if haskey(rule_usage_probs[cat(parent_cons)][rule(comp)], (start(parent_cons),tail(parent_cons)))
                            #     rule_usage_probs[cat(parent_cons)][rule(comp)][(start(parent_cons),tail(parent_cons))] += o * p
                            # else
                            #     rule_usage_probs[cat(parent_cons)][rule(comp)][(start(parent_cons),tail(parent_cons))] = o * p
                            # end
                            rule_usage_probs[cat(parent_cons)][rule(comp)][(start(parent_cons),tail(parent_cons))] = o * p
                        else
                            rule_usage_probs[cat(parent_cons)][rule(comp)] = Dict((start(parent_cons),tail(parent_cons)) => o * p)
                        end
                    else
                        rule_usage_probs[cat(parent_cons)] = Dict(rule(comp)=>Dict((start(parent_cons),tail(parent_cons)) => o * p))
                    end
                end
            end
        end
    end

    sequence_prop = sum(inside(head) for head in f.heads)

    expected_rule_usages = Dict(
        cat => Dict(
            rule => exp(sum(values(rule_usage_probs[cat][rule])).value - sequence_prop.value)
            for rule in keys(rule_usage_probs[cat]))
        for cat in keys(rule_usage_probs)
    )
end

function calculate_outside_probs_and_expected_rule_usages!(sent :: Vector{T}, grammar :: Grammar{C,T,CR,TR,S,Cond}) where {C,T,CR,TR,S,Cond}
    forest = run_chartparser(sent, grammar)
    d = calculate_outside_probs_and_expected_rule_usages!(forest, grammar)
    (forest, d)
end

function best_tree(f::ParseForest)
    @assert is_complete(f)
    best_tree(f, f.heads[findmax(map(prob, f.heads))[2]])
end

function best_tree(f::ParseForest{C,T,CR,TR,St,S}, cons::Constituent) where {C,T,CR,TR,St,S}
    if has_terminal(cons)
        TreeNode((cons,rule(terminal_completion(cons))),
                 Tuple{Constituent{C,T,CR,TR,S}, Union{CR,TR}})
    else
        comp = completions(cons)[findmax(map(prob, completions(cons)))[2]]
        node = TreeNode((cons, rule(comp)),
                        Tuple{Constituent{C,T,CR,TR,S}, Union{CR,TR}})
        trees = best_trees(f, get_edge(f.logbook, edgeid(comp))) # children trees
        for t in trees
            insert_child!(node, t)
        end
        node
    end
end

function best_trees(f::ParseForest, edge::Edge)
    best_traversal = traversals(edge)[findmax(map(prob, traversals(edge)))[2]]
    if edgeid(best_traversal) == 0
        (best_tree(f, get_cons(f.logbook, consid(best_traversal))),)
    else
        (best_trees(f, get_edge(f.logbook, edgeid(best_traversal)))...,
         best_tree(f,  get_cons(f.logbook, consid(best_traversal))))
    end
end

function sample_tree(f::ParseForest)
    @assert is_complete(f)
    head = categorical_sample(heads(f), map(prob, heads(f)))
    sample_tree(f, head)
end

function sample_tree(f::ParseForest{C,T,CR,TR,St,S}, cons::Constituent) where{C,T,CR,TR,St,S}
    if has_terminal(cons)
        TreeNode((cons,rule(terminal_completion(cons))),
                 Tuple{Constituent{C,T,CR,TR,S}, Union{CR,TR}})
    else
        comp = categorical_sample(completions(cons),
                                           map(prob, completions(cons)))
        node = TreeNode((cons, rule(comp)),
                        Tuple{Constituent{C,T,CR,TR,S}, Union{CR,TR}})
        trees = sample_trees(f, get_edge(f.logbook, edgeid(comp))) # children trees
        for t in trees
            insert_child!(node, t)
        end
        node
    end
end

function sample_trees(f::ParseForest, edge::Edge)
    traversal = categorical_sample(traversals(edge),
                                            map(prob, traversals(edge)))
    if edgeid(traversal) == 0 # then this edge contains the start state
        (sample_tree(f, get_cons(f.logbook, consid(traversal))),)
    else
        (sample_trees(f, get_edge(f.logbook, edgeid(traversal)))...,
         sample_tree(f,  get_cons(f.logbook, consid(traversal))))
    end
end

# # all_trees no yet implemented
# function all_trees{C,T,CR,TR,St,S}(f::ParseForest{C,T,CR,TR,St,S})
#     if is_complete(f)
#         vcat([all_trees(f, cons) for cons in heads(f)]...)
#     else
#         Vector{TreeNode{Tuple{Constituent{C,T,CR,TR,S}, Union{CR,TR}}}}
#     end
# end
#
# function all_trees{C,T,CR,TR,St,S}(
#     f    :: ParseForest{C,T,CR,TR,St,S},
#     cons :: Constituent
#     )
#     if has_terminal(cons)
#         [TreeNode((cons,rule(terminal_completion(cons))),
#                   Tuple{Constituent{C,T,CR,TR,S}, Union{CR,TR}})]
#     else
#         for comp in completions(cons)
#             node = TreeNode((cons, rule(comp)),
#                             Tuple{Constituent{C,T,CR,TR,S}, Union{CR,TR}})
#         end
#     end
# end
#
# function all_trees(f::ParseForest, edge::Edge)
#
# end

#######################
### ParseTree class ###
#######################

ParseTree{Cons<:Constituent,R} = TreeNode{Tuple{Cons,R}}

function show(io::IO, tree::ParseTree)
    print(io, "[", tree.data[1])
    for child in tree.children
        print(io, child)
    end
    print(io, "]")
end

cats_and_rules(t::ParseTree) = ((node.data[1].cat, node.data[2]) for node in t)
