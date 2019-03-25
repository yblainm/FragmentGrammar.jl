######################
### Parser Methods ###
######################

###################################################
## good but expensive test for create_or_update  ##
## (insert after get_edge and get_cons)          ##
## @assert id(edge) in keys(agenda) string(edge) ##
## @assert id(cons) in keys(agenda) string(cons) ##
###################################################

function create_or_update!(key::EdgeKey, trav, agenda, logbook, grammar)
    if isdiscovered(logbook, key)
        edge = get_edge(logbook, key)
        add!(edge, trav)
    else
        edge = Edge(key, trav, 0, grammar) # 0 is just a placeholder for the real id
        discover!(logbook, edge)           # that is corrected by `discover!`
    end
    enqueue!(agenda, edge, false) #::Void
    nothing
end

function create_or_update!(key::ConsKey, comp, agenda, logbook, grammar)
    if isdiscovered(logbook, key)
        cons = get_cons(logbook, key)
        add!(cons, comp)
    else
        cons = Constituent(key, comp, 0, grammar) # 0 is just a placeholder for the real id
        discover!(logbook, cons)                  # that is corrected by `discover!`
    end
    enqueue!(agenda, cons, false)
    nothing
end

function initialize(input, grammar, parsing_method, epsilon)
    n = length(input)

    C = category_type(grammar)
    T = terminal_type(grammar)
    CR = category_rule_type(grammar)
    TR = terminal_rule_type(grammar)
    St = state_type(grammar)
    S = score_type(grammar)

    chart = Chart([ChartCell(C, St) for i in 1:n+1])
    agenda = Agenda(parsing_method)
    logbook = ParserLogbook(C, T, CR, TR, St, S)
    for i in 1:n
        for (cat, rule, sco) in completions(grammar, input[i])
            create_or_update!(ConsKey(i,i+1,cat), TerminalCompletion(input[i], rule, sco), agenda, logbook, grammar)
        end
        if !ismissing(epsilon)
            for (cat, rule, sco) in completions(grammar, epsilon)
                create_or_update!(ConsKey(i,i,cat), TerminalCompletion(epsilon, rule, sco), agenda, logbook, grammar)
            end
        end
    end
    if !ismissing(epsilon)
        for (cat, rule, sco) in completions(grammar, epsilon)
            create_or_update!(ConsKey(n+1,n+1,cat), TerminalCompletion(epsilon, rule, sco), agenda, logbook, grammar)
        end
    end
    chart, agenda, logbook
end

function do_fundamental_rule!(edge::Edge, chart, agenda, logbook, grammar)
    for cat in keys(consids(chart, edge))
        if is_possible_transition(grammar, state(edge), cat)
            for consid in consids(chart, edge)[cat]
                cons = get_cons(logbook, consid)
                new_state = transition(grammar, state(edge), cat)
                new_key = EdgeKey(start(edge), tail(cons), new_state)
                traversal = Traversal(id(edge), consid, score(edge) * score(cons))
                create_or_update!(new_key, traversal, agenda, logbook, grammar)
            end
        end
    end
end

function do_fundamental_rule!(cons::Constituent, chart, agenda, logbook, grammar)
    for state in keys(edgeids(chart, cons))
        if is_possible_transition(grammar, state, cat(cons))
            for edgeid in edgeids(chart, cons)[state]
                edge = get_edge(logbook, edgeid)
                new_state = transition(grammar, state, cat(cons))
                new_key = EdgeKey(start(edge), tail(cons), new_state)
                traversal = Traversal(edgeid, id(cons), score(edge) * score(cons))
                create_or_update!(new_key, traversal, agenda, logbook, grammar)
            end
        end
    end
end

function introduce_edge!(cons::Constituent, agenda, logbook, grammar)
    if is_possible_transition(grammar, startstate(grammar), cat(cons))
        state = transition(grammar, startstate(grammar), cat(cons))
        key = EdgeKey(start(cons), tail(cons), state)
        traversal = Traversal(0, id(cons), score(cons))
        create_or_update!(key, traversal, agenda, logbook, grammar)
    end
    nothing
end

function complete_edge!(edge::Edge, agenda, logbook, grammar)
    for (cat, rule, sco) in completions(grammar, state(edge))
        key = ConsKey(start(edge), tail(edge), cat)
        comp = EdgeCompletion(id(edge), rule, score(edge) * sco)
        create_or_update!(key, comp, agenda, logbook, grammar)
    end
    nothing
end

Base.isapprox(x::LogProb, y::LogProb) = abs(x.log-y.log) < LOGSPACE_THRESHOLD

function finish!(edge::Edge, chart, agenda, logbook, grammar)
    edge.insidepopnumber += 1
    s = score(edge)
    if s ≈ lastpopscore(edge) || insidepopnumber(edge) == MAX_POP_NUMBER
        if !haskey(chart.cells[tail(edge)].edgeids, state(edge)) || !(id(edge) in chart.cells[tail(edge)].edgeids[state(edge)])
            if !isfinal(state(edge))
                insert!(chart, edge)
            end
            edge.isfinished = true
            edge.score = s
            do_fundamental_rule!(edge, chart, agenda, logbook, grammar)
        end
    else
        complete_edge!(edge, agenda, logbook, grammar)
        edge.lastpopscore = score(edge)
        enqueue!(agenda, edge, true)
    end
    nothing
end

function finish!(cons::Constituent, chart, agenda, logbook, grammar)
    cons.insidepopnumber += 1
    s = score(cons)
    if s ≈ lastpopscore(cons) || insidepopnumber(cons) == MAX_POP_NUMBER
        if !haskey(chart.cells[start(cons)].consids, cat(cons)) || !(id(cons) in chart.cells[start(cons)].consids[cat(cons)]) string(start(cons), tail(cons), cat(cons))
            insert!(chart, cons)
            cons.isfinished = true
            cons.score = s
            do_fundamental_rule!(cons, chart, agenda, logbook, grammar)
        end
    else
        introduce_edge!(cons, agenda, logbook, grammar)
        cons.lastpopscore = score(cons)
        enqueue!(agenda, cons, true)
    end
    nothing
end

function run_chartparser(treestring::AbstractString, grammar, parsing_method=:full, epsilon=missing)
    run_chartparser(tree(treestring), grammar, parsing_method, epsilon=epsilon)
end

run_chartparser(tree::Tree, grammar, parsing_method=:full; epsilon=missing) =
    run_chartparser(leaf_data(tree), grammar, boolean_dependency_matrix(tree), parsing_method, epsilon=epsilon)
    # run_chartparser(leaf_data(tree), grammar, trues(length(leaf_data(tree))+1, length(leaf_data(tree))+1))

run_chartparser(input::Vector, grammar, parsing_method=:full; epsilon=missing) =
    run_chartparser(input, grammar, trues(length(input)+1, length(input)+1), parsing_method, epsilon=epsilon)

function run_chartparser(input::Vector, grammar, dependency_matrix::AbstractMatrix{Bool}, parsing_method=:full; epsilon=missing)
    # input = map(terminal_type(grammar), input) :: Vector{terminal_type(grammar)}
    chart, agenda, logbook = initialize(input, grammar, parsing_method, epsilon)
    while !isempty(agenda)
        # finish = do inference and insert or accumulate
        id = dequeue!(agenda)
        if id > 0 # then id is an edge id
            finish!(get_edge(logbook, id), chart, agenda, logbook, grammar)#::Void
        else
            cons = get_cons(logbook, id)
            # println(dependency_matrix[start(cons), tail(cons)])
            if dependency_matrix[start(cons), tail(cons)]
                # println(cons)
                finish!(cons, chart, agenda, logbook, grammar)#::Void

                if parsing_method == :viterbi && length(cons) == length(input) && cat(cons) in startsymbols(grammar)
                    break
                end
            end
        end
    end
    ParseForest(chart, logbook, input, grammar)
end
