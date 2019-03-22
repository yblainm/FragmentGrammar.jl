function one_update_step_hyperparameters!(grammar, sequences)
    forests = map(x->run_chartparser(x, grammar), sequences)
    expected_rule_usages_for_each_sequence =
        map(x->calculate_outside_probs_and_expected_rule_usages!(x, grammar), forests)

    cats = union([keys(d) for d in expected_rule_usages_for_each_sequence]...)
    for cat in cats
        d = sum(d[cat] for d in expected_rule_usages_for_each_sequence if haskey(d, cat))
        set_params!(grammar.rule_cond.dists[dependent_components(cat)], d)
    end
    old_likelihood = prod(score(forest) for forest in forests)
end

function learn_hyperparameters!(grammar, sequences)
    old_likelihood = zero(LogProb)
    old_old_likelihood = zero(LogProb)
    i = 1
    while true
        println("iteration step $i")
        old_likelihood = one_update_step_hyperparameters!(grammar, sequences)
        println("old likelihood $old_likelihood")
        if abs(old_likelihood.value - old_old_likelihood.value) < 0.001
            break
        end
        i += 1
    end
end
