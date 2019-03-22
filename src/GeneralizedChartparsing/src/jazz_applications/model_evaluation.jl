function model_evaluation(parsable_trees=parsable_trees; random_transp=false)
    if random_transp
        for (i,t) in enumerate(parsable_trees)
            transp = sample_chord_transposition()
            parsable_trees[i] = map(transp, t)
        end
    end

    vals = [
        single_fold_cross_validation(parsable_trees, make_jazz_grammar, true),
        single_fold_cross_validation(parsable_trees, make_jazz_grammar, false),
        single_fold_cross_validation(parsable_trees, make_plain_PCFG_jazz_grammar, true),
        single_fold_cross_validation(parsable_trees, make_plain_PCFG_jazz_grammar, false),
        single_fold_cross_validation_baseline(parsable_trees)
    ]
end

function sample_chord_transposition()
    i = rand(1:12)
    c -> begin
        m = match(r"(\d+)(.*)", c)
        string(mod(parse(m[1]) + i, 12), m[2])
    end
end
