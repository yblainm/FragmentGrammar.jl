###########################################
### Building the meta-rule Jazz Grammar ###
###########################################

function make_meta_rule{T<:Tuple}(name::String, catslist::Vector{T}, f=x->x)
    rules = [ContextFreeRule(cats[1], map(f, cats[2:end])) for cats in catslist]
    eval( :( $(Symbol(name)) = MetaRule($name, $rules) ) )
end

const all_category_rule_symbols = [
    :prolongation;
    :diatonic_prep;
    :true_modulation;
    :parallel_modulation;
    :full_mode_change;
    :partial_mode_change;
    :plagal_prep;
    :octa_dom_subs3;
    :octa_dom_subs6;
    :octa_dom_subs9;
    :diatonic_subs;
    :min_M6_subs
]

# back-off order is from left to right
dependent_components(cat::Category) = (scaledeg(cat),)

function make_jazz_grammar(Score :: DataType = LogProb, category_rules_symbols=all_category_rule_symbols)
    K_M = [Key(r,"major") for r in 1:12]
    K_m = [Key(r,"minor") for r in 1:12]
    K = [K_M; K_m]
    S = [ScaleDeg(deg) for deg in 1:7]
    B = [true, false]
    C = [Category(s,k,b) for s in S for k in K for b in B]

    make_meta_rule("prolongation",
        [(c,c,c) for c in C], notdomsubst)
    make_meta_rule("diatonic_prep",
        [(c,transpose(c, 4), c) for c in C], notdomsubst)
    make_meta_rule("true_modulation",
        [(c, Category("V", modulate(c), false), c) for c in C if numeral(scaledeg(c)) != "I"], notdomsubst)
    make_meta_rule("parallel_modulation",
        [(c, Category("V", change_mode(modulate(c)), false), c) for c in C], notdomsubst)
    make_meta_rule("full_mode_change",
        [(Category("I", k, false), Category("V", change_mode(k), false), Category("I", change_mode(k), false)) for k in K])
    make_meta_rule("partial_mode_change",
        [(Category("I", k, false), Category("V", k, false), Category("I", change_mode(k), false)) for k in K])
    make_meta_rule("plagal_prep",
        [(Category("I", k, false), Category("IV", k, false), Category("I", k, false)) for k in K])
    make_meta_rule("octa_dom_subs3",
        [(Category("V", k, false), Category("V", k+3, true)) for k in K])
    make_meta_rule("octa_dom_subs6",
        [(Category("V", k, false), Category("V", k+6, true)) for k in K])
    make_meta_rule("octa_dom_subs9",
        [(Category("V", k, false), Category("V", k+9, true)) for k in K])
    make_meta_rule("diatonic_subs",
        [[(Category("I", k, false), Category("VI", k, false)) for k in K_M];
         [(Category("I", k, false), Category("III", k, false)) for k in K_m];
         [(Category("II", k, false), Category("IV", k, false)) for k in K];
         [(Category("V", k, b), Category("VII", k, false)) for k in K_M for b in B]])
    make_meta_rule("min_M6_subs",
        [(Category("V", k, b), Category("II", k, true)) for k in K_M for b in B])

    category_rules = map(eval, category_rules_symbols)

    make_meta_rule("triadic_termination", [(c, Terminal(c)) for c in C])
    make_meta_rule("alt_dominant", [(Category("V",k,b), Terminal(Category("V",k,b), "alt")) for k in K for b in B])
    make_meta_rule("aug_dominant", [(Category("V",k,b), Terminal(Category("V",k,b), "aug")) for k in K for b in B])
    make_meta_rule("sus_dominant", [(Category("V",k,b), Terminal(Category("V",k,b), "sus4")) for k in K for b in B])
    make_meta_rule("min_dominant", [(Category("V",k,b), Terminal(Category("V",k,b), "maj")) for k in K_m for b in B])
    # make_meta_rule("min_M6_dom", ...)

    terminal_rules = [triadic_termination,
                      alt_dominant,
                      aug_dominant,
                      sus_dominant,
                      min_dominant]

    startsymbols = [Category("I",k,false) for k in K]

    Grammar(category_rules, terminal_rules, startsymbols, Score, dependent_components)
end

####################################################
### Building the plain context-free Jazz Grammar ###
####################################################

function make_PCFG_rule_list{T<:Tuple}(name::String, catslist::Vector{T}, f=x->x)
    rules = [ContextFreeRule(cats[1], map(f, cats[2:end])) for cats in catslist]
    eval( :( $(Symbol(name)) = $rules) )
end

function make_plain_PCFG_jazz_grammar(Score :: DataType = LogProb, category_rules_symbols=all_category_rule_symbols)
    K_M = [Key(r,"major") for r in 1:12]
    K_m = [Key(r,"minor") for r in 1:12]
    K = [K_M; K_m]
    S = [ScaleDeg(deg) for deg in 1:7]
    B = [true, false]
    C = [Category(s,k,b) for s in S for k in K for b in B]

    make_PCFG_rule_list("prolongation",
        [(c,c,c) for c in C], notdomsubst)
    make_PCFG_rule_list("diatonic_prep",
        [(c,transpose(c, 4), c) for c in C], notdomsubst)
    make_PCFG_rule_list("true_modulation",
        [(c, Category("V", modulate(c), false), c) for c in C if numeral(scaledeg(c)) != "I"], notdomsubst)
    make_PCFG_rule_list("parallel_modulation",
        [(c, Category("V", change_mode(modulate(c)), false), c) for c in C], notdomsubst)
    make_PCFG_rule_list("full_mode_change",
        [(Category("I", k, false), Category("V", change_mode(k), false), Category("I", change_mode(k), false)) for k in K])
    make_PCFG_rule_list("partial_mode_change",
        [(Category("I", k, false), Category("V", k, false), Category("I", change_mode(k), false)) for k in K])
    make_PCFG_rule_list("plagal_prep",
        [(Category("I", k, false), Category("IV", k, false), Category("I", k, false)) for k in K])
    make_PCFG_rule_list("octa_dom_subs3",
        [(Category("V", k, false), Category("V", k+3, true)) for k in K])
    make_PCFG_rule_list("octa_dom_subs6",
        [(Category("V", k, false), Category("V", k+6, true)) for k in K])
    make_PCFG_rule_list("octa_dom_subs9",
        [(Category("V", k, false), Category("V", k+9, true)) for k in K])
    make_PCFG_rule_list("diatonic_subs",
        [[(Category("I", k, false), Category("VI", k, false)) for k in K_M];
         [(Category("I", k, false), Category("III", k, false)) for k in K_m];
         [(Category("II", k, false), Category("IV", k, false)) for k in K];
         [(Category("V", k, b), Category("VII", k, false)) for k in K_M for b in B]])
    make_PCFG_rule_list("min_M6_subs",
        [(Category("V", k, b), Category("II", k, true)) for k in K_M for b in B])

    category_rules = vcat(map(eval, category_rules_symbols)...)

    make_PCFG_rule_list("triadic_termination", [(c, Terminal(c)) for c in C])
    make_PCFG_rule_list("alt_dominant", [(Category("V",k,b), Terminal(Category("V",k,b), "alt")) for k in K for b in B])
    make_PCFG_rule_list("aug_dominant", [(Category("V",k,b), Terminal(Category("V",k,b), "aug")) for k in K for b in B])
    make_PCFG_rule_list("sus_dominant", [(Category("V",k,b), Terminal(Category("V",k,b), "sus4")) for k in K for b in B])
    make_PCFG_rule_list("min_dominant", [(Category("V",k,b), Terminal(Category("V",k,b), "maj")) for k in K_m for b in B])
    # make_meta_rule("min_M6_dom", ...)

    terminal_rules = [triadic_termination;
                      alt_dominant;
                      aug_dominant;
                      sus_dominant;
                      min_dominant]

    startsymbols = [Category("I",k,false) for k in K]

    Grammar(category_rules, terminal_rules, startsymbols, Score, dependent_components)
end
