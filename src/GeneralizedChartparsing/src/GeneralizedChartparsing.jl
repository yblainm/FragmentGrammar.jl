module GeneralizedChartparsing

const LOGSPACE_THRESHOLD = 0.0001

# MAX_POP_NUMBER is length of unary chain plus two for
# (1) creating the item and (2) inserting it into the chart

# const MAX_POP_NUMBER = 4
const MAX_POP_NUMBER = 4

####################
### Dependencies ###
####################
using ForceImport

@force using Base # must be concretized
# importall Base.Collections

@force using DataStructures

using Missings
using LogProbs
# using Base.Collections
using AutoHashEquals
# using DataFrames
# using Gadfly
using LightGraphs
using Setfield

##################################
### Exported methods and types ###
##################################


# semi-rings
export LogProb,
       Product

# parser methods
export run_chartparser,
       score,
       prob,
       inside,
       outside,
       best_tree,
       sample_tree,
       is_complete,
       heads,
       get_edge,
       get_cons,
       calculate_outside_probs_and_expected_rule_usages!

# evaluation methods
export single_fold_cross_validation,
       single_fold_cross_validation_baseline,
       parsable_trees,
       model_evaluation

# WSJ methods
# export read_treebank_grammar,
#        compute_treebank_scores,
#        treebank_goldstandard_scores

# grammar mathods
export Grammar,
       State,
       train!,
       dependent_components,
       set_params!,
       one_update_step_hyperparameters!,
       learn_hyperparameters!,
       MinimalistGrammar,
       LexicalItem

# jazz grammar methods
# export make_jazz_grammar,
#        make_plain_PCFG_jazz_grammar,
#        make_terminals,
#        pitch_class,
#        Terminal,
#        make_parsable_trees

# Tree Class types and methods
export TreeNode,
       leaf_data,
       dependency_matrix,
       boolean_dependency_matrix,
       leafs

export category_type,
       terminal_type,
       category_rule_type,
       terminal_rule_type,
       score_type,
       state_type,
       startstate,
       startsymbols,
       completions,
       score,
       prob,
       is_possible_transition,
       transition,
       isfinal

# vb_update
# export expected_lexitem_counts,
#        expected_lexitem_counts_by_category,
#        expected_lexitem_counts_by_sentence,
#        makeLexicon_andCorpus,
#        makeLexicon_test,
#        variational_inference,
#        variational_inference_full_corpus_parallel,
#        variational_inference_full_corpus,
#        load_lexicon,
#        sample_counts


# # UD corpus
# export read_corpus,
#        read_sentences,
#        get_sentences,
#        CoNLLUCorpus,
#        CoNLLUSentence,
#        CoNLLUWord,
#        evaluate_dependencies,
#        make_lexicon_from_corpus_all_combinations,
#        unkify!,
#        parse_given_dependencies,
#        kullback_leibler,
#        uptorank,
#        compare_compactness

# lexicon.jl
# export make_feature_list,
#        remove_duplicates!,
#        remove_vacuous_nullheads!

# evaluation
# export precision,
#        recall,
#        parse_and_evaluate_corpus,
#        parse_and_show_results,
#        vblearn_parse_and_show_results

# dependencies
export head,
       dependents,
       dependencies,
       graph_to_dependency_matrix,
       binarize_dependency_matrix,
       haspath,
       has_crossing_dependencies,
       has_noncontiguous_dependencies,
       tikzdependency

# Forward sampler for MG
# export SampleGrammar,
#        SampleDerivation,
#        find_selectors,
#        treeRootC,
#        probDerivation,
#        samplerMGderivation,
#        getsent_fromderiv,
#        sample_corpus,
#        corpus_without_duplicates,
#        write_probs,
#        write_sentence_probs,
#        write_sentence_probs_ranked


##################
### Load files ###
##################

include(joinpath("tools", "code_tools.jl"))

include(joinpath("semi_rings", "ProductSemiRings.jl"))

include(joinpath("tools", "Trees.jl"))
using .Trees
include(joinpath("tools", "tree_to_dependency_matrix.jl"))
include(joinpath("tools", "tree_evaluation.jl"))

# include(joinpath("grammars", "UrnModels.jl"))
include(joinpath("grammars", "CompoundDistributions.jl"))
#using GeneralizedChartparsing.CompoundDistributions
@force using .CompoundDistributions # Grammars.jl uses logscore
include(joinpath("grammars", "Grammars.jl"))
# include(joinpath("grammars", "parameter_learning.jl"))

# include(joinpath("minimalist", "MinimalistGrammar.jl"))
# include(joinpath("minimalist", "counts.jl"))
# include(joinpath("minimalist", "lexicon.jl"))
# include(joinpath("minimalist", "dependencies.jl"))
# include(joinpath("minimalist", "forwardsampler_MG.jl"))
using Nullables
include(joinpath("parser", "parser_types.jl"))
include(joinpath("parser", "parser_methods.jl"))
#
# include(joinpath("jazz_applications", "music_data_types.jl"))
# include(joinpath("jazz_applications", "JazzGrammars.jl"))
# include(joinpath("jazz_applications", "single_fold_cross_validation.jl"))
# include(joinpath("jazz_applications", "boxplot.jl"))
# include(joinpath("jazz_applications", "model_evaluation.jl"))
#
# include(joinpath("WSJ", "WSJ_methods.jl"))
#
# include(joinpath("tools", "corpus.jl"))
# include(joinpath("minimalist", "interface_parser_vb_update.jl"))

end # module
