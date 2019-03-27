startstate(grammar) ::State #
startsymbols(grammar) ::Vector{Category} #
completions(grammar, state) ::Vector{Tuple{Category, CategoryRule, LogProb}}
completions(grammar, terminal) ::Vector{Tuple{Category, TerminalRule, LogProb}}
prob(grammar, category, rule) ::LogProb

isfinal(state) ::Bool
is_possible_transition(grammar, state, cat) ::Bool # No need
transition(grammar, state, cat) ::State # Idem

category_type(grammar) ::Type
terminal_type(grammar) ::Type
category_rule_type(grammar) ::Type
terminal_rule_type(grammar) ::Type
score_type(grammar) ::Type
state_type(grammar) ::Type
