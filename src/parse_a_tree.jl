# g = grammar_from_string(
#     """
#     S --> NP VP
#     NP --> D N | dog
#     VP --> V NP | V PP
#     PP --> P N
#     D --> the | prep
#     N --> dog
#     V --> paints
#     P --> prep
#     """
# );
g = grammar_from_string( # N --> T1 and so on don't work ; T1 treated as a terminal!
    """
    N --> hand | size | ADJ T6 | V T9
    ADJ --> great | N T7 | V T8
    V --> drench | ADJ T10
    ADV --> very | ADJ T11
    T1 --> hand
    T2 --> size
    T3 --> great
    T4 --> drench
    T5 --> very
    T6 --> +ness
    T7 --> +y
    T8 --> +ed
    T9 --> +er
    T10 --> +ify
    T11 --> +ly
    """
);
g.start_categories = g.categories[1:4]

test_str = "hand +y"

function parseString(grammar, str) #get all parses for string
    scores = parse(grammar, split(str))
    g2 = add_score(grammar, :enum_forest, enum_forest_score)
    score = parse(g2, split(str))["S"]
    trees = tree_structs(g, score.enum_forest)
    return trees
end

# test_str = "the dog paints prep dog"

function sampleTree(grammar, str) #sample a parse from possble parses of string
    g3 = add_forest_score(add_random_prob_score(grammar, :count), :prob)
    forest = parse(g3, split(str))["S"].forest;
    rand(forest) |>            # get random tree iterator
    collect |>                 # collect rule applications of a random tree
    t -> tree_struct(g3, t)
end

#sampleTree(g, "the dog paints prep dog")

#function forwardSample(grammar, dist)

# v = [5, 10, 15]
# r = ChineseRest(v)
#println(r)
#println(g)
