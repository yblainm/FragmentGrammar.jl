module parse_a_tree

include("CompoundDists.jl")
using GeneralizedChartParsing
using .CompoundDists

g = grammar_from_string(
        """
        S --> NP VP
        NP --> D N | dog
        VP --> V NP | V PP
        PP --> P N
        D --> the | prep
        N --> dog
        V --> paints
        P --> prep
        """
    );

function parseString(grammar, str) #get all parses for string
        scores = parse(grammar, split(str))
        g2 = add_score(grammar, :enum_forest, enum_forest_score)
        score = parse(g2, split(str))["S"]
        trees = tree_structs(g, score.enum_forest)
        return trees
    end

#parseString(g, "the dog paints prep dog")

function sampleTree(grammar, str) #sample a parse from possble parses of string
        g3 = add_forest_score(add_random_prob_score(grammar, :count), :prob)
        forest = parse(g3, split(str))["S"].forest;
        rand(forest) |>            # get random tree iterator
        collect |>                 # collect rule applications of a random tree
        t -> tree_struct(g3, t)
    end

#sampleTree(g, "the dog paints prep dog")

#function forwardSample(grammar, dist)

v = [5, 10, 15]
r = ChineseRest(v)
#println(r)
#println(g)
end
