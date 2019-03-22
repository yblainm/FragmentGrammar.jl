precision(proposal, goldstandard) =
    num_common_cons(proposal, goldstandard) / num_cons(proposal)
recall(proposal, goldstandard) =
    num_common_cons(proposal, goldstandard) / num_cons(goldstandard)
f1_measure(proposal, goldstandard) =
    2 * precision(proposal, goldstandard) * recall(proposal, goldstandard) /
       (precision(proposal, goldstandard) + recall(proposal, goldstandard))

"number of constituents of tree"
function num_cons(tree::Tree)
    M = boolean_dependency_matrix(tree)
    count(x->x, M)
end

"number of common constituents"
function num_common_cons(tree1::Tree, tree2::Tree)
    A = boolean_dependency_matrix(tree1)
    B = boolean_dependency_matrix(tree2)
    sum(a && b for (a,b) in zip(A,B))
end

"number of crossed bracket pairs"
function num_crossed_brackets(tree1, tree2)
    num_cons(tree1) - num_common_cons(tree1, tree2)
end

# t1 = tree("[0maj[0maj[0maj][1maj]][0maj[7maj][0maj]]]")
# t2 = tree("[0maj[1maj[2min][1maj]][0maj[7maj][0maj]]]")
# t3 = tree("[0maj[0maj][0maj[0maj][0maj[7maj][0maj]]]]")
#
# num_crossed_brackets(t1, t3)
