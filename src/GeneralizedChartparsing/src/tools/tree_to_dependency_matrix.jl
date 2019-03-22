boolean_dependency_matrix(treestring::AbstractString) =
    boolean_dependency_matrix(tree(treestring))

boolean_dependency_matrix(t::Tree) =
    map(x-> x == "" ? false : true, dependency_matrix(t, x->x))

function dependency_matrix(treestring::AbstractString, translate_notation)
    dependency_matrix(tree(treestring), translate_notation)
end

function dependency_matrix(t::Tree, translate_notation)
    t = map(x->"x", t)
    k = translate_notation_and_add_leaf_indices!(t, translate_notation)
    add_inner_dependency_indices!(t)

    M = ["" for i in 1:k, j in 1:k]
    for node in t
        m = match(r"(.+) (\d+) (\d+)", node.data)
        M[parse(m[2]), parse(m[3])] = m[1]
    end
    M
end

function translate_notation_and_add_leaf_indices!(tree::Tree, translate_notation)
    k = 1
    for node in tree
        node.data = translate_notation(node.data)
        if isterminal(node)
            node.data *= " $k $(k+1)"
            k += 1
        end
    end
    k # highest indices
end

function add_inner_dependency_indices!(treenode::Tree)
    if !isterminal(treenode)
        m1 = match(r" (\d+) (\d+)", add_inner_dependency_indices!(treenode.children[1]).data)
        m2 = match(r" (\d+) (\d+)", add_inner_dependency_indices!(treenode.children[end]).data)
        treenode.data *= " $(m1[1]) $(m2[2])"
    end
    treenode
end

# dependency_matrix("[C[C[G[D-][G]][C]][C[G[D-][G]][C]]]", translate_goldstandard_notation2)
# boolean_dependency_matrix("[C[C[G[D-][G]][C]][C[G[D-][G]][C]]]")
