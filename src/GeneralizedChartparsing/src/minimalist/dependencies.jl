function head(tree :: TreeNode)
    if isterminal(tree)
        tree
    else
        nodecat = features(tree.data[1].cat)
        headfeat = catfeature(nodecat)
        #head(filter(t -> catfeature(t.data[1].cat[1]) == headfeat, tree.children)[1])
        if selects_right(features(tree.children[1].data[1].cat)[1])
            head(tree.children[1])
        else
            head(tree.children[2])
        end
    end
end

"""
This function assumes a binary MG derivation tree.
Give a list of dependents of the node at the root of tree. Dependents are represented
as a list of indices into the leaves (indices of their linear order).
"""
function dependents(tree :: TreeNode)
    nodecat = features(tree.data[1].cat)
    if length(nodecat) > 1 # has selector features
        dep = if selects_right(nodecat[1])
            head(tree.parent.children[2])
        else
            head(tree.parent.children[1])
        end
        [dependents(tree.parent); dep]
    else
        TreeNode[]
    end
end

"""
Given a tree, gives an unlabelled dependency graph over the words in the tree.
Return type is SimpleDiGraph.
"""
function dependencies{T}(tree :: TreeNode{T}; 
                         closed_under_transitivity=false,
                         directed=true)
    non_null_head(t) = !isempty(t.data[2][2])
    leaves = filter(non_null_head, leafs(tree))
    g = if directed
        DiGraph(length(leaves))
    else
        Graph(length(leaves))
    end
    
    for (i, leaf) in enumerate(leaves)
        dents = dependents(leaf)
        for (j, dent) in enumerate(leaves)
            if dent in dents
                add_edge!(g, i, j)
            end
        end
    end

    if closed_under_transitivity
        if directed
            transitiveclosure(g)
        else
            # transitiveclosure only defined for di-graphs
            Graph(transitiveclosure(DiGraph(g)))
        end
    else g end
end

"""
If a dependency matrix has dependencies (i,j) where j-i > 2, and doesn't have dependencies inside i,j that give a binary tree, add them. 

It works like this:
Let (i,j) be a cell in the matrix. If i - j > 3 then (i,j) spans more than 2 words. We check if either cells (i+1, j) or (i, j-1) are true. If not, then (i,j) is not binarized, and set both of those cells to true.
"""
function binarize_dependency_matrix(dm :: AbstractMatrix{Bool})
    n = size(dm,1)
    new_dm = copy(dm)
    # helper methods
    # is_binarized checks if i,j is binarized, uses bm for memoization
    is_binarized(i, j) = any([dm[i, j-1], dm[i+1, j]])
    # lt compares ranges (i,j) by their span length j-i
    lt(x1, x2) = x1[2]-x1[1] < x2[2]-x2[1]
    # push_sorted pushes an item into a sorted list using our lt
    push_sorted!(l, x) = insert!(l, searchsortedfirst(l, x, lt=lt), x)
    # get indices of dependencies spanning more than 2 words & sort it
    long_dependencies = [(i,j) for i in 1:n for j in i+3:n if dm[i,j]]
    sort!(long_dependencies, lt=lt)
    # binarize
    while !isempty(long_dependencies)
        i, j = pop!(long_dependencies)
        if !is_binarized(i, j)
            # add dependencies
            new_dm[i, j-1] = true
            new_dm[i+1, j] = true
            if j-i > 3
                push_sorted!(long_dependencies, (i, j-1))
                push_sorted!(long_dependencies, (i+1, j))
            end
        end
    end
    new_dm
end

"Converts a dependency graph with n vertices to an (n+1)×(n+1) dependency matrix"
function graph_to_dependency_matrix(g :: DiGraph{Int64})
    n = nv(g) + 1
    m = zeros(Matrix{Bool}(n, n))
    function dependencies_span(v)
        out_spans = map(dependencies_span, outneighbors(g, v))
        is = [s[1] for s in out_spans]
        js = [s[2] for s in out_spans]
        i = minimum([v  ; outneighbors(g, v)               ; is])
        j = maximum([v+1; [w+1 for w in outneighbors(g, v)]; js])
        i, j
    end
    for v in vertices(g)
        i, j = dependencies_span(v)
        m[i, j]   = true
        m[v, v+1] = true
    end
    m
end
