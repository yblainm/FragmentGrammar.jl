module Trees
using ForceImport
@force using Base

export Tree, EmptyTree, TreeNode,
       isterminal, insert_child!,
       tree, lisp_tree_structure, parenthesis_to_brackets,
       start, next, done, eltype,
       leafs, leaf_data, data,
       iterate, eltype, IteratorSize

abstract type Tree{T} end

mutable struct EmptyTree{T} <: Tree{T}
end

mutable struct TreeNode{T} <: Tree{T}
  data    ::T
  parent  ::Tree{T}
  children::Vector{TreeNode{T}}
end

isterminal(tree::TreeNode{T}) where T = isempty(tree.children)

TreeNode(data::T, parent::Tree{T}) where T = TreeNode(data, parent, Vector{TreeNode{T}}())

# TreeNode{T}(data::T, parent::Tree{T}) = TreeNode(data, parent, Vector{TreeNode{T}}())
TreeNode(data, T=typeof(data)) = TreeNode(data, EmptyTree{T}())

data(t::TreeNode) = t.data

show(io::IO, tree::EmptyTree{T}) where T = print(io::IO, "[]")

function show(io::IO, tree::TreeNode{T}) where T
  print(io, "[", tree.data)
  for child in tree.children
    print(io, child)
  end
  print(io, "]")
end

function insert_child!(tree::TreeNode{T}, data::T) where T
  push!(tree.children, TreeNode(data, tree))
end

function insert_child!(tree::TreeNode, child::TreeNode)
    child.parent = tree
    push!(tree.children, child)
end

function tree(str::AbstractString)
  node = TreeNode("")
  str = replace(str, " "=>"")[2:end-1]

  for c in str
    if c == '['
      insert_child!(node, "")
      node = node.children[end]
    elseif c == ']'
      node = node.parent
    else
      node.data = string(node.data, c)
    end
  end
  node
end

function tree(lst::Union{Tuple, AbstractVector}, T=typeof(lst[1]))
    node = TreeNode(lst[1], T)
    if length(lst) > 1
        for x in lst[2:end]
            insert_child!(node, tree(x, T))
        end
    end
    node
end

function lisp_tree_structure(str::String)
  # str = replace(str, "  ", " ")
  node = TreeNode("")
  str = replace(str, "  ", " ")[2:end-1]
  prev = '['
  start_reading_terminal = false
  last_is_terminal = false

  for c in str
    if c == '['
      insert_child!(node, "")
      node = node.children[end]
      start_reading_terminal = false
      last_is_terminal = false
    elseif c == ']'
      if last_is_terminal
        node = node.parent.parent
      else
        node = node.parent
      end
      start_reading_terminal = false
      last_is_terminal = false
    elseif c == ' '
      start_reading_terminal = prev != '[' && prev != ']'
    else
      if start_reading_terminal
        if last_is_terminal
          node = node.parent
        end
        insert_child!(node, "")
        node = node.children[end]
        start_reading_terminal = false
        last_is_terminal = true
      end
      node.data = string(node.data, c)
    end
    prev = c
  end

  if prev == ']'
    node
  else
    node.parent
  end
end

parenthesis_to_brackets(str::String) =
  replace(replace(str, "(", "["), ")", "]")

start(tree::TreeNode{T}) where T = [tree]
next(::TreeNode{T}, list::Vector{TreeNode{T}}) where T =
  list[1], prepend!(list[2:end], list[1].children)
done(::TreeNode{T}, list::Vector{TreeNode{T}}) where T = isempty(list)
eltype(::Type{TreeNode{T}}) where T = TreeNode{T}

length(tree::TreeNode) =
    isterminal(tree) ? 1 : 1 + sum(length(c) for c in tree.children)

leafs(tree) = [n for n in tree if isterminal(n)]
leaf_data(tree) = map(x->x.data, leafs(tree))

function Base.map(f, t::Tree)
    n = TreeNode(f(t.data))
    for c in t.children
        insert_child!(n, map(f, c))
    end
    n
end

eltype(::Type{Tree}) = Pointer
IteratorSize(::Type{Tree}) = Base.SizeUnknown()
function iterate(tree::Tree, state = [tree])
    if isempty(state)
        nothing
    else
        state[1], prepend!(state[2:end], collect(values(state[1].children)))
    end
end

# t = tree("[1[2[3][4][6]][5]]")
# t2 = map(x->2parse(x), t)


# print(tree(str))
#
# str = "[S [NP phpSyntaxTree][VP [V creates][NP nice syntax trees]]]"
# print(lisp_tree_structure(str))

# str = "((SBAR (IN Because) (S (NP (NN realEDASHestate) (NNS purchases) (CC and) (NNS leases)) (VP (VBP are) (NP (NP (JJ such) (JJ major) (JJ longEDASHterm) (NNS commitments)) (SBAR (IN that) (S (NP (JJS most) (NNS companies) (CC and) (NNS individuals)) (VP (VBP make) (NP (DT these) (NNS decisions)) (SBAR (WHADVP (RB only) (WRB when)) (S (ADJP (JJ confident) (PP (IN of) (NP (JJ future) (NX (NX (JJ economic) (NN stability)) (CC and) (NX (NN growth)))))))))))))) (EDOT EDOT)))"
# println(parenthesis_to_brackets(str))
# println()
# println(lisp_tree_structure(parenthesis_to_brackets(str)))

end # module
