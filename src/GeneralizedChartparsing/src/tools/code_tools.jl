dist(x::Number, y::Number) = x<y ? y-x : x-y

function make_accesses(T::Type)
    for (i,f) in enumerate(fieldnames(T))
        eval(:(($f)(t::$T) = getfield(t, $i)))
    end
end

function powerset(set::Vector)
    N = length(set)
    [set[[c=='1' for c in bits(n)[end-N+1:end]]] for n in 0:2^N-1]
end

function invert{K,V}(d::Dict{K,Vector{V}})
  result = Dict{V,Vector{K}}()
  for (key, values) in d
    for value in values
      if haskey(result, value)
        if !(key in result[value])
          push!(result[value], key)
        end
      else
        result[value] = [key]
      end
    end
  end
  result
end

function countdict(iter)
    d = Dict{eltype(iter),Int}()
    for x in iter
        if haskey(d, x)
            d[x] += 1
        else
            d[x] = 1
        end
    end
    d
end

import Base: +
function +{K1,K2,V1,V2}(d1::Dict{K1,V1}, d2::Dict{K2,V2})
    d = Dict{typejoin(K1,K2),typejoin(V1,V2)}(k=>v for (k,v) in d1)
    for k in keys(d2)
        if haskey(d, k)
            d[k] += d2[k]
        else
            d[k] = d2[k]
        end
    end
    d
end

########################
### TwoWayDict class ###
########################

function bijective_invert{K,V}(d::Dict{K,V})
    result =  Dict{V,K}()
    for (k, v) in d
        result[v] = k
    end
    result
end

type TwoWayDict{K,V}
    first  :: Dict{K, V}
    second :: Dict{V, K}
end

TwoWayDict{K,V}(first::Dict{K,V}) = TwoWayDict(first, bijective_invert(first))
TwoWayDict(pairs::Vararg) = TwoWayDict(Dict(pairs))
getindex{K,V}(d::TwoWayDict{K,V}, k::K) = d.first[k]
getindex{K,V}(d::TwoWayDict{K,V}, v::V) = d.second[v]

####################
### ModInt class ###
####################

struct ModInt{n} <: Number
  val::Int
  ModInt{n}(val) where {n} = new(mod(val,n))
end

show{n}(io::IO, a::ModInt{n}) = print(io, "$(a.val) mod $n")

+{n}(a::ModInt{n}, b::ModInt{n}) = ModInt{n}(a.val + b.val)
-{n}(a::ModInt{n}) = n - a.val
-{n}(a::ModInt{n}, b::ModInt{n}) = ModInt{n}(a.val - b.val)
*{n}(a::ModInt{n}, b::ModInt{n}) = ModInt{n}(a.val * b.val)
/{n}(a::ModInt{n}, b::ModInt{n}) = a * invmod(b, n)

<{n}(a::ModInt{n}, b::ModInt{n}) = a.val < b.val

one{n}(a::ModInt{n}) = ModInt{n}(1)
zero{n}(a::ModInt{n}) = ModInt{n}(0)

convert{n}(::Type{ModInt{n}}, x::Int) = ModInt{n}(x)
convert{n}(::Type{Int}, x::ModInt{n}) = x.val

getindex{n}(t::Tuple, i::ModInt{n}) = getindex(t, i.val + 1)

promote_rule{n}(::Type{ModInt{n}}, ::Type{Int}) = ModInt{n}
