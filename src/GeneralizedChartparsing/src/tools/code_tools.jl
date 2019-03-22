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

function invert(d::Dict{K,Vector{V}}) where {K,V}
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
function +(d1::Dict{K1,V1}, d2::Dict{K2,V2}) where {K1, V1, K2, V2}
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

function bijective_invert(d::Dict{K,V}) where {K,V}
    result =  Dict{V,K}()
    for (k, v) in d
        result[v] = k
    end
    result
end

struct TwoWayDict{K,V}
    first  :: Dict{K, V}
    second :: Dict{V, K}
end

TwoWayDict(first::Dict{K,V}) where {K,V} = TwoWayDict(first, bijective_invert(first))
TwoWayDict(pairs::Vararg) = TwoWayDict(Dict(pairs))
getindex(d::TwoWayDict{K,V}, k::K) where {K,V} = d.first[k]
getindex(d::TwoWayDict{K,V}, v::V) where {K,V} = d.second[v]

####################
### ModInt class ###
####################

mutable struct ModInt{n} <: Number
  val::Int
  ModInt{n}(val) where {n} = new(mod(val,n))
end

show(io::IO, a::ModInt{n}) where n = print(io, "$(a.val) mod $n")

+(a::ModInt{n}, b::ModInt{n}) where n = ModInt{n}(a.val + b.val)
-(a::ModInt{n}) where n = n - a.val
-(a::ModInt{n}, b::ModInt{n}) where n = ModInt{n}(a.val - b.val)
*(a::ModInt{n}, b::ModInt{n}) where n = ModInt{n}(a.val * b.val)
/(a::ModInt{n}, b::ModInt{n}) where n = a * invmod(b, n)

<(a::ModInt{n}, b::ModInt{n}) where n = a.val < b.val

one(a::ModInt{n}) where n = ModInt{n}(1)
zero(a::ModInt{n}) where n = ModInt{n}(0)

convert(::Type{ModInt{n}}, x::Int) where n = ModInt{n}(x)
convert(::Type{Int}, x::ModInt{n}) where n = x.val

getindex(t::Tuple, i::ModInt{n}) where n = getindex(t, i.val + 1)

promote_rule(::Type{ModInt{n}}, ::Type{Int}) where n = ModInt{n}
