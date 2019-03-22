"Container type for elements of product (semi-)rings."
struct Product{S,T}
    first  :: S
    second :: T
end
make_accesses(Product)

Product(first, second, more...) = Product(first, Product(second, more...))
components(p::Product{S,T}) where {S, T} = (p.first, p.second)
components(p::Product{S,Product{T1,T2}}) where {S,T1,T2} = (p.first, components(p.second)...)

show(io::IO, p::Product) = print(io, "Product", components(p))

for op in (:+,:-,:*,:/)
    eval(quote
        ($op)(p1::Product{S,T}, p2::Product{S,T}) where {S,T} =
            Product(($op)(p1.first, p2.first), ($op)(p1.second, p2.second))
    end)
end

zero(::Product{S,T}) where {S,T} = Product(zero(S), zero(T))
one(::Product{S,T}) where {S,T} = Product(one(S), one(T))

<(p1::Product, p2::Product) = p1.first < p2.first && p1.second < p2.second
