"Container type for elements of product (semi-)rings."
immutable Product{S,T}
    first  :: S
    second :: T
end
make_accesses(Product)

Product(first, second, more...) = Product(first, Product(second, more...))
components{S,T}(p::Product{S,T}) = (p.first, p.second)
components{S,T1,T2}(p::Product{S,Product{T1,T2}}) = (p.first, components(p.second)...)

show(io::IO, p::Product) = print(io, "Product", components(p))

for op in (:+,:-,:*,:/)
    eval(quote
        ($op){S,T}(p1::Product{S,T}, p2::Product{S,T}) =
            Product(($op)(p1.first, p2.first), ($op)(p1.second, p2.second))
    end)
end

zero{S,T}(::Product{S,T}) = Product(zero(S), zero(T))
one{S,T}(::Product{S,T}) = Product(one(S), one(T))

<(p1::Product, p2::Product) = p1.first < p2.first && p1.second < p2.second
