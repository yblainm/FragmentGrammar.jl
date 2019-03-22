using Distributions: Gamma

"type-parameterized implementation of dirichlet multinomial models"
type DirichletMultinomial{T} # T specifies the outcome value type
    n              :: Int # number of outcomes
    params         :: Dict{T, Float64} # hyperparameters
    initial_params :: Dict{T, Float64}
end
params(dm::DirichletMultinomial) = dm.params # accessor function

function DirichletMultinomial(n, params) # constructor method
    DirichletMultinomial(n, params, params)
end

"""
Return the log of the probability of the outcome d.
The outcome d is modeled by a muliset which is implemented
as a dictionary, mapping outcome values to their counts, or
any iterable object of outcome values.
"""
function logscore{T}(dm::DirichletMultinomial{T}, d::Dict{T, Int})
    s = sum(values(dm.params))
    LogProb((factorial(dm.n)*gamma(s) / gamma(dm.n+s)) *
    prod(gamma(d[k]+dm.params[k])/(factorial(d[k])*gamma(dm.params[k])) for k in keys(d)))
end

function logscore{T}(dm::DirichletMultinomial{T}, iter)
    d = Dict{T, Int}()
    for x in iter
        if haskey(d, x)
            d[x] += 1
        else
            d[x] = 1
        end
    end
    logscore(dm, d)
end

"update the parameters of dm with params"
function set_params!{T}(dm::DirichletMultinomial{T}, params)
    dm.params = dm.initial_params + params
end

function sample(dm::DirichletMultinomial)
    multinomial_weights = Dict(k => rand(Gamma(dm.params[k])) for k in keys(dm.params))
    samples = Dict(k=>0 for k in keys(dm.params))
    for i in 1:dm.n
        rand_number = rand() * sum(values(multinomial_weights))
        bound = 0.0
        for k in keys(dm.params)
            bound += multinomial_weights[k]
            if rand_number <= bound
                samples[k] += 1
                break
            end
        end
    end
    samples
end

add_obs!(dm::DirichletMultinomial, obs) = dm.params[obs] += 1

"""
Dirichlet categorical models are Dirichlet multinomial models
where the number of outcomes is fixed to one.
"""
type DirichletCategorical{T}
    dm :: DirichletMultinomial{T}
end

function DirichletCategorical{T}(params::Dict{T, Float64})
    DirichletCategorical(DirichletMultinomial(1, params))
end

function logscore{T}(dc::DirichletCategorical{T}, x::T)
    logscore(dc.dm, (x,))
end

function set_params!{T}(dc::DirichletCategorical{T}, params)
    set_params!(dc.dm, params)
end

function sample(dc::DirichletCategorical)
    d = sample(dc.dm)
    for key in keys(d)
        if d[key] == 1
            return key
        end
    end
end

add_obs!(dc::DirichletCategorical, obs) = add_obs!(dc.dm, obs)

# dirichlet categorical distribution with integer hyperparameters
type UrnModel{T}
    counts     :: Dict{T,Int}
    sum_counts :: Int
end
counts(um::UrnModel) = um.counts
sum_counts(um::UrnModel) = um.sum_counts

UrnModel{T}(dict::Dict{T,Int}) = UrnModel(dict, sum(values(dict)))
UrnModel(iter) = UrnModel(Dict{eltype(iter),Int}(x=>1 for x in iter))

logscore{T}(um::UrnModel{T}, x::T) = LogProb(counts(um)[x] / sum_counts(um))

function add_obs!(um::UrnModel, obs)
    counts(um)[obs] += 1
    um.sum_counts += 1
    obs
end

function remove_obs!(um::UrnModel, obs)
    @assert counts(um)[obs] > 1
    counts(um)[obs] -= 1
    um.sum_counts -= 1
    nothing
end

function sample(um::UrnModel)
    vals = keys(counts(um))
    weights = values(counts(um))
    rand_num = rand(1:sum_counts(um))
    bound = 0
    for (v,w) in zip(vals,weights)
        bound += w
        if rand_num <= bound
            return v
        end
    end
end

type SimpleCond{C, D, T} # C...context, D...distribution
    dists :: Dict{C, D}
    support :: Vector{T}
end

SimpleCond(dists) = SimpleCond(dists, dists |> keys |> collect)

logscore(cond::SimpleCond, x, context) = logscore(cond.dists[context], x)
sample(cond::SimpleCond, context) = sample(cond.dists[context])
function add_obs!{C,D}(cond::SimpleCond{C,D}, obs, context)
    if !haskey(cond.dists, context)
        cond.dists[context] = D(cond.support)
    end
    add_obs!(cond.dists[context], obs)
end

# urn model conditional
type UrnModelCond{T,C} # C ... context type
    support :: Vector{T}
    dists   :: Dict{C,UrnModel{T}}
end
UrnModelCond(support, C::DataType) =
    UrnModelCond(collect(support), Dict{C,UrnModel{eltype(support)}}())

function logscore(cond::UrnModelCond, x, context)
    if !haskey(cond.dists, context)
        cond.dists[context] = UrnModel(cond.support)
    end
    logscore(cond.dists[context], x)
end
function sample(cond::UrnModelCond, context)
    if !haskey(cond.dists, context)
        cond.dists[context] = UrnModel(cond.support)
    end
    sample(cond.dists[context])
end
function add_obs!(cond::UrnModelCond, obs, context)
    if !haskey(cond.dists, context)
        cond.dists[context] = UrnModel(cond.support)
    end
    add_obs!(cond.dists[context], obs)
end
function remove_obs!(cond::UrnModelCond, obs, context)
    remove_obs!(cond.dists[context], obs)
end
