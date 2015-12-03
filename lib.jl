using StatsBase
using Distributions
using HypothesisTests

function gen_data!(x, y, d, te)
    rand!(d, x)
    rand!(d, y)
    n = length(x)
    @assert n == length(y)
    for i in 1:n
        y[i] += te
    end
end

threshold(x::Array, p::Real) = quantile(x, p)

# Winsorize above only
function winsorize(x::Array, τ::Real)
    n = length(x)

    x′ = copy(x)
    for i in 1:n
        x′[i] = min(x[i], τ)
    end

    x′
end

# Trim above only
function trim(x::Array, τ::Real)
    n = length(x)

    n_remove = 0
    for i in 1:n
        if x[i] > τ
            n_remove += 1
        end
    end

    n′ = n - n_remove
    x′ = similar(x, n′)

    j = 0
    for i in 1:n
        if x[i] <= τ
            j += 1
            x′[j] = x[i]
        end
    end

    x′
end

function unadjusted(x, y, τ_x, τ_y, τ_xy)
    x′ = copy(x)
    y′ = copy(y)
    x′, y′
end

function trim_separate_thresholds(x, y, τ_x, τ_y, τ_xy)
    x′ = trim(x, τ_x)
    y′ = trim(y, τ_y)
    x′, y′
end

function trim_threshold_from_A(x, y, τ_x, τ_y, τ_xy)
    x′ = trim(x, τ_x)
    y′ = trim(y, τ_x)
    x′, y′
end

function trim_shared_threshold(x, y, τ_x, τ_y, τ_xy)
    x′ = trim(x, τ_xy)
    y′ = trim(y, τ_xy)
    x′, y′
end

function winsorize_separate_thresholds(x, y, τ_x, τ_y, τ_xy)
    x′ = winsorize(x, τ_x)
    y′ = winsorize(y, τ_y)
    x′, y′
end

function winsorize_threshold_from_A(x, y, τ_x, τ_y, τ_xy)
    x′ = winsorize(x, τ_x)
    y′ = winsorize(y, τ_x)
    x′, y′
end

function winsorize_shared_threshold(x, y, τ_x, τ_y, τ_xy)
    x′ = winsorize(x, τ_xy)
    y′ = winsorize(y, τ_xy)
    x′, y′
end

function analyze(x′, y′)
    δ = mean(y′) - mean(x′)
    σ = sqrt(var(y′) / length(y′) + var(x′) / length(x′))
    p = pvalue(UnequalVarianceTTest(y′, x′))
    lower, upper = ci(UnequalVarianceTTest(y′, x′))
    δ, σ, p, lower, upper
end
