include("lib.jl")

function simulate(d, n = 10_000, n_sims = 20_000)
    x = Array(Float64, n)
    y = Array(Float64, n)

    methods = Dict(
        "unadjusted" => unadjusted,
        "trim_shared_threshold" => trim_shared_threshold,
        "trim_separate_thresholds" => trim_separate_thresholds,
        "trim_threshold_from_A" => trim_threshold_from_A,
        "winsorize_shared_threshold" => winsorize_shared_threshold,
        "winsorize_separate_thresholds" => winsorize_separate_thresholds,
        "winsorize_threshold_from_A" => winsorize_threshold_from_A,
    )

    header = (
        "n",
        "method",
        "w",
        "te",
        "se",
        "delta",
        "sigma",
        "pvalue",
        "lower",
        "upper",
    )
    println(join(header, "\t"))

    for te in (0.00, 0.05, 0.10)
        se = sqrt(var(d) / n + var(d) / n)
        for i in 1:n_sims
            gen_data!(x, y, d, te)
            for w in 0.60:0.01:0.99
                τ_x = threshold(x, w)
                τ_y = threshold(y, w)
                τ_xy = threshold(vcat(x, y), w)
                for (method, method_func) in methods
                    x′, y′ = method_func(x, y, τ_x, τ_y, τ_xy)
                    δ, σ, p, lower, upper = analyze(x′, y′)
                    row = (n, method, 1 - w, te, se, δ, σ, p, lower, upper)
                    println(join(row, "\t"))
                end
            end
        end
    end
end

d = LogNormal(0, 1)
simulate(d)
