library("ggplot2")
library("dplyr")

methods <- read.csv("methods.csv", sep = "\t")

summaries <- methods %>%
    mutate(
        te_error = delta - te,
        se_error = sigma - se,
        ci_width = upper - lower,
        ci_covered = upper > te & lower < te
    ) %>%
    group_by(method, w, te, se) %>%
    summarize(
        te_bias = mean(te_error),
        te_rmse = sqrt(mean(te_error^2)),
        se_bias = mean(se_error),
        se_rmse = sqrt(mean(se_error^2)),
        width = mean(ci_width),
        coverage = mean(ci_covered)
    )

aa_summaries <- methods %>%
    filter(te == 0) %>%
    group_by(method, w) %>%
    summarize(fp = mean(pvalue < 0.05))

ab_summaries <- methods %>%
    filter(te > 0) %>%
    group_by(method, w, te) %>%
    summarize(fn = mean(pvalue > 0.05))

fp_fn <- inner_join(
    aa_summaries,
    ab_summaries,
    by = c("method", "w")
)

p <- ggplot(
    methods,
    aes(x = delta - te, fill = method)
) +
    geom_density() +
    facet_grid(method ~ te) +
    theme(legend.position = "none") +
    xlab("Estimated Delta - True Delta") +
    ylab("Estimated Probability Density") +
    ggtitle("Sampling Distribution of Deltas")
ggsave(file.path("graphs", "delta_kde.pdf"), height = 14, width = 12)
ggsave(file.path("graphs", "delta_kde.png"), height = 14, width = 12)

p <- ggplot(
    methods,
    aes(x = sigma - se, fill = method)
) +
    geom_density() +
    facet_grid(method ~ te) +
    theme(legend.position = "none") +
    xlab("Estimated SE - True SE") +
    ylab("Estimated Probability Density") +
    ggtitle("Sampling Distribution of SE's")
ggsave(file.path("graphs", "se_kde.pdf"), height = 14, width = 12)
ggsave(file.path("graphs", "se_kde.png"), height = 14, width = 12)

p <- ggplot(summaries, aes(x = w, y = te_bias, color = method)) +
    geom_line() +
    facet_grid(. ~ te) +
    geom_hline(yintercept = 0, alpha = 0.25) +
    xlab("Outlier Window") +
    ylab("Bias in Estimated Delta") +
    ggtitle("Outlier Methods Can Induce Bias in Deltas")
ggsave(file.path("graphs", "delta_bias.pdf"), height = 7, width = 12)
ggsave(file.path("graphs", "delta_bias.png"), height = 7, width = 12)

p <- ggplot(summaries, aes(x = w, y = te_rmse, color = method)) +
    geom_line() +
    facet_grid(. ~ te) +
    scale_y_log10() +
    xlab("Outlier Window") +
    ylab("RMSE of Estimated Deltas") +
    ggtitle("Outlier Methods Can Improve Inference for Deltas")
ggsave(file.path("graphs", "delta_rmse.pdf"), height = 7, width = 12)
ggsave(file.path("graphs", "delta_rmse.png"), height = 7, width = 12)

p <- ggplot(summaries, aes(x = w, y = se_bias, color = method)) +
    geom_line() +
    facet_grid(. ~ te) +
    geom_hline(yintercept = 0, alpha = 0.25) +
    xlab("Outlier Window") +
    ylab("Bias in Estimated SE") +
    ggtitle("Outlier Methods Always Induces Bias in SE's")
ggsave(file.path("graphs", "se_bias.pdf"), height = 7, width = 12)
ggsave(file.path("graphs", "se_bias.png"), height = 7, width = 12)

p <- ggplot(summaries, aes(x = w, y = se_rmse, color = method)) +
    geom_line() +
    facet_grid(. ~ te) +
    scale_y_log10() +
    xlab("Outlier Window") +
    ylab("RMSE of Estimated SE") +
    ggtitle("Outlier Methods Never Improves Inference for SE's")
ggsave(file.path("graphs", "se_rmse.pdf"), height = 7, width = 12)
ggsave(file.path("graphs", "se_rmse.png"), height = 7, width = 12)

p <- ggplot(summaries, aes(x = te_rmse, y = se_rmse, color = method)) +
    geom_point() +
    facet_grid(. ~ te) +
    scale_x_log10() +
    scale_y_log10() +
    xlab("RMSE of Estimated Delta") +
    ylab("RMSE of Estimated SE") +
    ggtitle("Delta vs SE Inferental Trade-Offs")
ggsave(file.path("graphs", "rmse_tradeoffs.pdf"), height = 7, width = 12)
ggsave(file.path("graphs", "rmse_tradeoffs.png"), height = 7, width = 12)

p <- ggplot(aa_summaries, aes(x = w, y = fp, color = method)) +
    geom_line() +
    geom_hline(yintercept = 0.05, alpha = 0.25) +
    xlab("Outlier Window") +
    ylab("False Positive Rate") +
    ggtitle("False Positive Rates for p < 0.05")
ggsave(file.path("graphs", "fp_rates.pdf"), height = 7, width = 12)
ggsave(file.path("graphs", "fp_rates.png"), height = 7, width = 12)

p <- ggplot(
    subset(ab_summaries, te < 0.1),
    aes(x = w, y = fn, color = method)
) +
    geom_line() +
    facet_grid(. ~ te) +
    xlab("Outlier Window") +
    ylab("False Negative Rate") +
    ggtitle("False Negative Rates for p < 0.05 When True Delta = 0.05")
ggsave(file.path("graphs", "fn_rates.pdf"), height = 7, width = 12)
ggsave(file.path("graphs", "fn_rates.png"), height = 7, width = 12)

p <- ggplot(
    subset(fp_fn, te < 0.1),
    aes(x = fp, y = fn, color = method)
) +
    geom_point() +
    geom_vline(xintercept = 0.05, alpha = 0.25) +
    facet_grid(. ~ te) +
    xlab("False Positive Rate") +
    ylab("False Negative Rate") +
    ggtitle("False Positives vs False Negatives When True Delta = 0.05")
ggsave(file.path("graphs", "fp_fn.pdf"), height = 7, width = 12)
ggsave(file.path("graphs", "fp_fn.png"), height = 7, width = 12)

p <- ggplot(summaries, aes(x = w, y = coverage, color = method)) +
    geom_line() +
    facet_grid(. ~ te) +
    geom_hline(yintercept = 0.95, alpha = 0.25) +
    xlab("Outlier Window") +
    ylab("Coverage Probability") +
    ggtitle("Coverage Probability for Nominal 95% CI's")
ggsave(file.path("graphs", "coverage.pdf"), height = 7, width = 12)
ggsave(file.path("graphs", "coverage.png"), height = 7, width = 12)

p <- ggplot(summaries, aes(x = w, y = width, color = method)) +
    geom_line() +
    facet_grid(. ~ te) +
    xlab("Outlier Window") +
    ylab("CI Width") +
    ggtitle("Width of Nominal 95% CI's")
ggsave(file.path("graphs", "ci_width.pdf"), height = 7, width = 12)
ggsave(file.path("graphs", "ci_width.png"), height = 7, width = 12)

p <- ggplot(summaries, aes(x = width, y = coverage, color = method)) +
    geom_point() +
    facet_grid(. ~ te) +
    xlab("CI Width") +
    ylab("Coverage Probability") +
    ggtitle("Coverage vs Width of Nominal 95% CI's")
ggsave(file.path("graphs", "ci_width_vs_coverage.pdf"), height = 7, width = 12)
ggsave(file.path("graphs", "ci_width_vs_coverage.png"), height = 7, width = 12)
