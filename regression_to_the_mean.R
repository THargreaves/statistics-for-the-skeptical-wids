# imports
library(Cairo)
library(dplyr)
library(gganimate)
library(ggplot2)

# set seed
set.seed(57)

# parameters
num_steps <- 100
cutoff <- exp(qnorm(0.9, sd = 0.5))

# simulate data
sims <- tibble(
    time = as.numeric(1:num_steps),
    severity = exp(rnorm(num_steps, sd = 0.5))
) %>%
    mutate(not_severe = severity <= cutoff)

sims_with_crossing <- sims
for (i in 2:nrow(sims)) {
    x1 <- sims$time[i - 1]
    x2 <- sims$time[i]
    y1 <- sims$severity[i - 1]
    y2 <- sims$severity[i]

    if (!((y1 < cutoff & y2 > cutoff) | (y2 < cutoff & y1 > cutoff))) {
        next()
    }

    x3 <- x1 + (x2 - x1) * (cutoff - y1) / (y2 - y1)
    sims_with_crossing <- sims_with_crossing %>%
        bind_rows(tibble(time = x3, severity = cutoff, not_severe = y1 > y2))
}
sims_with_crossing <- sims_with_crossing %>%
    arrange(time)

sims_newsworthy <- sims %>%
    mutate(
        following = lead(not_severe)
    ) %>%
    filter(!not_severe)

p <- ggplot(sims, aes(x = time, y = severity, col = not_severe, group = -1)) +
    geom_line(data = sims_with_crossing) +
    geom_point(aes(group = seq_len(nrow(sims)))) +
    geom_hline(yintercept = cutoff, lty = 2) +
    geom_point(data = sims_newsworthy, 
               aes(y = 3.7, col = following, 
                   group = seq_len(nrow(sims_newsworthy))),
               size = 3) +
    annotate('text', x = num_steps, y = cutoff, size = 6, fontface = 'bold',
             label = "'Newsworthy' Threshold", vjust = -0.5, hjust = 1) +
    annotate('text', x = 3.6, y = 3.9, size = 6, fontface = 'bold',
             label = "Following Severity Class:", vjust = 1, hjust = 0) +
    labs(
        x = 'Time',
        y = 'Severity'
    ) +
    scale_y_continuous(limits = c(0, 4)) +
    theme_minimal(base_size = 16) +
    theme(legend.position = 'none') +
    transition_reveal(time, keep_last = TRUE)

anim <- animate(p, width = 1280, height = 800, end_pause = 5,
        fps = 30, nframes = 300, type = "cairo")

anim_save("output/regression_to_the_mean.gif", anim)