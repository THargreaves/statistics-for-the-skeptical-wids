# imports
library(dplyr)
library(ggplot2)
library(rvest)
library(stringr)

# set seed
set.seed(57)

# parameters
num_blends <- 20
num_per_blend <- 10
num_participants <- num_blends * num_per_blend

# collect varieties
varieties <- read_html("https://en.wikipedia.org/wiki/Twinings") %>%
    html_nodes(xpath = '//*[@id="mw-content-text"]/div/ul/li/b') %>%
    html_text() %>%
    str_remove('–.*$')

# simulate data
sims <- tibble(
    id = factor(1:num_participants),
    blend = rep(sample(setdiff(varieties, "Turkish Apple Tea"), num_blends), 
                each = num_per_blend),
    without_tea = rbinom(num_participants, 50, 0.5),
    with_tea = rbinom(num_participants, 50, 0.5)
) %>%
    mutate(diff = with_tea - without_tea) %>%
    # force Turkish Apple Tea to have biggest difference
    group_by(blend) %>%
    mutate(group_mean = mean(diff)) %>%
    ungroup() %>%
    mutate(blend = case_when(
        group_mean == max(group_mean) ~ "Turkish Apple Tea",
        TRUE ~ blend
    )) %>%
    select(-group_mean)

# calculate mean difference
mean_diff <- mean(sims$diff)

# critical region
std_err <- sd(sims$diff) / sqrt(num_participants)
lower <- qt(0.025, num_participants - 1) * std_err
upper <- qt(0.975, num_participants - 1) * std_err

# plot without blend
p1 <- ggplot(sims, aes(x = id, y = diff)) +
    geom_col(aes(fill = id), alpha = 0.5, show.legend = FALSE) +
    geom_col(fill = NA, col = 'black') +
    annotate('rect', xmin = 0.55, xmax = num_participants + 0.45, 
             ymin = lower, ymax = upper, 
             fill = 'black', alpha = 0.25) +
    annotate('segment', x = 0.55, xend = num_participants + 0.45,
             y = mean_diff, yend = mean_diff,
             col = 'black', lty = 2, lwd = 1) +
    labs(
        x = 'Participant ID',
        y = 'Additional Games Won After Drinking Tea',
        title = paste('Difference in Performance When Playing Snakes and',
                      'Ladders Before and After Drinking Tea'),
        subtitle = paste('Average difference marked with black line and',
                         'non-critical region marked with shaped area')
    ) +
    scale_y_continuous(breaks = seq(-12, 12, by = 3)) +
    theme_minimal() +
    theme(axis.text.x = element_blank())

ggsave('all.svg', plot = p1, path = 'output', width = 40, height = 22, 
       units = "cm")

# create mean for each blend
mean_diffs <- sims %>%
    group_by(blend) %>%
    mutate(mean_diff = mean(diff)) %>%
    ungroup()

# and confidence intervals
crit_regs <- sims %>%
    group_by(blend) %>%
    summarise(std_err = sd(diff) / sqrt(num_per_blend)) %>%
    ungroup() %>%
    mutate(
        lower = qt(0.025, num_per_blend - 1) * std_err,
        upper = qt(0.975, num_per_blend - 1) * std_err
    )

# separate by blend
p2 <- ggplot(sims, aes(x = id, y = diff)) +
    geom_col(aes(fill = blend), alpha = 0.5, show.legend = FALSE) +
    geom_col(fill = NA, col = 'black') +
    geom_rect(data = crit_regs, aes(ymin = lower, ymax = upper),
              xmin = 0.55, xmax = num_per_blend + 0.45,
              fill = 'black', alpha = 0.25,
              inherit.aes = FALSE) +
    geom_segment(data = mean_diffs, aes(y = mean_diff, yend = mean_diff),
                 x = 0.55, xend = num_per_blend + 0.45,
                 col = 'black', lty = 2,
                 inherit.aes = FALSE) +
    labs(
        x = 'Participant ID',
        y = 'Additional Games Won After Drinking Tea',
        title = paste('Difference in Performance When Playing Snakes and',
                      'Ladders Before and After Drinking Tea'),
        subtitle = paste('Average difference marked with black line and',
                         'non-critical region marked with shaped area')
    ) +
    scale_y_continuous(breaks = seq(-12, 12, by = 3)) +
    facet_wrap(~blend, ncol = 5, scales = 'free_x') +
    theme_minimal() +
    theme(strip.text = element_text(face = 'bold')) +
    theme(axis.text.x = element_blank())

ggsave('blends.svg', plot = p2, path = 'output', width = 40, height = 22, 
       units = "cm")