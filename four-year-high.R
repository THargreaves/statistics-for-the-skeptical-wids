# imports
library(Cairo)
library(dplyr)
library(gganimate)
library(ggplot2)
library(magick)
library(purrr)
library(tidyr)

# parameters
num_iter <- 100000
rate <- 40000
num_frames <- 1500

# set seed for reproducibility
set.seed(1729)

# create simulated data
sims <- tibble(
  iter = rep(1:num_iter, each = 4),
  year = factor(rep(2016:2019, times = num_iter)),
  val = rpois(4 * num_iter, lambda = rate),
  speed = 10 ^ floor(log10(iter))
)

# vectorise binomial confidence interval generation
bin.cnf <- Vectorize(function(x, n) {list(binom.test(x, n)$conf.int)})

# keep track of proportion of time each year is highest
sims %<>%
  group_by(iter) %>%
  mutate(is_high = val == max(val)) %>%
  group_by(year) %>%
  mutate(sum_max = cumsum(is_high)) %>%
  filter(iter %% speed == 0) %>%
  mutate(
    prop_max = sum_max / iter,
    cnf = bin.cnf(sum_max, iter)
  ) %>%
  mutate(cnf = map(cnf, setNames, c("lower_cnf","upper_cnf"))) %>%
  unnest_wider(cnf) %>%
  ungroup() %>%
  select(-is_high, -sum_max)

# add initial layout
sims <- tibble(
  iter = 0,
  year = factor(2016:2019),
  val = NA,
  prop_max = 0,
  lower_cnf = 0,
  upper_cnf = 1
) %>%
  bind_rows(sims)

# distribution being sampled from
distr <- tibble(val = seq(qnorm(0.001, rate, sqrt(rate)), 
                          qnorm(0.999, rate, sqrt(rate)), 
                          length.out = 500)) %>%
  mutate(density = dnorm(val, rate, sqrt(rate)))

# create value distribution animation
val_anim <- {ggplot(sims, aes(x = val)) +
    geom_line(data = distr, aes(x = val, y = density)) +
    geom_vline(aes(xintercept = val, col = year), show.legend = FALSE) +
    geom_text(x = qnorm(0.001, rate, sqrt(rate)), 
              y = dnorm(rate, rate, sqrt(rate)),
              aes(label = paste0('Iteration: ', iter, '\n', 
                                 'Speed: ', speed, 'x')),
              size = 10, hjust = 0) +
    labs(
      title = 'Randomly generated number of knife crime incidents',
      x = 'Number of Incidents',
      y = 'Likelihood of Random Sample Value',
      col = 'Year'
    ) +
    theme_classic(base_size = 16) +
    transition_states(
      states = iter,
      transition_length = 5,
      state_length = 20
    ) +
    enter_fade() +
    exit_fade() +
    ease_aes(('sine-in-out'))} %>%
  animate(width = 640, height = 800, end_pause = 5,
          fps = 30, nframes = num_frames, type = "cairo")

# create proportion highest animation
prop_anim <- {ggplot(sims, aes(x = year, y = prop_max, fill = year)) +
    geom_col(show.legend = FALSE) +
    geom_errorbar(aes(ymin = lower_cnf, ymax = upper_cnf), width = 0.5) +
    labs(
      title = 'Proportion of Time Each Year Was Highest',
      x = 'Year',
      y = 'Proportion (with 95% confidence intervals)'
    ) +
    theme_classic(base_size = 16) +
    transition_states(
      states = iter,
      transition_length = 10,
      state_length = 10
    ) +
    enter_fade() +
    exit_fade() +
    ease_aes(('sine-in-out'))} %>%
  animate(width = 640, height = 800, end_pause = 5,
          fps = 30, nframes = num_frames, type = "cairo")

# faster but requires external stitching together
anim_save("output/four_year_high_left.gif", val_anim)
anim_save("output/four_year_high_right.gif", prop_anim)

# combine gifs - warning: this takes a long time
# val_mgif <- image_read(val_anim)
# prop_mgif <- image_read(prop_anim)
# 
# combi_gif <- image_append(c(val_mgif[1], prop_mgif[1]))
# for (i in 2:num_frames) {
#   combi_frm <- image_append(c(val_mgif[i], prop_mgif[i]))
#   combi_gif <- c(combi_gif, combi_frm)
# }
# 
# # export
# image_write_gif(combi_gif, "output/four_year_high.gif")
