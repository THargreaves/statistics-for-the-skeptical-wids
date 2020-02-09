# imports
library(dplyr)
library(ggplot2)

# reproducibility
set.seed(57)

# simulate data
sims <- tibble(
  class = factor(rep(c("Child", "Adult"), each = 20))
) %>% 
  group_by(class) %>%
  mutate(
    weight = case_when(
      class == 'Child' ~ 100,
      TRUE ~ 155
    ) * exp(rnorm(n(), sd = 0.08)),
    running_speed = case_when(
      class == 'Child' ~ 18,
      TRUE ~ 25
    ) / exp(abs(log(mean(weight) / weight))) ^ ifelse(weight < mean(weight), 0.1, 1)
    * exp(rnorm(n(), sd = 0.1))
  )

# base plot
p_base <- ggplot(sims, aes(x = weight, y = running_speed)) +
  theme_minimal() +
  labs(
    x = 'Weight (lbs)',
    y = 'Running Speed (km/h)',
    col = 'Class'
  ) +
  theme(legend.position = "bottom")

# regression line only
p_regr <- p_base +
  geom_smooth(col = 'red', method = 'lm')

ggsave('regr.svg', plot = p_regr, path = 'output', width = 20, height = 11, 
       units = "cm")

# with data points
p_points <- p_regr +
  geom_point()

ggsave('points.svg', plot = p_points, path = 'output', width = 20, height = 11, 
       units = "cm")

# by class
p_class <- p_base +
  geom_smooth(method = 'lm', col = 'black') +
  geom_point(aes(col = class))

ggsave('class.svg', plot = p_class, path = 'output', width = 20, height = 11, 
       units = "cm")

# correct
p_correct <- p_base +
  geom_smooth(method = 'lm', aes(col = class), show.legend = FALSE) +
  geom_point(aes(col = class))

ggsave('correct.svg', plot = p_correct, path = 'output', width = 20, height = 11, 
       units = "cm")