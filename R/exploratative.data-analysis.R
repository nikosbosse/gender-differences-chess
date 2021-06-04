# this script analyses average perfomance differences. In particular, it 
# generates: 
# - a plot of average perfomance differences vs. female participation

library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(patchwork)

# read in and combine data
chess <- readr::read_csv("data/20201006_FIDE_ratings.csv")
inactive <- readr::read_csv("data/inactive_players_jan20.txt", col_names = "fideid") %>%
  dplyr::mutate(inactive = TRUE)

combined <- dplyr::full_join(chess, inactive)

regions <- c("FRA", "GER", "RUS", "ESP", "POL", "IND", "IRI", "GRE", "CZE", 
             "TUR", "HUN", "BRA", "SRI", "SRB", "NED", "ITA", "COL", "UKR",
             "AUT", "SVK", "CHN", "CRO", "ROU", "MEX", "BEL", "SWE", "ENG", 
             "KAZ", "CUB", "GEO", "NOR", "USA", "SUI", "VIE", "LTU", "ARG", 
             "ISR", "DEN", "EGY", "PHI", "BAN", "LAT", "SLO", "CHI", "KEN", 
             "BUL")

data <- combined %>%
  dplyr::mutate(inactive = ifelse(is.na(inactive), FALSE, TRUE)) %>%
  dplyr::filter(birthday < 2000, 
                !inactive, 
                country %in% regions) %>%
  select(fideid, country, sex, rating, birthday)
 


# calculate mean performance by country 
performance <- data %>%
  group_by(country) %>%
  mutate(n_country = n()) %>%
  group_by(country, sex) %>%
  summarise(mean = mean(rating), 
            sd = sd(rating), 
            median = median(rating),
            n_country = unique(n_country),
            n = n(),
            n_rel = n() / unique(n_country)) %>%
  ungroup() 

# turn into wide format to calculate per country difference
performance_wide <- performance %>%
  tidyr::pivot_wider(id_cols = c(country,n_country), 
                     names_from = sex, 
                     values_from = c(mean, sd, n_rel, n)) %>%
  mutate(diff = mean_M - mean_F)


# scatter plot of average performance difference vs. female participation 
performance_wide %>%
  ggplot(aes(y = diff, x = n_rel_F)) + 
  geom_point() + 
  geom_hline(yintercept = 0) + 
  labs(y = "Difference in average performance men - women", 
       x = "Percentage of female players in a country") + 
  geom_smooth()

ggsave("results/plots/female-participation-performance.png")


# variances of men and women
# Interpretation: variance seems to be higher for women than for men
performance %>%
  ggplot(aes(y = sd, x = sex, fill = sex, color = sex)) + 
  geom_violin(alpha = 0.2) + 
  geom_boxplot(alpha = 0.3, color = "black") + 
  geom_jitter()

ggsave("results/plots/sd-men-vs-women.png")

# variances of men and women, filtering for countries with low numbers of females
# Interpretation: it seems that variance difference is even higher if we 
# filter out small sample sizes
p_sd <- performance %>%
  filter(n > 130) %>%
  ggplot(aes(y = sd, x = sex, fill = sex, color = sex)) + 
  geom_violin(alpha = 0.2) + 
  geom_boxplot(alpha = 0.3, color = "black") + 
  theme(legend.position = "bottom") + 
  geom_jitter()

# check standard deviation vs. number of people
performance %>%
  ggplot(aes(y = sd, x = n, fill = sex, color = sex)) + 
  geom_point() + 
  facet_wrap(~ sex, scales = "free_x") + 
  geom_smooth()

ggsave("results/plots/sd-vs-number-of-players.png")

# check mean in different countries for men and women
p_mean <- performance %>%
  ggplot(aes(y = mean, x = sex, fill = sex, color = sex)) + 
  geom_violin(alpha = 0.2) + 
  geom_boxplot(alpha = 0.3, color = "black") + 
  theme(legend.position = "bottom") + 
  geom_jitter()

ggsave("results/plots/mean-men-vs-women.png")

# check mediann in different countries for men and women
p_median <- performance %>%
  ggplot(aes(y = median, x = sex, fill = sex, color = sex)) + 
  geom_violin(alpha = 0.2) + 
  geom_boxplot(alpha = 0.3, color = "black") + 
  theme(legend.position = "bottom") + 
  geom_jitter()

ggsave("results/plots/median-men-vs-women.png")


# combined plot for mean, median, sd
p_sd | p_mean | p_median +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom") 

ggsave("results/plots/boxplot-differences-sd-mean-median.png")
