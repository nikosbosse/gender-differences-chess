# this script analyses average perfomance differences. In particular, it 
# generates: 
# - a plot of average perfomance differences vs. female participation

library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(data.table)

# read in and combine data
pvals_mean_rating <- fread("results/results_all.csv") %>%
  dplyr::mutate(p_val = 1 - perc_smaller_all) %>%
  dplyr::select(country, p_val)

pvals_sd_rating <- fread("results/results_all_sd.csv") %>%
  dplyr::mutate(p_val = 1 - perc_smaller) %>%
  dplyr::select(country, p_val)

pvals_median_rating <- fread("results/results_all_median.csv") %>%
  dplyr::mutate(p_val = 1 - perc_smaller) %>%
  dplyr::select(country, p_val)


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

world_data <- combined %>%
  dplyr::mutate(inactive = ifelse(is.na(inactive), FALSE, TRUE)) %>%
  dplyr::filter(birthday < 2000, 
                !inactive) %>%
  select(fideid, country, sex, rating, birthday)
 
data <- world_data %>%
  dplyr::filter(country %in% regions)

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
  ggplot(aes(x = diff, y = n_rel_F)) + 
  geom_point() + 
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") + 
  labs(x = "Difference in average performance men - women", 
       y = "Percentage of female players in a country") + 
  geom_smooth(method = "glm", 
              method.args=list(family="binomial"), 
              alpha = 0.1, colour = 'black', size = 0.4) + 
  theme_minimal()

ggsave("results/plots/female-participation-performance.png", 
       width = 10, height = 4)


# histogram of the distribution of ratings -------------------------------------

hist_F <- world_data %>%
  dplyr::filter(sex == "F") %>%
  dplyr::mutate(sex = "Female") %>%
  ggplot(aes(x = rating)) +
  geom_histogram(aes(y = stat(count) / sum(count)), 
                 colour = "white") + 
  facet_wrap(~ sex) + 
  theme_minimal() +
  scale_y_continuous(limits = c(0, 0.085)) + 
  scale_x_continuous(limits = c(1000, 2900)) + 
  labs(y = "Proportion", x = "Rating")
  
  
hist_M <- world_data %>%
  dplyr::filter(sex == "M") %>%
  dplyr::mutate(sex = "Male") %>%
  ggplot(aes(x = rating)) +
  geom_histogram(aes(y = stat(count) / sum(count)), 
                 colour = "white") + 
  facet_wrap(~ sex) + 
  theme_minimal() + 
  scale_y_continuous(limits = c(0, 0.085)) +
  scale_x_continuous(limits = c(1000, 2863)) +
  labs(y = "Proportion", x = "Rating")

hist_F + hist_M

ggsave("results/plots/histograms_ratings_world.png", 
       width = 10, height = 4)











scatter_plots <- function(data, top_x = Inf) {
  
  # prepare_data <- function(data, summary_function) {
  #   
  # }
  
  # scatter plot for the mean of women and mean in every country
  # ------------------------------------------------------------------------------
  plot_data <- data %>%
    dplyr::group_by(country, sex) %>%
    dplyr::arrange(country, sex, rating) %>%
    slice(1:min(top_x, nrow(data))) %>%
    dplyr::summarise(rating = mean(rating)) %>%
    tidyr::pivot_wider(names_from = sex, 
                       values_from = rating) %>%
    dplyr::mutate(difference = M - F) %>%
    dplyr::inner_join(pvals_mean_rating) %>%
    dplyr::mutate(`P-value` = ifelse(p_val < 0.05 | p_val > 0.95, 
                                     "Significant", 
                                     "Not significant"))
  
  p1 <- plot_data %>%
    ggplot(aes(y = F, x = M)) +
    geom_point(aes(colour = `P-value`)) +
    geom_abline(aes(slope = 1, intercept = 0), 
                linetype = "dashed", color = "grey40") +
    coord_cartesian(xlim = c(1200, 2200), ylim = c(1200, 2200)) + 
    labs(y = "Mean rating females", x = "Mean rating males") + 
    theme_minimal() + 
    theme(legend.position = "none") + 
    scale_color_manual(values = c("black", "tomato3")) 
  
  # additional histogram
  inset1 <- plot_data %>%
    ggplot(aes(x = difference)) +
    geom_histogram(aes(y = stat(count) / sum(count)), 
                   colour = "white", fill = "grey50") + 
    geom_vline(xintercept = 0, linetype = "dashed") +
    theme_bw() + 
    labs(x = "Diff. rating mean", y = "Proportion")  
  
  mean_plot <- p1 + inset_element(inset1, left = 0.02, bottom = 0.65, right = 0.5, top = 1, ignore_tag = TRUE)
  
  # ggsave("results/plots/scatter_mean_male_female.png", width = 4, height = 4)
  

  
  # scatter plot for the median of women and men in every country
  # ------------------------------------------------------------------------------
  plot_data <- data %>%
    dplyr::group_by(country, sex) %>%
    dplyr::arrange(country, sex, rating) %>%
    slice(1:min(top_x, nrow(data))) %>%
    dplyr::summarise(rating = median(rating)) %>%
    tidyr::pivot_wider(names_from = sex, 
                       values_from = rating) %>%
    dplyr::inner_join(pvals_median_rating) %>%
    dplyr::mutate(difference = M - F) %>%
    dplyr::mutate(`P-value` = ifelse(p_val < 0.05 | p_val > 0.95, 
                                     "Significant", 
                                     "Not significant"))
  
  p2 <- plot_data %>%
    ggplot(aes(y = F, x = M)) +
    geom_point(aes(colour = `P-value`)) +
    geom_abline(aes(slope = 1, intercept = 0), 
                linetype = "dashed", color = "grey40") +
    coord_cartesian(xlim = c(1200, 2200), ylim = c(1200, 2200)) + 
    labs(y = "Median rating females", x = "Median rating males") + 
    theme_minimal() + 
    theme(legend.position = "none") + 
    scale_color_manual(values = c("black", "tomato3"))
  
  # additional histogram
  inset2 <- plot_data %>%
    ggplot(aes(x = difference)) +
    geom_histogram(aes(y = stat(count) / sum(count)), 
                   colour = "white", fill = "grey50") + 
    geom_vline(xintercept = 0, linetype = "dashed") +
    theme_bw() + 
    labs(x = "Diff. rating median", y = "Proportion")  
  
  median_plot2 <- p2 + inset_element(inset2, left = 0.02, bottom = 0.65, right = 0.5, top = 1, ignore_tag = TRUE)
  
  # ggsave("results/plots/scatter_median_male_female.png", 
  #        width = 4, height = 4)
  
  
  # scatter plot for the sd of women and mean in every country
  # ------------------------------------------------------------------------------
  plot_data <- data %>%
    dplyr::group_by(country, sex) %>%
    dplyr::arrange(country, sex, rating) %>%
    slice(1:min(top_x, nrow(data))) %>%
    dplyr::summarise(rating = sd(rating)) %>%
    tidyr::pivot_wider(names_from = sex, 
                       values_from = rating) %>%
    dplyr::mutate(difference = M - F) %>%
    dplyr::inner_join(pvals_sd_rating) %>%
    dplyr::mutate(`P-value` = ifelse(p_val < 0.05 | p_val > 0.95, 
                                     "Significant", 
                                     "Not significant"))
  
  p3 <- plot_data %>%
    ggplot(aes(y = F, x = M)) +
    geom_point(aes(colour = `P-value`)) +
    geom_abline(aes(slope = 1, intercept = 0), 
                linetype = "dashed", color = "grey40") +
    coord_cartesian(xlim = c(160, 450), ylim = c(160, 450)) +
    labs(y = "SD rating females", x = "SD rating males") + 
    theme_minimal() + 
    scale_color_manual(values = c("black", "tomato3")) 
  
  # additional histogram
  inset3 <- plot_data %>%
    ggplot(aes(x = difference)) +
    geom_histogram(aes(y = stat(count) / sum(count)), 
                   colour = "white", fill = "grey50") + 
    geom_vline(xintercept = 0, linetype = "dashed") +
    theme_bw() + 
    labs(x = "Diff. rating SD", y = "Proportion")  
  
  sd_plot <- p3 + inset_element(inset3, left = 0.02, bottom = 0.65, right = 0.5, top = 1, ignore_tag = TRUE)
  
  # ggsave("results/plots/scatter_sd_male_female.png", 
  #        width = 4, height = 4)
  
  # together ---------------------------------------------------------------------
  (p1 + p2 + p3) /
    (inset1 + inset2 + inset3) +
    plot_layout(guides = "collect", 
                heights = c(2, 1)) &
    theme(legend.position = 'bottom') 
  
  ggsave(paste0("results/plots/all_scatter_plots-", top_x, ".png"),
         width = 12, height = 7)
  
  
  # combined mean and sd plot ----------------------------------------------------
  # p4 <- mean_plot + median_plot + sd_plot + 
  #   plot_layout(guides = "collect") &
  #   theme(legend.position = 'bottom') 
  
  
  # ggsave("results/plots/scatter_combined_male_female.png", 
  #        width = 12, height = 4)  
  
}

scatter_plots(data)
scatter_plots(data, 10)






# mean age plot for the countries ----------------------------------------------
plot_data <- data %>%
  dplyr::group_by(country, sex) %>%
  dplyr::summarise(age = mean(2021 - birthday, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = sex, 
                     values_from = age) %>%
  dplyr::mutate(difference = M - F) 

mean_age <- plot_data %>%
  ggplot(aes(y = F, x = M)) +
  geom_point() +
  geom_abline(aes(slope = 1, intercept = 0), 
              linetype = "dashed", color = "grey40") +
  coord_cartesian(xlim = c(20, 60), ylim = c(20, 60)) + 
  theme_minimal() + 
  labs(y = "Country average age female", x = "Country average age male")


# relationship between age and rating ------------------------------------------

data %>%
  dplyr::mutate(age = 2021 - birthday) %>%
  ggplot(aes(y = rating, x = age)) + 
  theme_minimal()

# for top 10: age vs. ratings
agetop10 <- data %>%
  dplyr::group_by(country, sex) %>%
  dplyr::arrange(-rating) %>%
  dplyr::slice(10) %>%
  dplyr::mutate(age = 2021 - birthday) %>%
  ggplot(aes(y = rating, x = age)) + 
  geom_point(aes(color = sex)) + 
  theme_minimal() + 
  labs(y = "Rating top 10", x = "Average age top 10")

ggsave("results/plots/scatter_age_rating.png")


# combine age plots
mean_age + agetop10 + 
  plot_annotation(tag_levels = 'A')

ggsave("results/plots/scatter_combined_age.png", 
       width = 12, height = 4)

# plot with age difference vs. rating difference for top 10. not so sure
rating_vs_age_top10 <- data %>%
  dplyr::group_by(country, sex) %>%
  dplyr::arrange(-rating) %>%
  dplyr::slice(10) %>%
  dplyr::mutate(age = 2021 - birthday) %>%
  dplyr::summarise(age = mean(age), 
                   rating = mean(rating)) %>%
  tidyr::pivot_wider(names_from = sex, values_from = c(age, rating)) %>%
  dplyr::mutate(age_diff = age_M - age_F, 
                rating_diff = rating_M - rating_F, 
                `Avg. man older` = age_diff > 0) %>% 
  ggplot(aes(y = rating_diff, x = age_diff)) + 
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") + 
  geom_point(aes(color = `Avg. man older`)) + 
  geom_smooth(method = "lm", se = TRUE, 
              size = 0.4,
              colour = 'black', alpha = 0.1) + 
  theme_minimal() + 
  labs(y = "Rating difference", x = "Age difference")

ggsave("results/plots/scatter_diff_age_rating.png", 
       width = 10, height = 4)




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
