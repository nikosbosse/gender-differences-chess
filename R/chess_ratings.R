# analysis script for my original blog article, but no longer in use

library(magrittr)
library(data.table)
library(ggplot2)
chess <- readr::read_csv("chess_standard_dataframe.csv")

n_highest = 10
n_draws = 100000

# analysis for india
chess_filtered_india <- chess %>%
  dplyr::filter(birthday <= 2000, 
                country == "IND")

# plot male and female
plot <- ggplot()

plot <- plot + 
  geom_density(data = chess_filtered_india %>%
                 dplyr::filter(sex == "F"), 
               aes(x = rating, y = ..count../sum(..count..), color = sex), alpha = 1) +   
  geom_density(data = chess_filtered_india %>%
                 dplyr::filter(sex == "M"), 
               aes(x = rating, y = ..count../sum(..count..), color = sex), alpha = 1) + 
  labs(y = "probability", x = "Elo Rating")
plot


# basic summary statistics india
summary_india <- chess_filtered_india %>%
  dplyr::group_by(sex) %>%
  dplyr::summarise(number = max(dplyr::n()), 
                   mean = mean(rating), 
                   median = median(rating), 
                   sd = sd(rating)) %>%
  dplyr::ungroup()
summary_india

# actual differences in top performers
summary_india_top <- chess_filtered_india %>%
  dplyr::group_by(sex) %>%
  dplyr::arrange(sex, rating) %>%
  dplyr::slice_tail(n = n_highest) %>%
  dplyr::summarise(number = max(dplyr::n()), 
                   mean = mean(rating), 
                   median = median(rating), 
                   sd = sd(rating)) %>%
  dplyr::ungroup()

summary_india

n_small_india <- summary_india %>%
  dplyr::filter(sex == "F") %>%
  dplyr::pull(number)

n_large_india <- summary_india %>%
  dplyr::filter(sex == "M") %>%
  dplyr::pull(number)

difference_top_india <- summary_india_top %>%
  dplyr::pull(mean) %>%
  diff()
  

# permutation analysis with only the highest
permute <- function(df, n_small = 20012, n_large = 243375, n_highest = 1) {
  n_total <- nrow(df)
  
  # resort data.frame
  df <- df[sample(n_total), ]
  
  values_small <- df$rating[1:n_small]
  values_large <- df$rating[(n_small + 1):n_total]
  
  if (n_highest == 1) {
    mean_small <- max(values_small)
    mean_large <- max(values_large)
  } else {
    mean_small <- mean(tail(sort(values_small), n_highest))
    mean_large <- mean(tail(sort(values_large), n_highest))
  }
  return(data.frame(mean_small, mean_large))
}


# analysis India
l_india <- list()
df <- data.table::as.data.table(chess_filtered_india)[, .(rating)] #convert for speed
for (i in 1:n_draws) {
  l_india[[i]] <- permute(df, 
                          n_small = n_small_india, 
                          n_large = n_large_india, 
                          n_highest = n_highest)
}


# permutation analysis India
results_india <- dplyr::bind_rows(l_india) %>%
  dplyr::mutate(difference = mean_large - mean_small)

data.table::fwrite(results_india, paste0("permutation_india_top", n_highest, ".csv"))

results_india %>%
  dplyr::pull(difference) %>%
  mean()

results_india %>%
  ggplot() + 
  geom_density(aes(x = difference, y = ..count..)) + 
  geom_vline(xintercept = difference_top_india, color = "red")

results_india %>%
  dplyr::filter(difference <= difference_top_india) %>%
  nrow() / nrow(results_india)



  






# analysis for entire world ----------------------------------------------------
chess_filtered <- chess %>%
  dplyr::filter(birthday <= 2000)

# plot male and female
plot <- ggplot()

plot <- plot + 
  geom_density(data = chess_filtered %>%
                 dplyr::filter(sex == "F"), 
               aes(x = rating, y = ..count../sum(..count..), color = sex), alpha = 1) +   
  geom_density(data = chess_filtered %>%
                 dplyr::filter(sex == "M"), 
               aes(x = rating, y = ..count../sum(..count..), color = sex), alpha = 1) + 
  labs(y = "probability", x = "Elo Rating")
plot




# basic summary statistics world
summary_world <- chess_filtered %>%
  dplyr::group_by(sex) %>%
  dplyr::summarise(number = max(dplyr::n()), 
                   mean = mean(rating), 
                   median = median(rating), 
                   sd = sd(rating)) %>%
  dplyr::ungroup()
summary_world

# actual differences in top performers
summary_world_top <- chess_filtered %>%
  dplyr::group_by(sex) %>%
  dplyr::arrange(sex, rating) %>%
  dplyr::slice_tail(n = n_highest) %>%
  dplyr::summarise(number = max(dplyr::n()), 
                   mean = mean(rating), 
                   median = median(rating), 
                   sd = sd(rating)) %>%
  dplyr::ungroup()
summary_world_top

n_small_world <- summary_world %>%
  dplyr::filter(sex == "F") %>%
  dplyr::pull(number)

n_large_world <- summary_world %>%
  dplyr::filter(sex == "M") %>%
  dplyr::pull(number)

difference_top_world <- summary_world_top %>%
  dplyr::pull(mean) %>%
  diff()

difference_top_world

# analysis world
l_world <- list()
df <- data.table::as.data.table(chess_filtered)[, .(rating)] #convert for speed
for (i in 1:n_draws) {
  l_world[[i]] <- permute(df, 
                          n_small = n_small_world, 
                          n_large = n_large_world, 
                          n_highest = n_highest)
}


# permutation analysis world
results_world <- dplyr::bind_rows(l_world) %>%
  dplyr::mutate(difference = mean_large - mean_small)

data.table::fwrite(results_world, paste0("permutation_world_top", n_highest, ".csv"))

results_world %>%
  dplyr::pull(difference) %>%
  mean()

results_world %>%
  ggplot() + 
  geom_density(aes(x = difference, y = ..count..)) + 
  geom_vline(xintercept = difference_top_world, color = "red")

results_world %>%
  dplyr::filter(difference <= difference_top_world) %>%
  nrow() / nrow(results_world)

