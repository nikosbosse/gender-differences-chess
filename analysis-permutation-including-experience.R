library(magrittr)
library(data.table)
library(ggplot2)
library(foreach)
library(doParallel)
registerDoParallel(cores=4)

# changed line 135635 in the original data file to Abrosimov, Nikolay
experience <- data.table::fread("data/experiencedatafromchessbase.csv", 
                                colClasses = c("numeric", "character", 
                                               "character", "character", 
                                               "character", "numeric", 
                                               "character", "numeric"))

names(experience) <- c("fideid", "last_name", "first_name", 
                       "first_rating_list", "last_rating_list", 
                       "number_rating_lists", "sex", "number_games")

fide <- data.table::fread("data/20201006_FIDE_ratings.csv")
inactive <- readr::read_csv("data/inactive_players_jan20.txt", col_names = "fideid") %>%
  dplyr::mutate(inactive = TRUE)

fide_active <- dplyr::full_join(fide, inactive) %>%
  dplyr::mutate(inactive = ifelse(is.na(inactive), FALSE, TRUE)) %>%
  dplyr::filter(birthday < 2000, 
                inactive == FALSE)


data <- experience %>%
  dplyr::select(fideid, number_games) %>%
  dplyr::inner_join(fide_active) %>%
  dplyr::mutate(age =  2021 - as.numeric(birthday))


permute <- function(data_female, data_male, n_highest = 10) {
  n_small <- length(data_female)
  n_large <- length(data_male)
  n_total <- n_small + n_large
  
  all_data <- c(data_female, data_male)
  
  # shuffle scores
  all_data <- all_data[sample(n_total)]
  
  # draw small and large sample
  values_small <- all_data[1:n_small]
  values_large <- all_data[(n_small + 1):n_total]
  
  
  if (n_highest == 1) {
    mean_small <- max(values_small)
    mean_large <- max(values_large)
  } else if (n_highest == Inf) {
    mean_small <- mean(values_small)
    mean_large <- mean(values_large)
  } else {
    mean_small <- mean(tail(sort(values_small), n_highest))
    mean_large <- mean(tail(sort(values_large), n_highest))
  }
  return(mean_large - mean_small)
}


apply_permuatation <- function(region, data, metric = "rating") {
  data_female <- data %>%
    dplyr::filter(country == region, 
                  sex == "F") %>%
    dplyr::pull(eval(metric))
  
  data_male <- data %>%
    dplyr::filter(country == region, 
                  sex == "M") %>%
    dplyr::pull(eval(metric))
  
  observed_difference <- data %>%
    dplyr::filter(country == region) %>%
    dplyr::group_by(sex) %>%
    dplyr::arrange(rating) %>%
    dplyr::slice_tail(n = n_highest) %>%
    dplyr::summarise(mean = mean(get(metric)), 
                     .groups = "drop_last") 
  
  male <- observed_difference$mean[observed_difference$sex == "M"]
  female <- observed_difference$mean[observed_difference$sex == "F"]
  diff <- male - female
  
  permuted_differences <- replicate(n = n_draws, expr = {
    permute(data_female, data_male, n_highest = n_highest)
  })
  
  res <- data.table(country = region, 
                    mean_male = male, 
                    mean_female = female,
                    observed_difference = diff, 
                    mean_perm_difference = mean(permuted_differences), 
                    "% perm>=obs" = mean(permuted_differences >= diff),
                    n_highest = n_highest, 
                    metric = metric)
  return(res)
}




# set parameters
n_draws = 100000
regions <- c("FRA", "GER", "RUS", "ESP", "POL", "IND", "IRI", "GRE", "CZE", 
             "TUR", "HUN", "BRA", "SRI", "SRB", "NED", "ITA", "COL", "UKR",
             "AUT", "SVK", "CHN", "CRO", "ROU", "MEX", "BEL", "SWE", "ENG", 
             "KAZ", "CUB", "GEO", "NOR", "USA", "SUI", "VIE", "LTU", "ARG", 
             "ISR", "DEN", "EGY", "PHI", "BAN", "LAT", "SLO", "CHI", "KEN", 
             "BUL")


n_highest = 1
results1_age <- foreach(i = 1:length(regions), .combine = 'rbind') %dopar% {
  apply_permuatation(region = regions[i], 
                     data = data, 
                     metric = "age")
}

data.table::fwrite(results1_age, "results/results_top1_age.csv")

results1_number_games <- foreach(i = 1:length(regions), .combine = 'rbind') %dopar% {
  apply_permuatation(region = regions[i], 
                     data = data, 
                     metric = "number_games")
}

data.table::fwrite(results1_number_games, "results/results_top1_number_games.csv")


n_highest = 10
results10_age <- foreach(i = 1:length(regions), .combine = 'rbind') %dopar% {
  apply_permuatation(region = regions[i], 
                     data = data, 
                     metric = "age")
}

data.table::fwrite(results10_age, "results/results_top10_age.csv")


results10_number_games <- foreach(i = 1:length(regions), .combine = 'rbind') %dopar% {
  apply_permuatation(region = regions[i], 
                     data = data, 
                     metric = "number_games")
}

data.table::fwrite(results10_number_games, "results/results10_number_games.csv")

n_highest = Inf
results_all_age <- foreach(i = 1:length(regions), .combine = 'rbind') %dopar% {
  apply_permuatation(region = regions[i], 
                     data = data, 
                     metric = "age")
}

data.table::fwrite(results_all_age, "results/results_all_age.csv")

results_all_number_games <- foreach(i = 1:length(regions), .combine = 'rbind') %dopar% {
  apply_permuatation(region = regions[i], 
                     data = data, 
                     metric = "age")
}

data.table::fwrite(results_all_number_games, "results/results_all_number_games.csv")

