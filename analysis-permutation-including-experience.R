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


permute <- function(data_female, data_male, n_highest = 10, 
                    data_col = NULL) {
  n_small <- nrow(data_female)
  n_large <- nrow(data_male)
  n_total <- n_small + n_large
  
  all_data <- rbindlist(list(data_female, data_male))
  
  # shuffle scores
  all_data <- all_data[sample(n_total)]
  
  # draw small and large sample
  values_small <- all_data[1:n_small]
  values_large <- all_data[(n_small + 1):n_total]
  
  if (n_highest == 1) {
    # select row in data.table where value rating is the highest and get the correspoding value in the data_col
    mean_small <- values_small[which(values_small[["rating"]] == max(values_small[["rating"]]))][[data_col]]
    mean_large <- values_large[which(values_large[["rating"]] == max(values_large[["rating"]]))][[data_col]]
  } else if (n_highest == Inf) {
    mean_small <- mean(values_small[[data_col]])
    mean_large <- mean(values_large[[data_col]])
  } else {
    # logic from inner to outer: sort data.table according to rating, take the last n_highest rows and take the mean of the data_col value
    mean_small <- mean(tail(values_small[order(values_small[["rating"]])], 10)[[data_col]])
    mean_large <- mean(tail(values_large[order(values_large[["rating"]])], 10)[[data_col]])
  }
  return(mean_large - mean_small)
}

apply_permuatation <- function(region, data, metric = "rating") {
  data_female <- data %>%
    dplyr::filter(country == region, 
                  sex == "F") %>%
    dplyr::select(c(rating, eval(metric), sex))
  
  data_male <- data %>%
    dplyr::filter(country == region, 
                  sex == "M")  %>%
    dplyr::select(c(rating, eval(metric), sex))
  
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
  
  # turn into data.table for faster computation
  data.table::setDT(data_female)
  data.table::setDT(data_male)
  
  permuted_differences <- replicate(n = n_draws, expr = {
    permute(data_female, data_male, n_highest = n_highest, data_col = metric)
  })
  
  # for some reason this is a list now - unsure why
  if (is.list(permuted_differences)) {
    permuted_differences <- unlist(permuted_differences)
  }
  
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
n_draws = 100
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
                     metric = "number_games")
}

data.table::fwrite(results_all_number_games, "results/results_all_number_games.csv")

