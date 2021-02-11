library(magrittr)
library(data.table)
library(ggplot2)
library(foreach)
library(doParallel)
registerDoParallel(cores=4)

# data from Weiji
# changed line 135635 in the original data file to Abrosimov, Nikolay
# this was necessary in order to be able to read in the data
experience <- data.table::fread("data/experiencedatafromchessbase.csv", 
                                colClasses = c("numeric", "character", 
                                               "character", "character", 
                                               "character", "numeric", 
                                               "character", "numeric"))

# rename experience data with clearer names
names(experience) <- c("fideid", "last_name", "first_name", 
                       "first_rating_list", "last_rating_list", 
                       "number_rating_lists", "sex", "number_games")

# read in october FIDE data list
fide <- data.table::fread("data/20201006_FIDE_ratings.csv")

# list with inactive players I got from Jose
inactive <- readr::read_csv("data/inactive_players_jan20.txt", col_names = "fideid") %>%
  dplyr::mutate(inactive = TRUE)

# get active players by joining the fide list with the list of inactive players
# all that have an NA for inactive are active players --> set all NAs to FALSE
# filter out all that are either inactive or too young and keep the rest
fide_active <- dplyr::full_join(fide, inactive) %>%
  dplyr::mutate(inactive = ifelse(is.na(inactive), FALSE, TRUE)) %>%
  dplyr::filter(birthday < 2000, 
                inactive == FALSE)

# get final data by joining the list of active players with the experience data
# calculate age
data <- experience %>%
  dplyr::select(fideid, number_games) %>%
  dplyr::inner_join(fide_active) %>%
  dplyr::mutate(age =  2021 - as.numeric(birthday))


# function to do a single permutation. Input are two data.tables (for male and
# female), the number of top players to look at and the name of the column of 
# interrest (should be one of c("age", "rating", "number_games"))
permute <- function(data_female, data_male, n_highest = 10, 
                    data_col = NULL) {
  n_small <- nrow(data_female)
  n_large <- nrow(data_male)
  n_total <- n_small + n_large
  
  # combine data into one data set
  all_data <- rbindlist(list(data_female, data_male))
  
  # random permutation of scores
  all_data <- all_data[sample(n_total)]
  
  # draw small and large sample and assign into two groups
  values_small <- all_data[1:n_small]
  values_large <- all_data[(n_small + 1):n_total]
  
  if (n_highest == 1) {
    # select row in data.table where value rating is the highest and get the corresponding value in the data_col
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


# function to apply the permutation to one country. currently number of draws and number of top players are set outside the function
# should probably be changed in the future. 
apply_permuatation <- function(region, data, metric = "rating") {
  # get female and male data by filtering
  data_female <- data %>%
    dplyr::filter(country == region, 
                  sex == "F") %>%
    dplyr::select(c(rating, eval(metric), sex))
  
  data_male <- data %>%
    dplyr::filter(country == region, 
                  sex == "M")  %>%
    dplyr::select(c(rating, eval(metric), sex))
  
  # compute observed difference in the variable of interest in data
  # filter the correct country, then split into males and females and sort each
  # group by rating, then keep only the n_highest ranked rows and obtain the 
  # mean in the variable of interest
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
  
  # apply permutation n_drwas times
  permuted_differences <- replicate(n = n_draws, expr = {
    permute(data_female, data_male, n_highest = n_highest, data_col = metric)
  })
  
  # for some reason the output is is a list now - unsure why, but can just unlist
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




# set parameters (some of this should maybe be done inside the functions)
regions <- c("FRA", "GER", "RUS", "ESP", "POL", "IND", "IRI", "GRE", "CZE", 
             "TUR", "HUN", "BRA", "SRI", "SRB", "NED", "ITA", "COL", "UKR",
             "AUT", "SVK", "CHN", "CRO", "ROU", "MEX", "BEL", "SWE", "ENG", 
             "KAZ", "CUB", "GEO", "NOR", "USA", "SUI", "VIE", "LTU", "ARG", 
             "ISR", "DEN", "EGY", "PHI", "BAN", "LAT", "SLO", "CHI", "KEN", 
             "BUL")

n_highest = 1
n_draws = 100

# apply permutation to each country. Use %dopar% for parallel computing
results1_age <- foreach(i = 1:length(regions), .combine = 'rbind') %dopar% {
  apply_permuatation(region = regions[i], 
                     data = data, 
                     metric = "age")
}

# write results csv
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

