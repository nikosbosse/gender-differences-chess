library(magrittr)
library(ggplot2)

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

regions <- c("FRA", "GER", "RUS", "ESP", "POL", "IND", "IRI", "GRE", "CZE", 
             "TUR", "HUN", "BRA", "SRI", "SRB", "NED", "ITA", "COL", "UKR",
             "AUT", "SVK", "CHN", "CRO", "ROU", "MEX", "BEL", "SWE", "ENG", 
             "KAZ", "CUB", "GEO", "NOR", "USA", "SUI", "VIE", "LTU", "ARG", 
             "ISR", "DEN", "EGY", "PHI", "BAN", "LAT", "SLO", "CHI", "KEN", 
             "BUL")


data <- experience %>%
  dplyr::select(fideid, number_games) %>%
  dplyr::inner_join(fide) %>%
  dplyr::filter(!(flag == "i"), 
                !(flag == "wi"), 
                country %in% regions)

# summary table
data %>%
  dplyr::group_by(sex) %>%
  dplyr::summarise(mean = mean(number_games), 
                   max = max(number_games), 
                   n_obs = dplyr::n(), 
                   sum = sum(number_games), 
                   sd = sd(number_games), 
                   cor = cor(rating, number_games)) %>%
  knitr::kable(format = "latex")



data_top_10 <- data %>%
  dplyr::group_by(country, sex) %>%
  dplyr::arrange(rating) %>%
  dplyr::slice(10)

# summary table for the top 10 players only
data_top_10 %>%
  dplyr::group_by(sex) %>%
  dplyr::summarise(mean = mean(number_games), 
                   max = max(number_games), 
                   n_obs = dplyr::n(), 
                   sum = sum(number_games), 
                   sd = sd(number_games), 
                   cor = cor(rating, number_games)) %>%
  knitr::kable(format = "latex")

# scatter plot for the difference between rating and the difference in experiene
data_top_10 %>%
  dplyr::mutate(age =  2021 - as.numeric(birthday)) %>%
  dplyr::select(rating, number_games, country, sex, age) %>%
  dplyr::group_by(sex, country) %>%
  dplyr::summarise(number_games = mean(number_games, na.rm = TRUE), 
                   age = mean(age, na.rm = TRUE), 
                   rating = mean(rating, na.rm = TRUE), 
                   .groups = "drop_last") %>%
  tidyr::pivot_wider(values_from = c(age, rating, number_games), names_from = sex) %>%
  dplyr::mutate(age_diff = age_M - age_F, 
                rating_diff = rating_M - rating_F, 
                experience_diff = number_games_M - number_games_F) %>%
  dplyr::select(country, age_diff, rating_diff, experience_diff) %>%
  tidyr::pivot_longer(cols = c(age_diff, experience_diff), 
                      names_to = "category") %>%
  ggplot2::ggplot(ggplot2::aes(y = rating_diff, x = value)) + 
  ggplot2::geom_point() + 
  ggplot2::facet_wrap(~ category, scales = "free")



# Distribution of experience by gender
ggplot() + 
  geom_histogram(data = data %>%
                   dplyr::filter(number_games < 2000, 
                                 sex == "F"), 
                 aes(y = (..count..)/sum(..count..), 
                     x = number_games, 
                     fill = sex),
                 binwidth = 50, alpha = 0.4) + 
  geom_histogram(data = data %>%
                   dplyr::filter(number_games < 2000, 
                                 sex == "M"), 
                 aes(y = (..count..)/sum(..count..), 
                     x = number_games, 
                     fill = sex),
                 binwidth = 50, alpha = 0.4) + 
  facet_grid(sex ~ .) + 
  theme_minimal() + 
  labs(title = "Relative experience by Gender", 
       x = "Proportion of players who played a given number of games", 
       y = "Number of games played")

ggplot2::ggsave(filename = "results/plots/distribution_experience_gender.png")


# Distribution of experience by gender
ggplot() + 
  geom_histogram(data = data_top_10 %>%
                   dplyr::filter(number_games < 2000, 
                                 sex == "F"), 
                 aes(y = (..count..)/sum(..count..), 
                     x = number_games, 
                     fill = sex),
                 binwidth = 50, alpha = 0.4) + 
  geom_histogram(data = data_top_10 %>%
                   dplyr::filter(number_games < 2000, 
                                 sex == "M"), 
                 aes(y = (..count..)/sum(..count..), 
                     x = number_games, 
                     fill = sex),
                 binwidth = 50, alpha = 0.4) + 
  facet_grid(sex ~ .) + 
  theme_minimal() + 
  labs(title = "Relative experience by Gender")



data_top_10 %>%
  dplyr::filter(number_games == 0)


# Rating and experience
data %>%
  dplyr::filter(number_games > 100) %>%
  ggplot(aes(x = number_games, y = rating, color = sex)) + 
  geom_point(alpha = 0.3, 
             size = 0.01) + 
  geom_smooth(method='lm', formula = y ~ x) + 
  theme_minimal() + 
  labs(title = "Correlation between rating and experience")
# could be log-shaped instead?

ggplot2::ggsave("results/plots/experience_and_ratings.png")

data %>%
  dplyr::filter(number_games > 100) %>%
  ggplot(aes(x = number_games, y = rating, color = sex)) + 
  geom_point(alpha = 0.3, 
             size = 0.01) + 
  geom_smooth(method='lm', formula = y ~ log(x)) + # y ~ log(x)
  theme_minimal() + 
  labs(title = "Correlation between rating and experience")
# could be log-shaped instead?

ggplot2::ggsave("results/plots/experience_and_ratings_log.png")
  
lm(formula = rating ~ sex + number_games + sex * number_games, data = data) %>%
  summary()

glm(formula = rating ~ sex + number_games + sex * number_games, 
    family = gaussian(link = "log"), 
    data = data) %>%
  summary()

# experience differences in countries?
data %>%
  dplyr::group_by()
