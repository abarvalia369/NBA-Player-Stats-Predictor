library(readr)
library(stats)
install.packages("devtools")
library(devtools)
devtools::install_github("janish-parikh/ZTest")
library(HypothesisTesting)


data <- read.csv("all_seasons.csv")
View(data)
head(data)
#---------------------------------Queries
#group by team(Lakers)
lakers_players <- subset(data, team_abbreviation == "LAL")
View(lakers_players)

#Num of players from each country
players_by_country <- table(data$country)
View(players_by_country)

#Avg height for each team
avg_height_by_team <- tapply(data$player_height, data$team_abbreviation, mean)
View(avg_height_by_team)

#Num of players drafter each year
players_by_draft_year <- table(data$draft_year)
View(players_by_draft_year)

number_one_picks <- subset(data, draft_number == '1')
View(number_one_picks)

#players with points > 20
high_scorers <- subset(data, pts > 20)
View(high_scorers)

#players with points > 30
higher_scorers <- subset(data, pts > 30)
View(higher_scorers)

high_rebounds <- subset(data, reb > 10)
View(high_rebounds)

high_assists <- subset(data, ast > 10)
View(high_assists)

avg_weight_by_country <- tapply(data$player_weight, data$country, mean)
View(avg_weight_by_country)

players_by_age <- table(data$age)
View(players_by_age)
#-------------------------------------Visualizers

#Scatterplot(height vs weight)
plot(data$player_height, data$player_weight, 
     main = "Player Height vs. Weight", 
     xlab = "Height (cm)", 
     ylab = "Weight (kg)", 
     pch = 19)

#Barplot(num of players per country)
top_countries <- head(sort(players_by_country, decreasing = TRUE), 10)
barplot(top_countries, 
        main = "Top 10 Countries by Number of Players", 
        xlab = "Country", 
        ylab = "Number of Players")

#Player ages by team
boxplot(data$age ~ data$team_abbreviation, 
        main = "Player Ages by Team", 
        xlab = "Team", 
        ylab = "Age", 
        las = 2, 
        cex.axis = 0.7)

#Scatterplot(height vs rebounds)
plot(data$player_height, data$reb, 
     main = "Player Height vs. Rebounds Per Game", 
     xlab = "Height (cm)", 
     ylab = "Rebounds Per Game", 
     pch = 19, # Type of point. 19 is a solid circle.
     col = "green") # Color of points

#Scatterplot(height vs assists)
plot(data$player_height, data$ast, 
     main = "Player Height vs Assists", 
     xlab = "Height (cm)", 
     ylab = "Assists per Game", 
     pch = 19, 
     col = "red")

#Scatterplot(height vs points)
plot(data$player_height, data$pts, 
     main = "Player Height vs. Points Per Game", 
     xlab = "Height (cm)", 
     ylab = "Points Per Game", 
     pch = 20, 
     col = "blue")

# Barplot avg points for top 10 countries
avg_pts_by_country <- aggregate(pts ~ country, data = data, FUN = mean)
View(avg_pts_by_country)
top_countries_by_pts <- avg_pts_by_country[order(-avg_pts_by_country$pts),][1:10,]
View(top_countries_by_pts)
barplot(top_countries_by_pts$pts, 
        names.arg = top_countries_by_pts$country, 
        las = 2, # Rotate axis labels to be perpendicular
        main = "Average Points Per Game of the Top Ten Scoring Countries", 
        xlab = "Country", 
        ylab = "Average Points Per Game",
        col = "purple",
        cex.names = 0.8) # Adjust size of country labels if necessary
#boxplot
top_countries_names <- names(top_countries)
data_top_countries <- subset(data, country %in% top_countries_names)
boxplot(pts ~ country, data = data_top_countries, 
        main = "Points per Game by Country (Top 10)", 
        xlab = "Country", 
        ylab = "Points per Game", 
        col = "orange",
        las = 2)

#-------------------------------------------Predictor
#average points per game by country (Top 10 countries)

data$from_top_country <- ifelse(data$country %in% top_countries_by_pts$country, 1, 0)
country_model <- glm(from_top_country ~ player_height + player_weight, data = data, family = "binomial")
summary(country_model)

# Predict the probability of being from one of the top 10 countries
data$predicted_prob_country <- predict(country_model, type = "response")

# Convert probabilities to binary predictions
data$predicted_from_top_country <- ifelse(data$predicted_prob_country > 0.5, 1, 0)

# Calculate the accuracy of the model
country_accuracy <- mean(data$predicted_from_top_country == data$from_top_country)
print(paste("Accuracy of the country predictor:", country_accuracy))
#------------------------------------
#------------------------------------

# Narrow query M
M <- mean(subset(data, country == "Spain" & draft_round == "1")$pts)

# Query M0
M0 <- league_avg_pts

# Show that M satisfies one of the inequalities
M > 2 * M0 # or M < 1/2 * M0

#-------------------------------------

#us and uk have equal pt avg
#they dont
perm1 <- z_test_from_data(data, "country", "pts", "USA","United Kingdom")
cat("The p-value of the hypothesis is equal to:",perm1)

not_number_one_picks <- subset(data, draft_number > '1')
View(not_number_one_picks)

#Miami and minnestoa have the same avg reb all time
#dont have same avg rebounds
perm2 <- z_test_from_data(data, "team_abbreviation", "reb", "ATL","GSW")
cat("The p-value of the hypothesis is equal to:",perm2)

jordan <- subset(data, player_name == "Michael Jordan")
View(jordan)
lebron <- subset(data, player_name == "LeBron James")
View(lebron)

#jordan and lebron have same avg points per game
#they dont
perm3 <- z_test_from_data(data, "player_name", "pts", "Michael Jordan","LeBron James")
cat("The p-value of the hypothesis is equal to:",perm3)
#--------------------------

# Calculate the overall average points per game
M0 <- mean(data$pts)

# and a specific condition, e.g., players older than 30
M <- mean(subset(data, team_abbreviation == "LAL" & age > 30)$pts)

# Check if the condition M > 2 * M0 or M < 1/2 * M0 is satisfied
return <- M > 2 * M0 | M < 1/2 * M0

# Output the results
cat("M0 (overall average points per game):", M0, "\n")
cat("M (average points per game for Lakers over 30 years old):", M, "\n")
cat("Does M satisfy the extreme result condition? ", return, "\n")
#-------------------------------------------------------

#belief player is from USA
#observation player scores more than 20 points per game

# prior probability and prior odds of the belief that a player is from the USA
prior_prob_usa <- nrow(data[data$country == "USA",]) / nrow(data)
prior_odds_usa <- prior_prob_usa / (1 - prior_prob_usa)
cat(prior_prob_usa, prior_odds_usa)

# true positive rate: players from the USA who score more than 20 points per game
true_positive <- nrow(data[data$country == "USA" & data$pts > 20,]) / nrow(data[data$country == "USA",])
cat(true_positive)
# false positive rate: players not from the USA who score more than 20 points per game
false_positive <- nrow(data[data$country != "USA" & data$pts > 20,]) / nrow(data[data$country != "USA",])
cat(false_positive)
# likelihood ratio
likelihood_ratio <- true_positive / false_positive
cat(likelihood_ratio)
# posterior odds
posterior_odds_usa <- likelihood_ratio * prior_odds_usa
cat(posterior_odds_usa)


# factor k by which the posterior odds are greater than the prior odds
k <- posterior_odds_usa / prior_odds_usa
cat(k)
# Print
cat("Prior Odds of USA:", prior_odds_usa, "\n")
cat("Likelihood Ratio:", likelihood_ratio, "\n")
cat("Posterior Odds of USA:", posterior_odds_usa, "\n")
cat("Factor k:", k, "\n")

#-------------------------------------------------------
#contingency table

data$high_scorer <- ifelse(data$pts > 20, "Yes", "No")

# Build the contingency table
T <- table(data$team_abbreviation, data$high_scorer)

#categorical variables(teams, highscorers(Y/N))
# compare the likelihood of being a high scorer for two specific teams
# Fcompare the Lakers "LAL" and the Celtics "BOS"
T_LAL_Yes <- T["LAL", "Yes"]
T_BOS_No <- T["BOS", "No"]
T_Yes_Total <- sum(T[, "Yes"])
T_No_Total <- sum(T[, "No"])

# Calculate the likelihood ratio
likelihood_ratio <- (T_LAL_Yes / T_Yes_Total) / (T_BOS_No / T_No_Total)

# Output the contingency table and likelihood ratio
print(T)
cat("The likelihood ratio comparing 'LAL' high scorers to 'BOS' non-high scorers is:", likelihood_ratio, "\n")
