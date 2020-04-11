##### Load Libraries #####
library(devtools)
library(dplyr)
library(caret)
library(tidyverse)
library(ggplot2)


##### Load the Dataset #####

devtools::install_github('oganm/dnddata') # connect to the dataset. I obtained permission from the owner to use the dataset for this project
raw_character_data <- dnddata::dnd_chars_singleclass  # download the data from the dnd_chars_singleclass dataset
character_data <- raw_character_data %>% # take the raw data and...
  select(class = justClass, race, str = Str, dex = Dex, con = Con, int = Int, wis = Wis, cha = Cha) # ...select the relevant info
head(character_data)  # display the data


##### Clean the Dataset #####

character_data_class <- character_data %>%  # take the character data and...
  group_by(class) %>% # ...group by class and...
  summarize(n = n())  # ...summarize frequency of each class
character_data_class[11:29,1:2] # display the data

character_data_race<- character_data %>% # take the character data and...
  group_by(race) %>%  # ...group by race and...
  summarize(n = n())  # ...summarize frequency of each race
head(character_data_race, 19)  # display the data

classes <- c("Barbarian", "Bard", "Cleric", "Druid", "Fighter", "Monk", "Paladin", "Ranger", "Rogue", "Sorcerer", "Warlock", "Wizard")  # define the relevant classes
races <- c("dwarf|elf|halfling|human|dragonborn|gnome|half-elf|half-orc|tiefling")  # define the relevant races
character_data <- character_data %>% # take the character data and...
  filter(class %in% classes) %>%   # ...filter out the relevant classes
  filter(str_detect(str_to_lower(race), races)) # ...filter out the relevant races
head(character_data)  # display the data


##### Data Visualizations #####

character_data_no_race <- character_data %>% # for modeling purposes, race does not match the rest of the predictors, so it is removed
  select(-race)

character_data_no_race %>%  # take the data and...
  gather(ability_score, score, -class) %>% # ...gather the ability scores (str, dex, etc) by rank (1-6) and...
  ggplot(aes(ability_score, score, fill = ability_score)) +  # ...plot the ability scores by rank and...
  geom_boxplot() +  # ...set the plot as a box plot to see relative frequencies and...
  facet_wrap(~class, scales = "free") # ...make a plot for each class

character_data_no_race %>% # take the data and...
  gather(ability_score, score, -class) %>% # ...gather the ability scores (str, dex, etc) by rank (1-6) and...
  ggplot(aes(class, score, fill = class)) +  # plot the classes by rank and...
  geom_boxplot() +  # ...set the plot as a box plot to see relative frequencies and...
  facet_wrap(~ability_score, scales = "free") # ...make a plot for each ability score

##### A First Model - kNN #####

fit_knn <- train(class ~ ., # train a kNN model
                 method = "knn",
                 tuneGrid = data.frame(k = seq(1, 50, 1)), 
                 data = character_data_no_race)
ggplot(fit_knn) # plot the kNN model
model_accs <- tibble(model = "Tuned kNN", acc = max(fit_knn$results$Accuracy)) # create a table to display all the calculated accs


##### A Second Model - Optimized Regression Tree #####

fit_tree <- train(class ~ ., # optimize Cp for the regression tree
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
                     data = character_data_no_race)
plot(fit_tree$finalModel, margin = 0.1)  # plot the optimized regression tree and...
text(fit_tree$finalModel, cex = 0.75) # ...add labels
model_accs <- bind_rows(model_accs,
                         tibble(model = "Optimized Regression Tree", acc = max(fit_tree$results$Accuracy))) # add calculated acc to acc table


##### Ranking Abilitiy Scores #####

rank_abilities <- function(ability_score){  # user written function to convert absolute ability scores to ranked ability scores with 6 being the highest rank and one being the lowest rank
  class <- ability_score[1] # define class within the function
  race <- ability_score[2]  # define race within the function
  rank <- rank(ability_score[3:8], ties.method = "max") # rank the ability scores from highest to lowest with ties going high
  str <- rank[[1]]  # define str within the function
  dex <- rank[[2]]  # define dex within the function
  con <- rank[[3]]  # define con within the function
  int <- rank[[4]]  # define int within the function
  wis <- rank[[5]]  # define wis within the function
  cha <- rank[[6]]  # define cha within the function
  result <- list(class = class, race = race, str = str, dex = dex, con = con, int = int, wis = wis, cha = cha)  # display defined variables as output of function as type list
}

ranked_character_data <- apply(character_data, 1, rank_abilities) # use function to convert raw ability scores to ranked ability scores
ranked_character_data <- ranked_character_data %>% # convert list to data frame
  map(as.data.frame) %>% 
  bind_rows()
head(ranked_character_data)  # display the data


##### Data Visualizations #####

ranked_character_data_no_race <- ranked_character_data %>% # for modeling purposes, race does not match the rest of the predictors, so it is removed
  select(-race)

ranked_character_data_no_race %>%  # take the data and...
  gather(ability_score, rank, -class) %>% # ...gather the ability scores (str, dex, etc) by rank (1-6) and...
  ggplot(aes(ability_score, rank, fill = ability_score)) +  # ...plot the ability scores by rank and...
  geom_boxplot() +  # ...set the plot as a box plot to see relative frequencies and...
  facet_wrap(~class, scales = "free") # ...make a plot for each class

ranked_character_data_no_race %>% # take the data and...
  gather(ability_score, rank, -class) %>% # ...gather the ability scores (str, dex, etc) by rank (1-6) and...
  ggplot(aes(class, rank, fill = class)) +  # plot the classes by rank and...
  geom_boxplot() +  # ...set the plot as a box plot to see relative frequencies and...
  facet_wrap(~ability_score, scales = "free") # ...make a plot for each ability score


##### A Third Model - kNN with Ranked Ability Scores #####

fit_knn_ranked <- train(class ~ ., # train a kNN model
                 method = "knn",
                 tuneGrid = data.frame(k = seq(1, 50, 1)), 
                 data = ranked_character_data_no_race)
ggplot(fit_knn_ranked) # plot the kNN model
model_accs <- bind_rows(model_accs,
                        tibble(model = "Tuned kNN Ranked", acc = max(fit_knn_ranked$results$Accuracy))) # add calculated acc to acc table


##### A Fourth Model - Optimized Regression Tree with Ranked Ability Scores #####

fit_tree_ranked <- train(class ~ ., # optimize Cp for the regression tree
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
                     data = ranked_character_data_no_race)
plot(fit_tree_ranked$finalModel, margin = 0.05)  # plot the optimized regression tree
text(fit_tree_ranked$finalModel, cex = 0.65)
model_accs <- bind_rows(model_accs,
                        tibble(model = "Optimized Regression Tree Ranked", acc = max(fit_tree_ranked$results$Accuracy))) # add calculated acc to acc table
