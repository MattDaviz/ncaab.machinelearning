# Load Libraries -------
library(readr); library(dplyr); library(tidyr); library(reshape)

# Load Data -----------
seasons <- read_csv("../data/Seasons.csv")
teams <- read_csv("../data/Teams.csv")
TourneyCompactResults <- read_csv("../data/TourneyCompactResults.csv")
seeds <- read_csv("../data/TourneySeeds.csv")
submission <- read_csv("../data/SampleSubmission.csv")
massey_ordinals_2003_2016 <- read_csv('../data/massey_ordinals_2003-2016.csv')
Massey_ordinals_2017 <- read_csv('../data/Massey_ordinals_2017.csv')

# Ensemble tournament ordinals test ----------
# Test all systems
ensemble <- massey_ordinals_2003_2016 %>% 
  filter(rating_day_num == 133)
ensemble.wide <- spread(ensemble, sys_name, orank)
ensemble.17 <- Massey_ordinals_2017 %>% 
  filter(rating_day_num == 133)
ensemble.17.wide <- spread(ensemble.17, sys_name, orank)

    # Create data frame of games to predict
games.to.predict <- cbind(submission$Id, colsplit(submission$Id, split = "_", names = c('season', 'team1', 'team2')))  

    # Subset tournament data to only include years of which we have massey ordinals data
ordinals.tourney.data <- TourneyCompactResults %>% 
  filter(Season >= 2003)

ordinals.tourney.data$team1 <- ifelse(ordinals.tourney.data$Wteam > ordinals.tourney.data$Lteam, ordinals.tourney.data$Lteam, ordinals.tourney.data$Wteam) #If the ID of the winning team is higher than the ID of the losing team, team1 is the losing team, else its the winning team
ordinals.tourney.data$team2 <- ifelse(ordinals.tourney.data$Wteam > ordinals.tourney.data$Lteam, ordinals.tourney.data$Wteam, ordinals.tourney.data$Lteam) #Vice versa to find team2
ordinals.tourney.data$team1Victory <- ifelse(ordinals.tourney.data$Wteam == ordinals.tourney.data$team1, 1, 0)

# Merge data sets together
train <- merge(ordinals.tourney.data, ensemble.wide[,c(1, 3:135)], by.x = c('Season', 'team1'), by.y = c('season','team'))
names(train)[12:143] <- paste('team1.', names(train)[12:143], sep = '')
train <- merge(train, ensemble.wide[,c(1, 3:135)], by.x = c('Season', 'team1'), by.y = c('season', 'team'))
names(train)[144:275] <- paste('team2.', names(train)[144:275], sep = '')

# Create test and train data sets
test <- train %>% 
  filter(Season >= 2016)
train <- train %>% 
  filter(Season <= 2015)

      # THIS IS WHERE I STOPPED 3.20.2017
lr_model <- glm(team1Victory ~ ., 
                family=binomial(link = 'logit'), data = train, na.action = na.exclude)

#Test it on the test dataset
test_prediction <- predict(lr_model, test, type = 'response')

#Compute practical accuracy measure (betting on a win or a loss)
test$guess <- round(test_prediction, digits=0)
test$correct <- ifelse(test$guess == test$team1Victory, 1, 0)
sum(test$correct)/67

test$prediction <- test_prediction
test$submissionID <- paste(test$Season, test$team1, test$team2, sep = "_")

# Specific systems in use
ensemble <- massey_ordinals_2003_2016 %>% 
  filter(rating_day_num == 133,
         sys_name %in% c('BOB', 'MOR', 'RTH', 'SAG', 'WOL', 'POM', 'WLK', 'DOL', 'COL', 'RPI'))

ensemble.17 <- Massey_ordinals_2017 %>% 
  filter(rating_day_num == 133, 
         sys_name %in% c('BOB', 'MOR', 'RTH', 'SAG', 'WOL', 'POM', 'WLK', 'DOL', 'COL', 'RPI'))

ensemble.17.wide <- spread(ensemble.17, sys_name, orank)

games.to.predict <- cbind(submission$id, colsplit(submission$id, split = "_", names = c('season', 'team1', 'team2')))  

ordinals.tourney.data <- TourneyCompactResults %>% 
  filter(Season >= 2003)

ordinals.tourney.data$team1 <- ifelse(ordinals.tourney.data$Wteam > ordinals.tourney.data$Lteam, ordinals.tourney.data$Lteam, ordinals.tourney.data$Wteam) #If the ID of the winning team is higher than the ID of the losing team, team1 is the losing team, else its the winning team
ordinals.tourney.data$team2 <- ifelse(ordinals.tourney.data$Wteam > ordinals.tourney.data$Lteam, ordinals.tourney.data$Wteam, ordinals.tourney.data$Lteam) #Vice versa to find team2
ordinals.tourney.data$team1Victory <- ifelse(ordinals.tourney.data$Wteam == ordinals.tourney.data$team1, 1, 0)

train <- merge(ordinals.tourney.data, ensemble.wide[,c("season", "team", 'BOB', 'MOR', 'RTH', 'SAG', 'WOL', 'POM', 'WLK', 'DOL', 'COL',
                                                       'RPI')], by.x = c("Season","team1"), by.y = c("season","team"))
names(train)[12:21] <- paste("team1.",names(train)[12:21],sep = "")

train <- merge(train, ensemble.wide[,c("season", "team", 'BOB', 'MOR', 'RTH', 'SAG', 'WOL', 'POM', 'WLK', 'DOL', 'COL', 'RPI')], 
               by.x = c('Season', 'team2'), by.y = c('season', 'team'))
names(train)[22:31] <- paste("team2.",names(train)[22:31],sep = "")

test <- train %>% 
  filter(Season >= 2016)
train <- train %>% 
  filter(Season <= 2015)
str(train)
lr_model <- glm(team1Victory ~ team1.BOB + team1.MOR + team1.RTH + team1.SAG + team1.WOL + team1.POM + team1.WLK + team1.DOL + team1.COL +
                  team1.RPI + team2.BOB + team2.MOR + team2.RTH + team2.SAG + team2.WOL + team2.POM + team2.WLK + team2.DOL + team2.COL +
                  team2.RPI
                , family=binomial(link = 'logit'), data = train)
summary(lr_model)

#Test it on the test dataset
test_prediction <- predict(lr_model, test, type = 'response')

#Compute practical accuracy measure (betting on a win or a loss)
test$guess <- round(test_prediction, digits=0)
test$correct <- ifelse(test$guess == test$team1Victory, 1, 0)
sum(test$correct)/67

test$prediction <- test_prediction
test$submissionID <- paste(test$Season, test$team1, test$team2, sep = "_")

games.to.predict <- cbind(submission.2016$Id, colsplit(submission.2016$Id, split = "_", names = c('season', 'team1', 'team2')))  
```
## Merge predictions into Submission and write it to a file
```{r}
submission <- merge(games.to.predict, test[,c("submissionID","prediction")], by.x = c("submission.2016$Id"), by.y = c("submissionID"), all.x = TRUE)
submission$pred <- ifelse(is.na(submission$prediction),.5, submission$prediction)
submission$prediction <- NULL
submission <- submission[,c(1,5)]
colnames(submission)[1] <- 'id'
write.csv(submission, file="ensemble2016.csv", row.names=FALSE)

set.seed(69)
library('kaggleNCAA')
# You MUST move your submission file into the kaggleNCAA package folder location!!
#(C:\Users\Matthew\Documents\R\win-library\3.3\kaggleNCAA)
f <- system.file('ensemble2016.csv', package = 'kaggleNCAA')
dat <- parseBracket(f)
sim <- simTourney(dat, 1000, year = 2016, progress=TRUE)
bracket <- extractBracket(sim)
printableBracket(bracket)

setwd('C:\\Users\\mattd\\Dropbox\\NCAAM')

# Create unique output filename
output_filename <- '2016 ensemble 1000.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 2400, res = 300, quality = 400)

printableBracket(bracket)
dev.off()

#Do it for 2017
games.to.predict <- cbind(SampleSubmission$Id, colsplit(SampleSubmission$Id, split = "_", names = c('season', 'team1', 'team2')))  

# Train dataset does not have BOB yet
train <- merge(ordinals.tourney.data, ensemble.wide[,c("season", "team", 'MOR', 'RTH', 'SAG', 'WOL', 'POM', 'WLK', 'DOL', 'COL',
                                                       'RPI')], by.x = c("Season","team1"), by.y = c("season","team"))
names(train)[12:20] <- paste("team1.",names(train)[12:20],sep = "")

train <- merge(train, ensemble.wide[,c("season", "team", 'MOR', 'RTH', 'SAG', 'WOL', 'POM', 'WLK', 'DOL', 'COL', 'RPI')], 
               by.x = c('Season', 'team2'), by.y = c('season', 'team'))
names(train)[21:29] <- paste("team2.",names(train)[21:29],sep = "")

# Create 2017 Test dataset
test <- merge(games.to.predict, ensemble.17.wide[,c("season", "team", 'MOR', 'RTH', 'SAG', 'WOL', 'POM', 'WLK', 'DOL', 'COL',
                                                    'RPI')], by.x = c("season","team1"), by.y = c("season","team"))
names(test)[5:13] <- paste('team1.', names(test)[5:13], sep = '')
test <- merge(test, ensemble.17.wide[,c('season', 'team', 'MOR', 'RTH', 'SAG', 'WOL', 'POM', 'WLK', 'DOL', 'COL',
                                  'RPI')], by.x = c('season', 'team2'), by.y = c('season', 'team'))
names(test)[14:22] <- paste('team2.', names(test)[14:22], sep = '')

lr_model <- glm(team1Victory ~ team1.MOR + team1.RTH + team1.SAG + team1.WOL + team1.POM + team1.WLK + team1.DOL + team1.COL +
                  team1.RPI + team2.MOR + team2.RTH + team2.SAG + team2.WOL + team2.POM + team2.WLK + team2.DOL + team2.COL +
                  team2.RPI
                , family=binomial(link = 'logit'), data = train)
summary(lr_model)

#Test it on the test dataset
test_prediction <- predict(lr_model, test, type = 'response')

#Compute practical accuracy measure (betting on a win or a loss)
test$guess <- round(test_prediction, digits=0)
test$correct <- ifelse(test$guess == test$team1Victory, 1, 0)
sum(test$correct)/67

test$prediction <- test_prediction
test$submissionID <- paste(test$season, test$team1, test$team2, sep = "_")

submission <- merge(games.to.predict, test[,c("submissionID","prediction")], by.x = c("SampleSubmission$Id"), by.y = c("submissionID"), all.x = TRUE)
submission$pred <- ifelse(is.na(submission$prediction),.5, submission$prediction)
submission$prediction <- NULL
submission <- submission[,c(1,5)]
colnames(submission)[1] <- 'id'
write.csv(submission, file="ensemble2017.csv", row.names=FALSE)


library('kaggleNCAA')
# You MUST move your submission file into the kaggleNCAA package folder location!!
#(C:\Users\Matthew\Documents\R\win-library\3.3\kaggleNCAA)
f <- system.file('ensemble2017.csv', package = 'kaggleNCAA')
dat <- parseBracket(f)

set.seed(69)
simulation <- simTourney(dat, 1000, year = 2017, progress=TRUE)
bracket <- extractBracket(simulation)
printableBracket(bracket)

setwd('C:\\Users\\mattd\\Dropbox\\NCAAM')

# Create unique output filename
output_filename <- '2017 ensemble 1000.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 2400, res = 300, quality = 400)

printableBracket(bracket)
dev.off()


# ALL Ensemble tournament ordinals test ----------
# Get all results for 2017
ensemble.17 <- Massey_ordinals_2017 %>% 
  filter(rating_day_num == 133)


ensemble.17.wide <- spread(ensemble.17, sys_name, orank)
sys_name.ensemble <- unique(ensemble.17$sys_name)

ensemble.test <- massey_ordinals_2003_2016 %>% 
  filter(rating_day_num == 133,
         sys_name %in% sys_name.ensemble) %>% 
  group_by(sys_name) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  top_n(12)

ensemble.wide <- spread(ensemble, sys_name, orank)

games.to.predict <- cbind(submission.2016$Id, colsplit(submission.2016$Id, split = "_", names = c('season', 'team1', 'team2'))) 
games.to.predict <- cbind(submission$id, colsplit(submission$id, split = "_", names = c('season', 'team1', 'team2')))  

ordinals.tourney.data <- TourneyCompactResults %>% 
  filter(Season >= 2003)

ordinals.tourney.data$team1 <- ifelse(ordinals.tourney.data$Wteam > ordinals.tourney.data$Lteam, ordinals.tourney.data$Lteam, ordinals.tourney.data$Wteam) #If the ID of the winning team is higher than the ID of the losing team, team1 is the losing team, else its the winning team
ordinals.tourney.data$team2 <- ifelse(ordinals.tourney.data$Wteam > ordinals.tourney.data$Lteam, ordinals.tourney.data$Wteam, ordinals.tourney.data$Lteam) #Vice versa to find team2
ordinals.tourney.data$team1Victory <- ifelse(ordinals.tourney.data$Wteam == ordinals.tourney.data$team1, 1, 0)

train <- merge(ordinals.tourney.data, ensemble.wide[,c(1,3:45)], by.x = c("Season","team1"), by.y = c("season","team"))
names(train)[12:53] <- paste("team1.",names(train)[12:53],sep = "")

train <- merge(train, ensemble.wide[,c(1,3:45)], by.x = c('Season', 'team2'), by.y = c('season', 'team'))
names(train)[54:95] <- paste("team2.",names(train)[54:95],sep = "")

train$team1.DC2

test <- train %>% 
  filter(Season >= 2016)
train <- train %>% 
  filter(Season <= 2016)
str(train)
lr_model <- glm(team1Victory ~ team1.MOR + team1.RTH + team1.SAG + team1.WOL + team1.POM + team1.WLK + team1.DOL + team1.COL + team1.RPI +
                  team1.BIH + team1.WOB + team1.DOK + team2.MOR + team2.RTH + team2.SAG + team2.WOL + team2.POM + team2.WLK + team2.DOL + team2.COL + team2.RPI +
                  team2.BIH + team2.WOB + team2.DOK +
                  team2.WOL, family=binomial(link = 'logit'), data = train)
summary(lr_model)

#Test it on the test dataset
test_prediction <- predict(lr_model, test, type = 'response')

#Compute practical accuracy measure (betting on a win or a loss)
test$guess <- round(test_prediction, digits=0)
test$correct <- ifelse(test$guess == test$team1Victory, 1, 0)
sum(test$correct)/67

test$prediction <- test_prediction
test$submissionID <- paste(test$Season, test$team1, test$team2, sep = "_")

games.to.predict <- cbind(submission.2016$Id, colsplit(submission.2016$Id, split = "_", names = c('season', 'team1', 'team2')))  
```
## Merge predictions into Submission and write it to a file
```{r}
submission <- merge(games.to.predict, test[,c("submissionID","prediction")], by.x = c("submission.2016$Id"), by.y = c("submissionID"), all.x = TRUE)
submission$pred <- ifelse(is.na(submission$prediction),.5, submission$prediction)
submission$prediction <- NULL
submission <- submission[,c(1,5)]
colnames(submission)[1] <- 'id'
write.csv(submission, file="allensemble2016.csv", row.names=FALSE)

set.seed(69)
library('kaggleNCAA')
# You MUST move your submission file into the kaggleNCAA package folder location!!
#(C:\Users\Matthew\Documents\R\win-library\3.3\kaggleNCAA)
f <- system.file('allensemble2016.csv', package = 'kaggleNCAA')
dat <- parseBracket(f)
sim <- simTourney(dat, 1000, year = 2016, progress=TRUE)
bracket <- extractBracket(sim)
printableBracket(bracket)

setwd('C:\\Users\\mattd\\Dropbox\\NCAAM')

# Create unique output filename
output_filename <- '2016 all ensemble 1000.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 2400, res = 300, quality = 400)

printableBracket(bracket)
dev.off()

#Do it for 2017
games.to.predict <- cbind(SampleSubmission$Id, colsplit(SampleSubmission$Id, split = "_", names = c('season', 'team1', 'team2')))  

# Train dataset does not have BOB yet
train <- merge(ordinals.tourney.data, ensemble.wide[,c(1,3:53)], by.x = c("Season","team1"), by.y = c("season","team"))
names(train)[12:20] <- paste("team1.",names(train)[12:20],sep = "")

train <- merge(train, ensemble.wide[,c("season", "team", 'MOR', 'RTH', 'SAG', 'WOL', 'POM', 'WLK', 'DOL', 'COL', 'RPI')], 
               by.x = c('Season', 'team2'), by.y = c('season', 'team'))
names(train)[21:29] <- paste("team2.",names(train)[21:29],sep = "")

# Create 2017 Test dataset
test <- merge(games.to.predict, ensemble.17.wide[,c(1,3:53)], by.x = c("season","team1"), by.y = c("season","team"))
names(test)[5:54] <- paste('team1.', names(test)[5:54], sep = '')
test <- merge(test, ensemble.17.wide[,c(1,3:53)], by.x = c('season', 'team2'), by.y = c('season', 'team'))
names(test)[55:104] <- paste('team2.', names(test)[55:104], sep = '')

#Test it on the test dataset
test_prediction <- predict(lr_model, test, type = 'response')

#Compute practical accuracy measure (betting on a win or a loss)
test$guess <- round(test_prediction, digits=0)
test$correct <- ifelse(test$guess == test$team1Victory, 1, 0)
sum(test$correct)/67

test$prediction <- test_prediction
test$submissionID <- paste(test$season, test$team1, test$team2, sep = "_")

submission <- merge(games.to.predict, test[,c("submissionID","prediction")], by.x = c("SampleSubmission$Id"), by.y = c("submissionID"), all.x = TRUE)
submission$pred <- ifelse(is.na(submission$prediction),.5, submission$prediction)
submission$prediction <- NULL
submission <- submission[,c(1,5)]
colnames(submission)[1] <- 'id'
write.csv(submission, file="allensemble2017.csv", row.names=FALSE)

set.seed(70)
library('kaggleNCAA')
# You MUST move your submission file into the kaggleNCAA package folder location!!
#(C:\Users\Matthew\Documents\R\win-library\3.3\kaggleNCAA)
f <- system.file('allensemble2017.csv', package = 'kaggleNCAA')
dat <- parseBracket(f)
sim <- simTourney(dat, 1000, year = 2017, progress=TRUE)
bracket <- extractBracket(sim)
printableBracket(bracket)

setwd('C:\\Users\\mattd\\Dropbox\\NCAAM')

# Create unique output filename
output_filename <- '2017 all ensemble 1000 - second run.jpeg'

# Open the file for the plot to be written to
jpeg(output_filename, height = 2400, width = 2400, res = 300, quality = 400)

printableBracket(bracket)
dev.off()
