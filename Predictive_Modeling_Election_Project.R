### EDA

# load data
library(readxl)
dataset <- read_excel("C:/Users/lucky/Desktop/Predictive Modeling/Predictive Modeling Project/Simmons_Alicia_dataset.xlsx", 
                      sheet = "dataset")
View(dataset)

# create unique id for each observation
dataset$id <- paste(dataset$year,"-",dataset$state_name, sep="")

# Change all Character Variables to Factor Variables
dataset[sapply(dataset, is.character)] <- 
  lapply(dataset[sapply(dataset,is.character)], as.factor)

# examine correlations between numeric variables before transformations
library("dplyr")
#raw_data_cor <- cor(select_if(dataset[,4:13],is.numeric))
raw_data_cor <- cor(dataset[,c('population','percent_registered','gdp','median_income','unemployment_rate','violent_crime_occurances','property_crime_occurances')])
View(raw_data_cor)
library(corrplot)
corrplot(raw_data_cor, order='original', tl.cex = .85)

# apply transformations to the data

# calculate population growth variable
dataset$population_growth <- (dataset$population - dataset$prev_population) / dataset$prev_population

# calculate per-capita GDP
dataset$gdp_percap <- dataset$gdp / dataset$population

# normalize median income
# divide by mean for each state
state_med_income_means <- aggregate(dataset$median_income,by=list(dataset$state_name),FUN=mean)
names(state_med_income_means) <- c("state_name","mean_state_med_income")
if(!('mean_state_mean_income' %in% names(dataset)))
  dataset <- merge(dataset, state_med_income_means, id.vars = "state_name")
dataset$median_income_normalized <- dataset$median_income / dataset$mean_state_med_income

## normalize unemployment rate
# divide by mean for each state
state_unemployment_means = aggregate(dataset$unemployment_rate,by=list(dataset$state_name),FUN=mean)
names(state_unemployment_means) <- c("state_name","mean_state_unemployment_rate")
if(!('mean_state_unemployment_rate' %in% names(dataset)))
  dataset <- merge(dataset, state_unemployment_means, id.vars = "state_name")
dataset$unemployment_rate_normalized <- dataset$unemployment_rate / dataset$mean_state_unemployment_rate

# calculate per-capita crime
cor(dataset$property_crime_occurances,dataset$violent_crime_occurances) # 0.9564125
dataset$violent_crime_percap <- dataset$violent_crime_occurances / dataset$population
dataset$property_crime_percap <- dataset$property_crime_occurances / dataset$population
cor(dataset$property_crime_percap,dataset$violent_crime_percap) # 0.561908
# note that this transformation makes these predictors much less correlated

# create new variable based on historical votes
hist_results <- as.data.frame.matrix(table(dataset$state_name,dataset$pop_vote_result))
hist_results[(hist_results$D!=0) & (hist_results$R!=0),]
hist_results$dem_ratio <- hist_results$D/(hist_results$D + hist_results$R)
library(tibble)
hist_results <- rownames_to_column(hist_results, "state_name")
# visualize new variable distribution
library(ggplot2)
ggplot(data=hist_results,aes(x=dem_ratio)) +
  geom_bar()
# add dem_ratio to dataset
dataset <- merge(dataset,hist_results[,c("state_name","dem_ratio")],id.vars="state_name")

# organize the columns to be used as predictors
predictors <- c("population_growth","percent_registered","gdp_percap","median_income_normalized","unemployment_rate_normalized","violent_crime_percap","property_crime_percap","governor_party","dem_ratio")
target <- "pop_vote_result"

data_predictors <- dataset[predictors]
data_target <- dataset[target]

# display summary information
summary(dataset[c(predictors,target)])

## visualizations

# pairs plot
pairs(dataset[c(predictors,target)])

# examine correlations between numeric predictors
library("dplyr")
#data_cor <- cor(select_if(data_predictors,is.numeric))
data_cor <- cor(dataset[,c('population_growth','percent_registered','gdp_percap','median_income_normalized','unemployment_rate_normalized','violent_crime_percap','property_crime_percap','dem_ratio')])
View(data_cor)
library(corrplot)
corrplot(data_cor, order='original', tl.cex = .85)

# examine relationships of numeric predictors with the target variable
library(ggplot2)
ggplot(data=dataset, aes(y=pop_vote_result,x=population_growth,fill=pop_vote_result)) +
  geom_violin() +
  scale_fill_manual(values=c("blue","red"))
ggplot(data=dataset, aes(y=pop_vote_result,x=percent_registered,fill=pop_vote_result)) +
  geom_violin() +
  scale_fill_manual(values=c("blue","red"))
ggplot(data=dataset, aes(y=pop_vote_result,x=gdp_percap,fill=pop_vote_result)) +
  geom_violin() +
  scale_fill_manual(values=c("blue","red"))
ggplot(data=dataset, aes(y=pop_vote_result,x=median_income_normalized,fill=pop_vote_result)) +
  geom_violin() +
  scale_fill_manual(values=c("blue","red"))
ggplot(data=dataset, aes(y=pop_vote_result,x=unemployment_rate_normalized,fill=pop_vote_result)) +
  geom_violin() +
  scale_fill_manual(values=c("blue","red"))
ggplot(data=dataset, aes(y=pop_vote_result,x=violent_crime_percap,fill=pop_vote_result)) +
  geom_violin() +
  scale_fill_manual(values=c("blue","red"))
ggplot(data=dataset, aes(y=pop_vote_result,x=property_crime_percap,fill=pop_vote_result)) +
  geom_violin() +
  scale_fill_manual(values=c("blue","red"))

# examine relationships of categorical predictors with the target variable
library(ggmosaic)
ggplot(data=dataset) +
  geom_mosaic(aes(x=governor_party,fill=pop_vote_result)) +
  scale_fill_manual(values=c("blue","red"))

# examine historical results by state
ggplot(data=dataset) +
  geom_bar(aes(y=state_name,fill=pop_vote_result)) +
  scale_fill_manual(values=c("blue","red"))


### Models

model_data <- dataset[,c("id",predictors,target)]
# partition the dataset into training (70%) and testing (30%)
library(caret)
set.seed(17)
trainIndex <- createDataPartition(model_data$pop_vote_result, p = 0.7, list = FALSE) # stratified sampling
train_data <- model_data[trainIndex, ]
test_data <- model_data[-trainIndex, ]

# define a benchmark model that assumes states will go to the same parties as 2020
# future models should outperform this model to be considered valuable
library(dplyr) 
results_2020 <- dataset[dataset$year==2020,c("state_name",target)]
pred_2020 <- merge(dataset[-trainIndex,c("id","state_name",predictors)], results_2020, id.vars = "state_name")[,c("state_name",target)]
confusionMatrix(pred_2020$pop_vote_result, test_data$pop_vote_result)

# define another benchmark model that assumes states will follow historical voting patterns
states <- list()
exp <- list()

for (i in seq(1,51,1)) {
  obs <- sort_by(dataset,dataset$id)[i,]
  st <- as.character(obs$state_name)
  states <- c(states,st)
  dr <- obs$dem_ratio
  if (dr > 0.5) e <- 'D'
  else if (dr < 0.5) e <- 'R'
  else e <- as.character(pred_2020[i,'pop_vote_result'])
  exp <- c(exp,e)
}
expected_votes <- data.frame(state_name=unlist(states),exp_vote=unlist(exp))
dataset <- merge(dataset,expected_votes,id.vars='state_name')
dataset$exp_vote <- as.factor(dataset$exp_vote)

confusionMatrix(dataset[-trainIndex,"exp_vote"], test_data$pop_vote_result)
# this gives us a benchmark accuracy of 92.31%

# Define the hyperparameter grid
param_grid <- expand.grid(cp = seq(0.01, 0.5, by = 0.01))

# Create a train control object for cross-validation
ctrl <- trainControl(method = "cv", number = 5)

# Perform hyperparameter tuning using caret's train function
set.seed(123456)  # For reproducibility
tree_tune1 <- train(pop_vote_result ~ population_growth + percent_registered + gdp_percap + median_income_normalized + unemployment_rate_normalized + violent_crime_percap + property_crime_percap + governor_party + dem_ratio, 
                    data = train_data,
                    method = "rpart", # rpart classification and regression trees
                    tuneGrid = param_grid,
                    trControl = ctrl)

# Print the best model and its parameters
print(tree_tune1)
#create confusion matrix
predictions1 <- predict(tree_tune1, newdata = test_data, type="raw")
confusionMatrix(predictions1, test_data$pop_vote_result)

library(pROC)
roc_obj1 <- roc(as.numeric(predictions1), as.numeric(test_data$pop_vote_result))
plot(roc_obj1, main = "ROC Curve", col = "blue", lwd = 2, print.auc=TRUE)

# try another model
library(rpart)
tree_tune2 <- rpart(pop_vote_result ~ population_growth + percent_registered + gdp_percap + median_income_normalized + unemployment_rate_normalized + violent_crime_percap + property_crime_percap + governor_party + dem_ratio, 
                    data = train_data)
# Load the library
library(rpart.plot)
# Plotting tree
rpart.plot(tree_tune2)

predictions2 <- predict(tree_tune2, newdata = test_data, type="class")
confusionMatrix(predictions2, test_data$pop_vote_result)

roc_obj2 <- roc(as.numeric(predictions2), as.numeric(test_data$pop_vote_result))
plot(roc_obj2, main = "ROC Curve", col = "blue", lwd = 2, print.auc=TRUE)

# tree model w/o dem ratio
tree_tune3 <- rpart(pop_vote_result ~ population_growth + percent_registered + gdp_percap + median_income_normalized + unemployment_rate_normalized + violent_crime_percap + property_crime_percap + governor_party, 
                    data = train_data)
rpart.plot(tree_tune3)
tree_pred3 <- predict(tree_tune3, newdata = test_data, type="class")
confusionMatrix(tree_pred3, test_data$pop_vote_result)

# random forest
library(randomForest)
modelrf <- randomForest(formula = pop_vote_result ~ population_growth + percent_registered + gdp_percap + median_income_normalized + unemployment_rate_normalized + violent_crime_percap + property_crime_percap + governor_party + dem_ratio,
                        data = train_data)
modelrf
plot(modelrf)
varImpPlot(modelrf)

predictions3 <- predict(modelrf, newdata=test_data)
confusionMatrix(predictions3, test_data$pop_vote_result)

# Define a hyperparameter grid for tuning
# in this case for RF .mtry: The number of predictors that will be randomly sampled at each split when creating tree models.
param_grid <- expand.grid(.mtry = seq(1,length(predictors)))   # mtry: Number of randomly sampled predictors

# Create a train control object for cross-validation
ctrl <- trainControl(method = "cv", number = 10)

set.seed(2003)
rf_tune <- train(pop_vote_result ~ population_growth + percent_registered + gdp_percap + median_income_normalized + unemployment_rate_normalized + violent_crime_percap + property_crime_percap + governor_party + dem_ratio, 
                 data = train_data,
                 method = "rf",
                 tuneGrid = param_grid,
                 ntree = 1000, # The ntree parameter is set by passing ntree to train directly
                 trControl = ctrl)

# Print the best model and its parameters
print(rf_tune)

# Calculate feature importance
importance <- varImp(rf_tune, scale = FALSE)

# Visualize feature importance using a bar plot
library(ggplot2)  # For plotting
ggplot(importance, aes(x = reorder(Overall, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Feature", y = "Importance Score") +
  ggtitle("Variable Importance Plot")

predictions4 <- predict(rf_tune, newdata = test_data)
conf_matrix <- confusionMatrix(predictions4, test_data$pop_vote_result)
conf_matrix # accuracy of 95.6%

# generate roc curve
library(pROC)
rftune_roc <- roc(as.numeric(predictions4), as.numeric(test_data$pop_vote_result))
plot(rftune_roc, main = "ROC Curve", col = "blue", lwd = 2, print.auc=TRUE)

# check for patterns in misclassified data
pred_df <- test_data
pred_df$pred <- predictions4
pred_df[pred_df$pop_vote_result != pred_df$pred,]
# there are no obvious patterns in the misclassified data

# random forest w/o dem_ratio
rf_tune2 <- train(pop_vote_result ~ population_growth + percent_registered + gdp_percap + median_income_normalized + unemployment_rate_normalized + violent_crime_percap + property_crime_percap + governor_party, 
                  data = train_data,
                  method = "rf",
                  tuneGrid = param_grid,
                  ntree = 1000, # The ntree parameter is set by passing ntree to train directly
                  trControl = ctrl)
rf_pred2 <- predict(rf_tune2, newdata = test_data)
confusionMatrix(rf_pred2, test_data$pop_vote_result)

# try knn cluster model
set.seed(7)
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
fit.knn <- train(pop_vote_result ~ population_growth + percent_registered + gdp_percap + median_income_normalized + unemployment_rate_normalized + violent_crime_percap + property_crime_percap + governor_party + dem_ratio, 
                 data=train_data, 
                 method="knn",
                 metric="Accuracy" ,
                 trControl=trainControl)
knn.k1 <- fit.knn$bestTune
print(fit.knn)
plot(fit.knn)
predictions5 <- predict(fit.knn, newdata = test_data)
cf <- confusionMatrix(predictions5, test_data$pop_vote_result)
print(cf)

# try knn with grid search
set.seed(7)
grid <- expand.grid(.k=seq(1,20,by=1))
fit.knn2 <- train(pop_vote_result ~ population_growth + percent_registered + gdp_percap + median_income_normalized + unemployment_rate_normalized + violent_crime_percap + property_crime_percap + governor_party + dem_ratio, 
                  data=train_data, 
                  method="knn", 
                  metric="Accuracy", 
                  tuneGrid=grid, 
                  trControl=trainControl)
knn.k2 <- fit.knn$bestTune # keep this optimal k for testing with stand alone knn() function in next section
print(fit.knn2)
plot(fit.knn2)
predictions6 <- predict(fit.knn2, newdata = test_data)
cf <- confusionMatrix(predictions6, test_data$pop_vote_result)
print(cf)

### Validation and Predictions

# load 2024 data
library(readxl)
data_2024 <- read_excel("C:/Users/lucky/Desktop/Predictive Modeling/Predictive Modeling Project/2024_data.xlsx", 
                        sheet = "data")

# create unique id for each observation
data_2024$id <- paste(data_2024$year,"-",data_2024$state_name, sep="")
# order data by id for consistency
data_2024 <- data_2024[order(data_2024$id),]
data_hist <- dataset[order(dataset$year,dataset$state_name),]

# Change all Character Variables to Factor Variables
data_2024[sapply(data_2024, is.character)] <- 
  lapply(data_2024[sapply(data_2024,is.character)], as.factor)

# apply transformations to the data

# calculate population growth variable
data_2024$population_growth <- (data_2024$population - data_2024$prev_population) / data_2024$prev_population

# calculate per-capita GDP
data_2024$gdp_percap <- data_2024$gdp / data_2024$population

# normalize median income
# divide by mean for each state
# using state_med_income calculated in training data EDA
data_2024$median_income_normalized <- data_2024$median_income / head(data_hist$mean_state_med_income,51)

## normalize unemployment rate
# divide by mean for each state
# using mean_state_unemployment_rate calculated in training data EDA
data_2024$unemployment_rate_normalized <- data_2024$unemployment_rate / head(data_hist$mean_state_unemployment_rate,51)

# calculate per-capita crime
data_2024$violent_crime_percap <- data_2024$violent_crime_occurances / data_2024$population
data_2024$property_crime_percap <- data_2024$property_crime_occurances / data_2024$population

# add in column of historical voting ratios
data_2024$dem_ratio <- head(data_hist$dem_ratio,51)

# organize the columns to be used as predictors
new_data <- data_2024[,predictors]

# make predictions using best model (tuned random forest)
predictions_2024 <- predict(rf_tune, newdata = new_data)
data_2024$pred_pop_vote_result <- predictions_2024
print(data_2024[c("state_name","pred_pop_vote_result")],n=51)

summary(data_2024$pred_pop_vote_result)

# calculate electoral allocation
electors <- read_excel("C:/Users/lucky/Desktop/Predictive Modeling/Predictive Modeling Project/Electors.xlsx", 
                       sheet = "data")
predicted_electors <- merge(data_2024[c("state_name","state_abr","pred_pop_vote_result")],electors[c("state_name","electoral_votes")],id.vars="state_name")
aggregate(predicted_electors$electoral_votes,by=list(predicted_electors$pred_pop_vote_result), FUN=sum)

# export predictions
library(writexl)
write_xlsx(predicted_electors, "C:/Users/lucky/Desktop/Predictive Modeling/Predictive Modeling Project/2024_predictions.xlsx")

# compare predictions to actual results
library(caret) 
results <- read_excel("C:/Users/lucky/Desktop/Predictive Modeling/Predictive Modeling Project/2024_results.xlsx")
results$pop_vote_result <- as.factor(results$pop_vote_result)
eval_cm <- confusionMatrix(predicted_electors$pred_pop_vote_result, results$pop_vote_result)
eval_cm
misclass <- merge(data_2024,
                  merge(predicted_electors,results,by=c('state_name','state_abr'))[predicted_electors$pred_pop_vote_result!=results$pop_vote_result,c('state_name','pred_pop_vote_result','pop_vote_result')],
                  on='state_name')[,c('state_name',predictors,'pred_pop_vote_result','pop_vote_result')]
misclass
#write_xlsx(misclass, "C:/Users/lucky/Desktop/Predictive Modeling/Predictive Modeling Project/2024_misclassifications.xlsx")

# compare to benchmark predictions
bench <- sort_by(dataset,dataset$id)[1:51,c('state_name','exp_vote')]
bench_cm <- confusionMatrix(bench$exp_vote,results$pop_vote_result)
bench_cm
bench_misclass <- merge(data_2024,
                        merge(bench,results,by='state_name')[bench$exp_vote!=results$pop_vote_result,c('state_name','exp_vote','pop_vote_result')],
                        on='state_name')[,c('state_name',predictors,'exp_vote','pop_vote_result')]
bench_misclass