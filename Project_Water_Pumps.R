#Course: HarvardX: PH125.9x Data Science
#Author: Khushnud Sapaev
#Purpose: Classification models to predict waterpump status

#Loading libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(class)) install.packages("class", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")
if(!require(naivebayes)) install.packages("naivebayes", repos = "http://cran.us.r-project.org")
if(!require(kernlab)) install.packages("kernlab", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
################################## DATASETS ##########################################
#Loading datasets
traindata = read.csv('https://s3.amazonaws.com/drivendata/data/7/public/4910797b-ee55-40a7-8668-10efd5c1b960.csv')
traindata_labels = read.csv('https://s3.amazonaws.com/drivendata/data/7/public/0bf8bc6e-30d0-4c50-956a-603fc693d966.csv')
#Combining train dataset and labels
traindata = left_join(traindata, traindata_labels, by = "id")
traindata <- traindata %>% select(status_group, everything())
rm(traindata_labels)

#Summary of datasets
str(traindata)
summary(traindata)


################### DATA CLEANSING ################################################
#Checking duplicate or grouped columns
traindata %>% group_by(quality_group, water_quality) %>% summarise(total = n())
traindata %>% group_by(waterpoint_type_group, waterpoint_type) %>% summarise(total = n())
traindata %>% group_by(extraction_type_class, extraction_type_group, extraction_type) %>% tally()
traindata %>% group_by(quantity_group, quantity) %>% tally()
traindata %>% group_by(source_class, source_type, source) %>% tally()
traindata %>% group_by(payment, payment_type) %>% tally()
traindata %>% group_by(management, management_group) %>% tally()
traindata %>% group_by(scheme_management) %>% tally()
traindata %>% group_by(permit) %>% tally()
traindata %>% group_by(public_meeting) %>% tally()
traindata %>% group_by(region, region_code, district_code) %>% tally()
traindata %>% group_by(num_private) %>% tally()

################
#Removing duplicate or nonrelevant columns
drops <- c("id", "amount_tsh", "funder", "wpt_name", "num_private", "subvillage", 
           "region_code", "district_code", "ward", "public_meeting", "recorded_by", "scheme_name",
           "permit","extraction_type" ,"extraction_type_class", "management", "payment", "quality_group",
           "quantity_group", "source_type", "source_class", "waterpoint_type_group")
traindata = traindata[ , !(names(traindata) %in% drops)]
rm(drops)

################
#Renaming blank factor level of scheme management as "None"
levels(traindata$scheme_management)[1] = "None"

################
#Checked installers, most frequent installer has three letters, government occurs in different formats
head(sort(table(traindata$installer), decreasing = T), 25)
# Make installer lowercase, take first 3 letters as a sub string
traindata$installer <- substr(tolower(traindata$installer),1,3)
traindata$installer[traindata$installer %in% c(" ", "", "0", "_", "-")] <- "other"
# Take the top 25 substrings from above by occurance frequency
install_top_25 <- names(summary(as.factor(traindata$installer)))[1:25]
traindata$installer[!(traindata$installer %in% install_top_25)] <- "other"
traindata$installer <- as.factor(traindata$installer)

rm(install_top_25)


################
#Replacing zero construction year with medians of years according to installer
median.years <- tapply(traindata$construction_year[traindata$construction_year>0], traindata$installer[traindata$construction_year>0], median)
for (i in 1:length(levels(traindata$installer))){
  traindata$construction_year[traindata$installer==levels(traindata$installer)[i]] = ifelse(traindata$construction_year[traindata$installer==levels(traindata$installer)[i]]==0, median.years[i],traindata$construction_year[traindata$installer==levels(traindata$installer)[i]])
  }
rm(median.years)
rm(i)

################
#Creating three categories from lga column
traindata = traindata %>% mutate(lga = ifelse(grepl("Rural", lga), "rural",
                                       ifelse(grepl(" Urban", lga), "urban","other")))
traindata$lga = as.factor(traindata$lga)

################
#Converting date recorded variable as date
traindata$date_recorded = lubridate::ymd(traindata$date_recorded)

#Creating an age variable by subtracting construction year from the max year value recorded date
traindata$pump_age = max(lubridate::year(traindata$date_recorded)) - traindata$construction_year
traindata$construction_year <- NULL
#Creating a month variable from date recorded
traindata$month = lubridate::month(traindata$date_recorded, label = TRUE, abbr = FALSE)

#Dropping date recorded
traindata$date_recorded = NULL

#Tanzania has five seasons
#Creating Seasons from month
traindata$season = ifelse(traindata$month %in% c("December", "January", "February"), "hot dry",
                          ifelse(traindata$month == "March", "intermittent rain",
                                  ifelse(traindata$month %in% c("April", "May"), "heavy rain",
                                         ifelse(traindata$month == "November", "short rain",
                                                "cooler dry"))))
traindata$season = as.factor(traindata$season)

#Dropping month variable
traindata$month <- NULL



################################ VISUALIZATION #######################################

# Creating scatter plot: latitude vs longitude with color as status_group on the map of pumps in Tanzania
ggplot() +geom_polygon(data=map_data("world", region = "Tanzania"), aes(x=long, y=lat, group=group), colour="black", fill="white") +
  geom_point(data=subset(traindata, latitude < 0 & longitude > 0), 
             aes(x=longitude, y=latitude, color = status_group), size = 1)  +
  geom_point(shape = 1, alpha=0.5) + theme(legend.position = "bottom") +
  ggtitle("Map of Tanzania with pump locations")

#Creating scatter plot: quantity vs waterpoint type with color as status_group
ggplot(traindata, aes(quantity, waterpoint_type, col = status_group)) + geom_point() +
  geom_jitter(alpha=0.5) + theme(legend.position = "bottom") +
  ggtitle("Status of pumps in Tanzania by water quantity and waterpoint type")

#Creating scatter plot: newly generated variables pump age vs record season with color as status_group
ggplot(traindata, aes(pump_age, season, col = status_group)) + geom_point() +
  geom_jitter(alpha=0.4) + theme(legend.position = "bottom") + scale_x_continuous(breaks = c(seq(0,max(traindata$pump_age),5))) +
  ggtitle("Distribution of pumps in Tanzania by age and season")


#Table formats of pump description by status
table(traindata$season, traindata$status_group)
table(traindata$lga, traindata$status_group)
table(traindata$pump_age, traindata$status_group)
table(traindata$waterpoint_type, traindata$status_group)
table(traindata$water_quality, traindata$status_group)
table(traindata$extraction_type_group, traindata$status_group)


################### DATA SPLITTING ################################################

set.seed(99)
split = createDataPartition(traindata$status_group, p=0.7, list=FALSE)
training_set = traindata[split, ] #67% data for training 
test_set = traindata[-split, ] #33% testing
rm(split)


################### DATA CLASSIFICATION ################################################

#################
#3-fold CV model selection
trControl = trainControl(method = "cv", number = 3, savePredictions = TRUE, search="grid")
#Grid parameters for XGBoost
parametersGrid = expand.grid(eta = 0.1, colsample_bytree = c(0.6, 0.7, 0.8), 
                             max_depth = c(5,10), nrounds = 100, gamma = 1, min_child_weight = 2,
                             subsample=c(0.5, 0.75, 1))

#################
#Classification Models
svmTrain = train(status_group ~ ., data=training_set,method="svmLinear", trControl=trControl)
svmTrain$bestTune
gbmTrain = train(status_group ~ ., data=training_set,method="gbm", trControl=trControl, verbose=FALSE)
gbmTrain$bestTune
knnTrain = train(status_group ~ ., data=training_set,method="knn", preProcess = c("center","scale"), trControl=trControl)
knnTrain$bestTune
ksvmTrain = train(status_group ~ ., data=training_set,method="svmRadial", trControl=trControl)
ksvmTrain$bestTune
rfTrain = train(status_group ~ ., data=training_set,method="rf", trControl=trControl, importance = TRUE, verbose=FALSE)
rfTrain$bestTune
xgboostTrain = train(status_group ~ ., data=training_set,method="xgbTree", trControl=trControl, tuneGrid=parametersGrid)
xgboostTrain$bestTune


#Compare training models using resamples
results =resamples(list(StochasticGB=gbmTrain,kNN=knnTrain,SVM=svmTrain,
                        KernelSVM=ksvmTrain,RandomForest=rfTrain,XGBoost=xgboostTrain))
                    
summary(results)
bwplot(results)

#Prediction and accuracy
#SVM
y_pred_svm <- predict(svmTrain, test_set)
varImp(svmTrain)
#Confusion matrix
cm = confusionMatrix(y_pred_svm, test_set$status_group)
cm$byClass[,5:7]

#StochasticGB
y_pred_gbm <- predict(gbmTrain, test_set)
#Confusion matrix
cm = confusionMatrix(y_pred_gbm, test_set$status_group)
cm$byClass[,5:7]

#KNN
y_pred_knn <- predict(knnTrain, test_set)
varImp(knnTrain)
#Confusion matrix
cm = confusionMatrix(y_pred_knn, test_set$status_group)
cm$byClass[,5:7]

#KSVM
y_pred_ksvm <- predict(ksvmTrain, test_set)
varImp(ksvmTrain)
#Confusion matrix
cm = confusionMatrix(y_pred_ksvm, test_set$status_group)
cm$byClass[,5:7]

#RF
y_pred_rf <- predict(rfTrain, test_set)
varImp(rfTrain)
#Confusion matrix
cm = confusionMatrix(y_pred_rf, test_set$status_group)
cm$byClass[,5:7]

#XGBoost
xgboostTrain$finalModel
y_pred_xgb = predict(xgboostTrain, test_set)
varImp(xgboostTrain)
#Confusion Matrix
cm = confusionMatrix(y_pred_xgb, test_set$status_group)
cm$byClass[,5:7]

