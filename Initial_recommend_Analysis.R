#Importing the dataset
getwd()
data <- read.csv("sales.csv")[,-1] 
str(data)
View(data)
dim(data)

#Data preperation
# data$Issue.Date <- as.factor(ifelse(data$Issue.Date == 0, '2018', '2019'))
# data$Cls <- as.factor(ifelse(data$Cls == 1, 'N',
#                              ifelse(data$Cls == 2, 'O',
#                                     ifelse(data$Cls == 3, 'P',
#                                            ifelse(data$Cls == 4, 'Q',
#                                                   ifelse(data$Cls == 5, 'R',
#                                                          ifelse(data$Cls==6 , 's')))))))
# 
# data$Sector <- as.factor(ifelse(data$Sector == 1, 'BDP-KTM',
#                                 ifelse(data$Sector == 2, 'BIR-KTM',
#                                        ifelse(data$Sector == 3, 'BWA-KTM',
#                                               ifelse(data$Sector == 4, 'KTM-BDP',
#                                                      ifelse(data$Sector == 5, 'KTM-BIR', 'KTM-BWA'))))))
# data$Pax <- as.factor(ifelse(data$Pax == 0, 'ADULT','CHILD'))

#Changing the column names
# names(data)[names(data) == "registered"] <- "new"
# names(data)[names(data) == "cnt"] <- "total"


#Modeling
data <- data[c(-1,-2,-4,-7,-8,-9,-10,-12,-13,-14,-15,-16)]
data[] <- lapply(data, as.integer)
View(data)
library(caTools)

set.seed(123)
split = sample.split(data$Net, SplitRatio = 0.8)
train_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)

#writing new files for the test and train set
# write.csv(train_set, "data_train.csv", row.names = FALSE)
# write.csv(test_set, "data_test.csv", row.names = FALSE)

#Multilinear Regression
values_count <- sapply(lapply(data, unique), length)
values_count
multi = lm(formula = Net ~., data = train_set)

#predicting the test values
y_pred_m = predict(multi, newdata = test_set)

#Performance metrics
library(Metrics)
mae_m = mae(test_set[[4]], y_pred_m )
rmse_m = rmse(test_set[[4]], y_pred_m)
mae_m
rmse_m

#Random Forest
library(randomForest)
set.seed(123)
rf = randomForest(formula = Net ~., data = train_set,
                   ntree = 100)

#Predicting the test values
y_pred_rf = predict(rf, newdata = test_set)

#Performance metrics
mae_rf = mae(test_set[[4]], y_pred_rf)
rmse_rf = rmse(test_set[[4]], y_pred_rf)
mae_rf
rmse_rf

#Decision Tree
library(rpart)
dt = rpart(formula = Net ~., data = train_set,
           control = rpart.control(minsplit = 3))

#Predicting the test values
y_pred_dt  = predict(dt, newdata = test_set)

#Performance metrics
mae_dt = mae(test_set[[4]], y_pred_dt)
rmse_dt = rmse(test_set[[4]], y_pred_dt)
mae_dt
rmse_dt
plot(mae_dt)

#Saving the model
saveRDS(rf, file = "C:/Users/Asus-PC/Desktop/Project/rf.rda")

#Singel Prediction values
test_pred <- test_set
values = data.frame(Pax = 1,
                    Sector = 5,
                    Cls = 6,
                    Net = NA)
test_pred <- rbind(test_pred, values)
prediction <- predict(rf, newdata = test_pred[nrow(test_pred), -4])
prediction
