Our example solution uses multiple imputation and random forest. We start by loading the packages we need:
library(randomForest)
library(mice)
And then read in the training and test sets:
train = read.csv("train.csv")
test = read.csv("test.csv")
We removed the User ID from the training set:
train$UserID = NULL
And then combined the training and test sets so that we could do the imputation on YOB (the only variable with NA values). The new variable "type" keeps a memory of which observations belong in the training set, and which belong in the test set:
imputed1 = train
imputed1$type = "train"
imputed1$Happy = NULL
imputed2 = test
imputed2$type="test"
imputed2$UserID = NULL
imputed = rbind(imputed1, imputed2)
Now we use the mice package to do imputation on the data frame "imputed". Note that this takes a little while!
imputed = complete(mice(imputed))
And then we can split the data back into the training set and the test set, and remove the "type" variable:
trainImp = subset(imputed, type=="train")
trainImp$Happy = train$Happy
trainImp$type=NULL
testImp = subset(imputed, type=="test")
testImp$type=NULL
Now, we can create our random forest model, and make predictions on the test set:
RandomForestModel = randomForest(Happy ~ ., data=trainImp)
RfPredict = predict(RandomForestModel, newdata=testImp, type="prob")[,2]
And then we just need to prepare these predictions to make a submission:
submission = data.frame(UserID = test$UserID, Probability1 = RfPredict)
write.csv(submission, "submissionRf.csv", row.names=FALSE)
If you make this submission on Kaggle, you can see that the AUC on the private test set is 0.77459.
Again, please post your solution here if you would like to share it with the class.




JeffHebert
3 days ago
This was a great class, and the competition was very helpful. I learned a lot by trying to solve the Happiness problem. My best results came from a Ridge Regression. For those who don't know, Ridge Regression is like linear regression, except all the coefficients are restricted. The practical consequence is that the model usually ends up selecting just a few important parameters.
I started with simple cleaning method. I did not impute.
## Read and prepare data
train = read.csv("Kagle/train.csv")
test = read.csv("Kagle/test.csv")
train$YOB[train$YOB<=1900] <- NA     #Remove invalid dates
train$YOB[train$YOB>=2010] <- NA     #Remove invalid dates
test$YOB[test$YOB<=1900] <- NA     #Remove invalid dates
test$YOB[test$YOB>=2010] <- NA     #Remove invalid dates
train[,2][is.na(train[,2])] <- 1979                      #Set missing years to 1900
train$YOB=as.integer(train$YOB)
test[,2][is.na(test[,2])] <- 1979                        #Set missing years to 1900
test$YOB=as.integer(test$YOB)
test$votes = as.integer(test$votes)
I had to convert the train data to a data matrix to work with glmnet.
library(glmnet)
train_glmnet = data.matrix(train)

features = model.matrix(~.-Happy, data=train_clean)
features=features[,-1] # I had to remove the intercept

#Alpha=0 for lasso
glmModel = glmnet( x= features, y = as.factor(train$Happy), alpha=0, family="binomial")
I used cross validation to find the best lambda. This makes some interesting plots.
cv.glmmod <- cv.glmnet(x= features, y = as.factor(train$Happy), alpha=0, family="binomial")

plot(cv.glmmod)
best_lambda <- cv.glmmod$lambda.min
From here, I found that the best lambda was 0.0614347 for this ridge regression model. I found this corresponded with model 81, so I used this awkward method to get the mpredictions
predictglmModel = predict(glmModel, type="response", newx=features)[,81]