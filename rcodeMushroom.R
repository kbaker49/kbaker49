library(readr)
mushroom_data <- read_delim("mushroom/secondary_data.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
summary(mushroom_data)
#Histograms of Predictors
par(mfrow = c(3, 1))
#for numeric
hist(mushroom_data$`cap-diameter`, main='Cap Diameter Distribution')
hist(mushroom_data$`stem-height`, main='Stem Height Distribution')
hist(mushroom_data$`stem-width`, main='Stem Width Distribution')                
#For categorical
par(mfrow = c(3, 5))
barplot(table(mushroom_data$`cap-shape`),
        ylab = "Frequency",
        xlab = "Cap Shape")
barplot(table(mushroom_data$`cap-surface`),
        ylab = "Frequency",
        xlab = "Cap Surface")
barplot(table(mushroom_data$`cap-color`),
        ylab = "Frequency",
        xlab = "Cap Color")
barplot(table(mushroom_data$`does-bruise-or-bleed`),
        ylab = "Frequency",
        xlab = "Does Bruise or Bleed")
barplot(table(mushroom_data$`gill-attachment`),
        ylab = "Frequency",
        xlab = "Gill Attachment")
barplot(table(mushroom_data$`gill-spacing`),
        ylab = "Frequency",
        xlab = "Gill Spacing")
barplot(table(mushroom_data$`gill-color`),
        ylab = "Frequency",
        xlab = "Gill Color")
barplot(table(mushroom_data$`stem-root`),
        ylab = "Frequency",
        xlab = "Stem Root")
barplot(table(mushroom_data$`stem-surface`),
        ylab = "Frequency",
        xlab = "Stem Surface")
barplot(table(mushroom_data$`stem-color`),
        ylab = "Frequency",
        xlab = "Stem Color")
barplot(table(mushroom_data$`veil-type`),
        ylab = "Frequency",
        xlab = "Viel Type")
barplot(table(mushroom_data$`veil-color`),
        ylab = "Frequency",
        xlab = "Viel Color")
barplot(table(mushroom_data$`has-ring`),
        ylab = "Frequency",
        xlab = "Has Ring")
barplot(table(mushroom_data$`ring-type`),
        ylab = "Frequency",
        xlab = "Ring Type")
barplot(table(mushroom_data$`spore-print-color`),
        ylab = "Frequency",
        xlab = "Spore Print Color")
par(mfrow = c(1, 2))
barplot(table(mushroom_data$`habitat`),
        ylab = "Frequency",
        xlab = "Habitat")
barplot(table(mushroom_data$`season`),
        ylab = "Frequency",
        xlab = "Season")

#get data frame with only predictors
mushroom_Predictors <- subset(mushroom_data, select = -class)
mushroom_Predictors_Original <- mushroom_Predictors
mushroom_Class<-subset(mushroom_data, select = class)

#Deal with missing values
library(naniar)
(sum(is.na(mushroom_Predictors))/prod(dim(mushroom_Predictors)))*100
gg_miss_var(mushroom_Predictors, show_pct = TRUE)
vis_miss(mushroom_Predictors, warn_large_data = FALSE)

#Missing percent for those predictors over 50%
colMeans(is.na(mushroom_Predictors))

# get subset of colnames NA proportion is greater than threshold
threshold <- .5
names(mushroom_Predictors)[sapply(mushroom_Predictors, function(x) mean(is.na(x)) > threshold)]

#Remove Columns with > 50% missing data
mushroom_Predictors <- subset(mushroom_Predictors, select = -c(`veil-color`, `stem-root`, `stem-surface`,`veil-type`, `spore-print-color`))
dim(mushroom_Predictors)
dim(mushroom_Predictors_Original)
colMeans(is.na(mushroom_Predictors))
(sum(is.na(mushroom_Predictors))/prod(dim(mushroom_Predictors)))*100

####PERFORM IMPUTATION#########
##Mode Imputation because only categorical vars have missing values
colMeans(is.na(mushroom_Pred))
calc_mode <- function(x){
  # List the distinct / unique values
  distinct_values <- unique(x)
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}

#Replace NA avalues with mode
mushroom_Predictors["cap-surface"][is.na(mushroom_Predictors["cap-surface"])] <- calc_mode(na.omit(mushroom_Predictors$`cap-surface`))
mushroom_Predictors["gill-attachment"][is.na(mushroom_Predictors["gill-attachment"])] <- calc_mode(na.omit(mushroom_Predictors$`gill-attachment`))
mushroom_Predictors["ring-type"][is.na(mushroom_Predictors["ring-type"])] <- calc_mode(na.omit(mushroom_Predictors$`ring-type`))
mushroom_Predictors["gill-spacing"][is.na(mushroom_Predictors["gill-spacing"])] <- calc_mode(na.omit(mushroom_Predictors$`gill-spacing`))
colMeans(is.na(mushroom_Predictors))
dim(mushroom_Predictors)

##Check for NZV before converting
#Check Near Zero Variance for all categorical predictors
library(caret)
nzv <- nearZeroVar(mushroom_Predictors, saveMetrics= TRUE)
library(dplyr)
filter(nzv, zeroVar == TRUE | nzv == TRUE)
# Filter out predictors identified as near-zero variance
zero_var_cols <- which(nzv$nzv == TRUE | nzv$zeroVar == TRUE)
zero_var_cols
mushroom_Predictors_Processed <- mushroom_Predictors[, -zero_var_cols]
#Convert all Categorical Variables to dummy variables
library(fastDummies)
mushroom_Predictors_Processed <- dummy_cols(mushroom_Predictors_Processed,select_columns = "cap-shape")
mushroom_Predictors_Processed <- dummy_cols(mushroom_Predictors_Processed,select_columns = "cap-surface")
mushroom_Predictors_Processed <- dummy_cols(mushroom_Predictors_Processed,select_columns = "cap-color")
mushroom_Predictors_Processed <- dummy_cols(mushroom_Predictors_Processed,select_columns = "does-bruise-or-bleed")
mushroom_Predictors_Processed <- dummy_cols(mushroom_Predictors_Processed,select_columns = "gill-attachment")
mushroom_Predictors_Processed <- dummy_cols(mushroom_Predictors_Processed,select_columns = "gill-spacing")
mushroom_Predictors_Processed <- dummy_cols(mushroom_Predictors_Processed,select_columns = "gill-color")
mushroom_Predictors_Processed <- dummy_cols(mushroom_Predictors_Processed,select_columns = "stem-color")
mushroom_Predictors_Processed <- dummy_cols(mushroom_Predictors_Processed,select_columns = "has-ring")
mushroom_Predictors_Processed <- dummy_cols(mushroom_Predictors_Processed,select_columns = "habitat")
mushroom_Predictors_Processed <- dummy_cols(mushroom_Predictors_Processed,select_columns = "season")
colnames(mushroom_Predictors_Processed)

##make seperate data frames##
mushroom_Predictors_Processed<-subset(mushroom_Predictors_Processed, select = -c(`cap-shape`, `cap-surface`, `cap-color`, `does-bruise-or-bleed`, `gill-attachment`, `gill-spacing`, `gill-color`, `stem-color`, `has-ring`, `habitat`, `season`))
mushroom_Predictors_Numeric<-subset(mushroom_Predictors_Processed, select = c(`cap-diameter`, `stem-height`, `stem-width`))
mushroom_Predictors_Categorical <-subset(mushroom_Predictors_Processed, select=-c(`cap-diameter`, `stem-height`, `stem-width`))

###look at dimensions###
dim(mushroom_Predictors)
dim(mushroom_Predictors_Processed)
dim(mushroom_Predictors_Categorical)
dim(mushroom_Predictors_Numeric)

##CHECK IF NEED TO DELETE PREDICTORS FOR NUMERIC VARS ONLY
#check out correlation matrix
par(mfrow = c(1, 1))
library(corrplot)
corr_matrix <- cor(mushroom_Predictors_Numeric)
corrplot(corr_matrix)
cor <- cor(mushroom_Predictors_Numeric)
library(caret)
high <- findCorrelation(cor, .7)
high


#taking a look at the skewness values
library(e1071)
skewValues <- apply(mushroom_Predictors_Numeric, 2, skewness)
#filtering by those classified as extremely skewed
highSkew<-Filter(function(x) any(x < -1 | x > 1), skewValues)
length(highSkew)
highSkew

par(mfrow = c(3, 1))
#for numeric
hist(mushroom_Predictors_Numeric$`cap-diameter`, main='Cap Diameter Distribution')
hist(mushroom_Predictors_Numeric$`stem-height`, main='Stem Height Distribution')
hist(mushroom_Predictors_Numeric$`stem-width`, main='Stem Width Distribution') 

par(mfrow = c(1, 3))
boxplot(mushroom_Predictors_Numeric$`cap-diameter`, main='Cap Diameter Distribution')
boxplot(mushroom_Predictors_Numeric$`stem-height`, main='Stem Height Distribution')
boxplot(mushroom_Predictors_Numeric$`stem-width`, main='Stem Width Distribution')
#Perform transformations on numeric data, these have zero values so need to add
library(caret)
BCStemWidth <- BoxCoxTrans(mushroom_Predictors_Numeric$`stem-width`)
BCStemWidth
BCStemHeight <- BoxCoxTrans(mushroom_Predictors_Numeric$`stem-height`)
BCStemHeight
BCCapDiam <- BoxCoxTrans(mushroom_Predictors_Numeric$`cap-diameter`)
BCCapDiam

#Find samples with zero values in stem-height and stem-width
StemHeightZero <- mushroom_Predictors_Numeric[mushroom_Predictors_Numeric$`stem-height` == 0,]
StemHeightZero
StemWidthZero <- mushroom_Predictors_Numeric[mushroom_Predictors_Numeric$`stem-width` == 0,]
StemWidthZero

#Adjust values of all numeric observations
mushroom_Predictors_Numeric$`stem-height` <- mushroom_Predictors_Numeric$`stem-height` + .1
mushroom_Predictors_Numeric[mushroom_Predictors_Numeric$`stem-height` == .1,]
mushroom_Predictors_Numeric$`stem-width` <- mushroom_Predictors_Numeric$`stem-width` + .1
mushroom_Predictors_Numeric[mushroom_Predictors_Numeric$`stem-width` == .1,]
mushroom_Predictors_Numeric$`cap-diameter` <- mushroom_Predictors_Numeric$`cap-diameter` + .1


##HANDLE Outliers and skewness now that there are no zero values
#Visual Check as well
par(mfrow = c(3, 1))
#for numeric
hist(mushroom_Predictors_Numeric$`cap-diameter`, main='Cap Diameter Distribution')
hist(mushroom_Predictors_Numeric$`stem-height`, main='Stem Height Distribution')
hist(mushroom_Predictors_Numeric$`stem-width`, main='Stem Width Distribution') 
par(mfrow = c(1, 3))
boxplot(mushroom_Predictors_Numeric$`cap-diameter`, main='Cap Diameter Distribution')
boxplot(mushroom_Predictors_Numeric$`stem-height`, main='Stem Height Distribution')
boxplot(mushroom_Predictors_Numeric$`stem-width`, main='Stem Width Distribution')
###transformations
#use spatial sign for outliers, boxcox for skew, and center and scale
library(caret)
trans <- preProcess(as.data.frame(mushroom_Predictors_Numeric), method = c("spatialSign", "BoxCox", "center", "scale"))
mushroom_Predictors_Numeric <- predict(trans, as.data.frame(mushroom_Predictors_Numeric))
##Skewness check after 
skewness(mushroom_Predictors_Numeric$`stem-width`)
skewness(mushroom_Predictors_Numeric$`stem-height`)
skewness(mushroom_Predictors_Numeric$`cap-diameter`)
#Visual Check as well
par(mfrow = c(3, 1))
#for numeric
hist(mushroom_Predictors_Numeric$`cap-diameter`, main='Cap Diameter Distribution')
hist(mushroom_Predictors_Numeric$`stem-height`, main='Stem Height Distribution')
hist(mushroom_Predictors_Numeric$`stem-width`, main='Stem Width Distribution') 
par(mfrow = c(1, 3))
boxplot(mushroom_Predictors_Numeric$`cap-diameter`, main='Cap Diameter Distribution')
boxplot(mushroom_Predictors_Numeric$`stem-height`, main='Stem Height Distribution')
boxplot(mushroom_Predictors_Numeric$`stem-width`, main='Stem Width Distribution')

#add transformed numeric variables back into DF
mushroom_Predictors_Processed$`cap-diameter`<-mushroom_Predictors_Numeric$`cap-diameter`
mushroom_Predictors_Processed$`stem-height`<-mushroom_Predictors_Numeric$`stem-height`
mushroom_Predictors_Processed$`stem-width`<-mushroom_Predictors_Numeric$`stem-width`
#combine response with processed predictors
mushroom_Processed <- cbind(mushroom_Predictors_Processed, class = mushroom_data$class)
dim(mushroom_Processed)
#Spend the data

#determine response balance
class_counts <- table(mushroom_Processed$class)
par(mfrow = c(1, 1))
barplot(class_counts, 
        main = "Barplot of Binary Response Variable",
        xlab = "Categories",
        ylab = "Counts",
        col = c("blue", "red"), # Color for the bars
        legend = names(class_counts)) # Add a legend with category names
class_counts <- table(mushroom_Processed$class)
class_percentages <- prop.table(class_counts) * 100
class_percentages
# Set seed for reproducibility
set.seed(123)

# Determine number of rows for training set
train_rows <- round(0.7 * nrow(mushroom_Processed))

# Generate random indices for splitting the data
train_indices <- sample(seq_len(nrow(mushroom_Processed)), size = train_rows, replace = FALSE)

# Create training and testing sets
train_data <- mushroom_Processed[train_indices, ]
test_data <- mushroom_Processed[-train_indices, ]
x_test <- test_data[, -which(names(test_data) == "class")]
y_test <- test_data$class

### models
# Define the trainControl with 5-fold CV and required settings
ctrl <- trainControl(method = "cv",
                     number = 5,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     savePredictions = TRUE)

## Train the logistic regression model
mushroom_lr_model <- train(x = train_data[, -which(names(train_data) == "class")],  # Select predictors except the response variable,
                           y = train_data$class,  # Response variable
                           method = "glm",
                           metric = "ROC",
                           trControl = ctrl)
# View the trained model
mushroom_lr_model
#View training confusion matrix
confusionMatrix(data = mushroom_lr_model$pred$pred,
                reference = mushroom_lr_model$pred$obs)
# Predict on test data
mushroom_lr_prob <- predict(mushroom_lr_model,
                            newdata = x_test,
                            type = "prob")
mushroom_lr_pred <- predict(mushroom_lr_model,
                            newdata = x_test,
                            type = "raw")
class(mushroom_lr_pred)
class(test_data$class)
test_data$class <- factor(test_data$class)
# View test data confusion matrix
confusionMatrix(data = mushroom_lr_pred,
                reference = test_data$class,
                positive = "e")
# Get ROC curve from test data
mushroom_lr_roc <- roc(response = test_data$class,
                       predictor = mushroom_lr_prob$e,
                       levels = rev(levels(test_data$class)))
# Plot the ROC curve
par(mfrow = c(1, 1))
plot(mushroom_lr_roc, legacy.axes = TRUE)
# View AUC of ROC curve
auc(mushroom_lr_roc)

## Train the LDA model
mushroom_LDA_model <- train(x = train_data[, -which(names(train_data) == "class")],
                            y = train_data$class,  # Response variable
                            method = "lda",
                            metric = "ROC",
                            trControl = ctrl)
# View the trained model
mushroom_LDA_model

# View training confusion matrix
confusionMatrix(data = mushroom_LDA_model$pred$pred,
                reference = mushroom_LDA_model$pred$obs)
# Predict on test data
mushroom_lda_prob <- predict(mushroom_LDA_model,
                             newdata = x_test,
                             type = "prob")
mushroom_lda_pred <- predict(mushroom_LDA_model,
                             newdata = x_test,
                             type = "raw")
# View test data confusion matrix
confusionMatrix(data = mushroom_lda_pred,
                reference = test_data$class,
                positive = "e")
# Get ROC curve from test data
mushroom_lda_roc <- roc(response = test_data$class,
                        predictor = mushroom_lda_prob$e,
                        levels = rev(levels(test_data$class)))
# Plot the ROC curve
plot(mushroom_lda_roc, legacy.axes = TRUE)
# View AUC of ROC curve
auc(mushroom_lda_roc)

# Train PLSDA model
plsGrid <- expand.grid(.ncomp = c(1,5,10,15,20,50,75,84))
mushroom_plsda_model <- train(x = train_data[, -which(names(train_data) == "class")],
                              y = train_data$class,  # Response variable
                              method = "pls",
                              tuneGrid = plsGrid,
                              metric = "ROC",
                              trControl = ctrl)

mushroom_plsda_model
plot(mushroom_plsda_model)

confusionMatrix(data = mushroom_plsda_model$pred$pred,
                reference = mushroom_plsda_model$pred$obs)

mushroom_plsda_prob <- predict(mushroom_plsda_model,
                               newdata = x_test,
                               type = "prob")

mushroom_plsda_pred <- predict(mushroom_plsda_model,
                               newdata = x_test,
                               type = "raw")

confusionMatrix(data = mushroom_plsda_pred,
                reference = test_data$class,
                positive = "e")

mushroom_plsda_roc <- roc(response = test_data$class,
                          predictor = mushroom_plsda_prob$e,
                          levels = rev(levels(test_data$class)))
plot(mushroom_plsda_roc, legacy.axes = TRUE)
auc(mushroom_plsda_roc)

#Train GLMNet model
glmnGrid <- expand.grid(.alpha = c(0, .1, .2, .4, .6, .8, 1),
                        .lambda = seq(.01, .2, length = 10))
mushroom_glmnet_model <- train(x = train_data[, -which(names(train_data) == "class")],
                               y = train_data$class,  # Response variable
                               method = "glmnet",
                               tuneGrid = glmnGrid,
                               metric = "ROC",
                               trControl = ctrl)
mushroom_glmnet_model
plot(mushroom_glmnet_model)
confusionMatrix(data = mushroom_glmnet_model$pred$pred,
                reference = mushroom_glmnet_model$pred$obs) 

mushroom_glmnet_prob <- predict(mushroom_glmnet_model,
                                newdata = x_test,
                                type = "prob")

mushroom_glmnet_pred <- predict(mushroom_glmnet_model,
                                newdata = x_test,
                                type = "raw")

confusionMatrix(data = mushroom_glmnet_pred,
                reference = test_data$class,
                positive = "e")

mushroom_glmnet_roc <- roc(response = test_data$class,
                           predictor = mushroom_glmnet_prob$e,
                           levels = rev(levels(test_data$class)))
plot(mushroom_glmnet_roc, legacy.axes = TRUE)
auc(mushroom_glmnet_roc)

#Train Nearest Shrunken Centroid model
nscGrid <- data.frame(.threshold = seq(0,4, by=0.1))
mushroom_nsc_model <- train(x = train_data[, -which(names(train_data) == "class")],
                            y = train_data$class,  # Response variable
                            method = "pam",
                            tuneGrid = nscGrid,
                            metric = "ROC",
                            trControl = ctrl)

mushroom_nsc_model
plot(mushroom_nsc_model)
confusionMatrix(data = mushroom_nsc_model$pred$pred,
                reference = mushroom_nsc_model$pred$obs) 

mushroom_nsc_prob <- predict(mushroom_nsc_model,
                             newdata = x_test,
                             type = "prob")

mushroom_nsc_pred <- predict(mushroom_nsc_model,
                             newdata = x_test,
                             type = "raw")

confusionMatrix(data = mushroom_nsc_pred,
                reference = test_data$class,
                positive = "e")

mushroom_nsc_roc <- roc(response = test_data$class,
                        predictor = mushroom_nsc_prob$e,
                        levels = rev(levels(test_data$class)))
plot(mushroom_nsc_roc, legacy.axes = TRUE)
auc(mushroom_nsc_roc)

## Non-linear models
## Train Nonlinear Discriminant Analysis model
library(doParallel)
# Set up parallel processing
cores <- detectCores()
cl <- makeCluster(cores - 1) # Using one less core for other operations
registerDoParallel(cl)

# Your original code with parallel processing
fdaGrid <-  expand.grid(.degree = 1:2, .nprune = 2:10)
mushroom_fda_model <- train(x = train_data[, -which(names(train_data) == "class")],
                            y = train_data$class,
                            method = "fda",
                            tuneGrid = fdaGrid,
                            metric = "ROC",
                            trControl = ctrl)

# Stop and close the parallel processing cluster
stopCluster(cl)
mushroom_fda_model
plot(mushroom_fda_model)
confusionMatrix(data = mushroom_fda_model$pred$pred,
                reference = mushroom_fda_model$pred$obs) 

mushroom_fda_prob <- predict(mushroom_fda_model,
                             newdata = x_test,
                             type = "prob")

mushroom_fda_pred <- predict(mushroom_fda_model,
                             newdata = x_test,
                             type = "raw")

confusionMatrix(data = mushroom_fda_pred,
                reference = test_data$class,
                positive = "e")

mushroom_fda_roc <- roc(response = test_data$class,
                        predictor = mushroom_fda_prob$e,
                        levels = rev(levels(test_data$class)))
plot(mushroom_fda_roc, legacy.axes = TRUE)
auc(mushroom_fda_roc)

## Train Support Vector Machine model
library(kernlab)
sigmaRangeReduced <- sigest(as.matrix(train_data[, -which(names(train_data) == "class")]))
svmGrid <- expand.grid(.sigma = sigmaRangeReduced[1],
                       .C = 2^(seq(-4, 4)))
mushroom_svm_model <- train(x = train_data[, -which(names(train_data) == "class")],
                            y = train_data$class,  # Response variable
                            method = "svmRadial",
                            tuneGrid = svmGrid,
                            metric = "ROC",
                            trControl = ctrl)

mushroom_svm_model
plot(mushroom_svm_model)
confusionMatrix(data = mushroom_svm_model$pred$pred,
                reference = mushroom_svm_model$pred$obs) 

mushroom_svm_prob <- predict(mushroom_svm_model,
                             newdata = x_test,
                             type = "prob")

mushroom_svm_pred <- predict(mushroom_svm_model,
                             newdata = x_test,
                             type = "raw")
factor(mushroom_svm_pred)
confusionMatrix(data = mushroom_svm_pred,
                reference = test_data$class,
                positive = "e")

mushroom_svm_roc <- roc(response = test_data$class, 
               predictor = mushroom_svm_prob[, "p"])

# Plot ROC curve
plot(mushroom_svm_roc, legacy.axes = TRUE)
auc(mushroom_svm_roc)

## Train K-nearest Neighbors model
knnGrid <- data.frame(.k = 1:15)
mushroom_knn_model <- train(x = train_data[, -which(names(train_data) == "class")],
                            y = train_data$class,  # Response variable
                            method = "knn",
                            tuneGrid = knnGrid,
                            metric = "ROC",
                            trControl = ctrl)

mushroom_knn_model
plot(mushroom_knn_model)
confusionMatrix(data = mushroom_knn_model$pred$pred,
                reference = mushroom_knn_model$pred$obs) 

mushroom_knn_prob <- predict(mushroom_knn_model,
                             newdata = x_test,
                             type = "prob")

mushroom_knn_pred <- predict(mushroom_knn_model,
                             newdata = x_test,
                             type = "raw")

confusionMatrix(data = mushroom_knn_pred,
                reference = test_data$class,
                positive = "e")

mushroom_knn_roc <- roc(response = test_data$class,
                        predictor = mushroom_knn_prob$e,
                        levels = rev(levels(test_data$class)))
plot(mushroom_knn_roc, legacy.axes = TRUE)
auc(mushroom_knn_roc)


###NB#############
## Train Naive Bayes model
library(klaR)
library(naivebayes) 
nbGrid <- expand.grid(laplace = 0:5, usekernel = c(FALSE, TRUE), adjust = 1) # this is the equivalent of 'NB' tunegrid
mushroom_nb_model <- train(x = train_data[, -which(names(train_data) == "class")],
                           y = train_data$class,  # Response variable
                           method = "naive_bayes", # note not 'nb'
                           tuneGrid = nbGrid,
                           metric = "ROC",
                           trControl = ctrl)

mushroom_nb_model
plot(mushroom_nb_model)
confusionMatrix(data = mushroom_nb_model$pred$pred,
                reference = mushroom_nb_model$pred$obs) 

mushroom_nb_prob <- predict(mushroom_nb_model,
                            newdata = x_test,
                            type = "prob")

mushroom_nb_pred <- predict(mushroom_nb_model,
                            newdata = x_test,
                            type = "raw")

confusionMatrix(data = mushroom_nb_pred,
                reference = test_data$class,
                positive = "e")

mushroom_nb_roc <- roc(response = test_data$class,
                       predictor = mushroom_nb_prob$e,
                       levels = rev(levels(test_data$class)))
plot(mushroom_nb_roc, legacy.axes = TRUE)
auc(mushroom_nb_roc)

###NEWRAL NET######
nnetGrid <- expand.grid(.size = 1:10, .decay = c(0, .1, 1, 2))
maxSize <- max(nnetGrid$.size)
numWts <- (maxSize * (84 + 1) + (maxSize+1)*2) ## 84 is the number of predictors


mushroom_nnet_model <- train(x = train_data[, -which(names(train_data) == "class")],
                y = train_data$class,  # Response variable,
                 method = "nnet",
                 metric = "ROC",
                 tuneGrid = nnetGrid,
                 trace = FALSE,
                 maxit = 2000,
                 MaxNWts = numWts,
                 trControl = ctrl)
mushroom_nnet_model
plot(mushroom_nnet_model)
confusionMatrix(data = mushroom_nnet_model$pred$pred,
                reference = mushroom_nnet_model$pred$obs) 

mushroom_nnet_prob <- predict(mushroom_nnet_model,
                            newdata = x_test,
                            type = "prob")

mushroom_nnet_pred <- predict(mushroom_nnet_model,
                            newdata = x_test,
                            type = "raw")

confusionMatrix(data = mushroom_nnet_pred,
                reference = test_data$class,
                positive = "e")

mushroom_nnet_roc <- roc(response = test_data$class,
                       predictor = mushroom_nnet_prob$e,
                       levels = rev(levels(test_data$class)))
plot(mushroom_nnet_roc, legacy.axes = TRUE)
auc(mushroom_nnet_roc)


########BEST MODEL IS NNet....EXPLORE#############
dev.close()
varImp(mushroom_nnet_model)
plot(varImp(mushroom_nnet_model))
