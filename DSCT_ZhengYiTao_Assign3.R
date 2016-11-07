#REGRESSION AND MODEL EVALUATION ASSIGNMENT #3
#DONE BY: ZHENG YI TAO

library(cluster)
library(dplyr)
library(Rcpp)
library(Amelia)
library(data.table)
library(splitstackshape) #Package to convert Column into dichotomous variables
library(ggfortify)
library(scales)
library(MASS)
library(ggplot2)

#dataset <- read.csv("C:\\Users\\yitao22\\Dropbox\\DSCT\\Assignment 3\\german_credit_data.csv", header = TRUE, sep=";", stringsAsFactors = FALSE)
dataset <- read.csv("D:\\DSCT\\Assignment 3\\german_credit_data.csv", header = TRUE, sep=";", stringsAsFactors = FALSE)

#******************************************Question 1********************************************
#Check for each variable how many have missing records
sapply(dataset, function(x) sum(is.na(x)))
missmap(dataset, main = "Missing values vs observed")

#Check if any record are non-numeric
sapply(dataset,is.numeric)

# view a summary of the raw data
head(dataset)

# looking at summary statistics for the entire data set
summary(dataset)

#Standardise Columns name format
colnames(dataset) <- c("Account_Status", "Loan_Duration", "Credit_History", "Loan_Purpose",
                       "Credit_Amount_Asked", "Savings_Account", "Employment_Years",
                       "Installment_Rate", "Personal", "Debtors_Guarantors", "Present_Residence_Years",
                       "Assets", "Age", "Other_Instalment_plans", "Housing", "Num_of_Credits", 
                       "Job_Classification", "Num_of_Dependents", "Have_Telephone", "Is_Foreign_Worker",
                       "Credit_Offered")

#******************************************Question 2********************************************

#Convert categorical variables into factors
categoricalVar <- c(1, 3, 4, 6, 7, 9, 10, 12, 14, 15, 17, 19, 20)
dataset[,categoricalVar] <- lapply(dataset[,categoricalVar], factor)

#y-variable have to be 0 and 1, thus 1 is Accept, 0 is Reject
dataset$Credit_Offered[dataset$Credit_Offered == 2] <- 0

#Give names to categorical variables
levels(dataset$Savings_Account) <- c(50, 250, 750, 1000, "None")
levels(dataset$Employment_Years) <- c("Unemployed", 0.5, 2, 6, 8, NA)
levels(dataset$Debtors_Guarantors) <- c("None", "Co-applicant", "Guarantor")
levels(dataset$Other_Instalment_plans) <- c("Bank", "Retail stores", "None", NA)
levels(dataset$Housing) <- c("Rent", "Own", "For free")
levels(dataset$Job_Classification) <- c("Unemployed/Unskilled Non-resident",
                                        "Unskilled Resident",
                                        "Skilled Employee",
                                        "Management")
levels(dataset$Have_Telephone) <- c("None", "Yes")
levels(dataset$Is_Foreign_Worker) <- c("Yes", "No")

#Decreasing number of categories for Purpose of Loan
dataset$Loan_Purpose[dataset$Loan_Purpose %in% c(0, 1)] <- 1
dataset$Loan_Purpose[dataset$Loan_Purpose %in% c(2, 3, 4, 5)] <- 2
dataset$Loan_Purpose[dataset$Loan_Purpose %in% c(6, 8)] <- 3
dataset$Loan_Purpose[dataset$Loan_Purpose %in% 9] <- 4
dataset$Loan_Purpose <- factor(dataset$Loan_Purpose)
levels(dataset$Loan_Purpose) <- c("Car", "Household", "Education","Business","Others")
levels(dataset$Assets) <- c("Real_Estate", "No_Real_Estate/Life_Insurance",
                            "No_Real_Estate/Life_Insurance/Owns_Significant",
                            "No_Property")
levels(dataset$Account_Status) <- c("-1", "100", "200", "None")
levels(dataset$Credit_History) <- c("None", "Paid", "Paying_Duly", "Delay_before", "Critical")

#Create Gender from Personal
dataset$Gender[dataset$Personal==1] <- 1
dataset$Gender[dataset$Personal==2] <- 2
dataset$Gender[dataset$Personal==3] <- 1
dataset$Gender[dataset$Personal==4] <- 1
dataset$Gender[dataset$Personal==5] <- 2
dataset$Gender[dataset$Personal==7] <- NA
dataset$Gender <- factor(dataset$Gender) #Drop levels that are count = 0
levels(dataset$Gender) <- c("Male", "Female")
levels(dataset$Personal) <- c("Divorced/Separated", "Divorced/Separated/Married",
                              "Single", "Married/Widowed", NA)

#Create Dichotomous variables
dataset <- cSplit_e(data = dataset, split.col = "Personal", sep = "/", type = "character",
                    drop = TRUE, fill = 0)

dataset <- cSplit_e(data = dataset, split.col = "Assets", sep = "/", type = "character",
                    drop = TRUE, fill = 0)

#Removal of rows with NA
dataset <- dataset[!rowSums(is.na(dataset)) > 0,]

#******************************************Question 3********************************************

#Calculating distances between oberservations with Gower Distance
gower_dist <- daisy(dataset[,-1], metric = "gower", type = list(logratio = 3))
summary(gower_dist)
 
#Comparing the most similar rows
gower_mat <- as.matrix(gower_dist)
dataset[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

#Comparing the most different rows
dataset[which(gower_mat == max(gower_mat[gower_mat != min(gower_mat)]),
              arr.ind = TRUE)[1, ], ]

#Plot Cluster 
library(Rtsne)
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
pam_fit <- pam(gower_dist, diss = TRUE, k = 5)

#Observing the 5 medoids
dataset[pam_fit$medoids,]

#PAM algorithm
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


#******************************************Question 4********************************************

# create a separate model training and testing set (typically 80/20)
train <- dataset[1:801,]
test <- dataset[802:1002,]

# create a logistic regression model with the training data
model <- glm(Credit_Offered ~.,family=binomial(link='logit'),data=train)

#Looking at the most significant variables
summary(model)
sig.var <- summary(model)$coeff[-1,4] < 0.01
names(sig.var)[sig.var == T]

# reviewing the model using chisq test across each predictor in sequence
# to determine the relative importance of each predictor
anova(model, test="Chisq")

#******************************************Question 5********************************************

# using function stepAIC to determine the best combination of predictors
step <- stepAIC(model, direction="both")
step$anova # display results
summary(step)

#Newly adjusted model
modelRefined <- glm(Credit_Offered ~ Loan_Duration + Account_Status + Credit_History-1
                    , family=binomial(link='logit'),data=train)

summary(modelRefined)

anova(modelRefined, test="Chisq")

#Check for multicollinearity
library(car)
vif(modelRefined)

#******************************************Question 6********************************************

table(dataset$Credit_Offered, dataset$Loan_Duration)
table(dataset$Credit_Offered, dataset$Credit_History)
table(dataset$Credit_Offered, dataset$Account_Status)

table(train$Credit_Offered)
table(train$Credit_History)
table(train$Loan_Duration)
table(train$Account_Status)

# odds ratios and 95% CI
exp(cbind("Odds Ratio" = coef(modelRefined), confint(modelRefined)))

#******************************************Question 7********************************************

# use the model to predict Credit Offer in the test set
pred_results <- predict(modelRefined,newdata=test[-19],type='response')

# the results of the model is a probability value and must be mapped to 1 or 0
fitted_results <- ifelse(pred_results > 0.5, 1, 0)

#Build a confusion matrix
library(caret)
library(e1071)
confMat <- confusionMatrix(data = fitted_results, reference = test$Credit_Offered, positive = "1")

#ggplot of Confusion Matrix
Actual <- factor(c(0, 0, 1, 1))
Predicted <- factor(c(0, 1, 0, 1))
Y      <- c(25, 36, 10, 130)
df <- data.frame(Actual, Predicted, Y)

ggplot(data =  df, mapping = aes(x = Actual, y = Predicted)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "red", high = "green") +
  theme_bw() + theme(legend.position = "none") +
  ggtitle("Confusion Matrix")

#Print the Accuracy and Sensitivity
confMat$overall["Accuracy"]
confMat$byClass["Sensitivity"]

#******************************************Question 8********************************************

#plot ROC
library(ROCR)
pred <- prediction(pred_results, test$Credit_Offered)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main="ROC", colorize = T)

#AUC Calculations
AUC <- performance(pred, measure = "auc")
AUC <- AUC@y.values[[1]]
AUC
