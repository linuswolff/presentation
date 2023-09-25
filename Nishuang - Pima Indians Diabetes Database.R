## -----------------------
## Read data
## -----------------------
library(readr)

Diabetes <- read_csv("Desktop/Intro to Data/presentation/diabetes.csv")
str(Diabetes)

## -----------------------
## Data preparation
## -----------------------

# Check original missing value
print(all(!is.na(Diabetes)))

# This dataset contains following theoretically impossible 0 value:
# 35  entries where Blood Pressure is 0
# 5   entries where Glucose        is 0
# 374 entries where Insulin        is 0
# 227 entries where SkinThickness  is 0
# 11  entries where BMI            is 0

# Treat 0 in the biological variables other than number of times pregnant as missing values 
cols_change <- colnames(Diabetes)[!colnames(Diabetes) %in% c("Pregnancies", "Outcome")]
bool_data <- Diabetes[cols_change] == 0
Diabetes[cols_change][bool_data] <- NA

# Show the number of missing values of each column
print(apply(bool_data, 2, sum))

# Replace missing values
# For Blood Pressure, Glucose, and BMI:
# replacing missing data with sensible values (mean or median) given the distribution of the data
Diabetes$BloodPressure[is.na(Diabetes$BloodPressure)] <- median(Diabetes$BloodPressure,na.rm = T)
Diabetes$Glucose[is.na(Diabetes$Glucose)] <- median(Diabetes$Glucose,na.rm = T)
Diabetes$BMI[is.na(Diabetes$BMI)] <- median(Diabetes$BMI,na.rm = T)

# For Insulin and SkinThickness:
# replacing missing data with prediction (Mutiple Imputaion)
install.packages("mice")
library(mice)
mice_mod <- mice(Diabetes[, c("Insulin","SkinThickness")], method='rf') 

# Save the complete imputation output 
mice_complete <- complete(mice_mod)

# Replace tsk_thickness and serum variables from the mice
Diabetes$SkinThickness <- mice_complete$SkinThickness
Diabetes$Insulin <- mice_complete$Insulin

# Make sure there is no missing data
sum(is.na(Diabetes))

## -----------------------
## Descriptive Data
## -----------------------

#Plotting the distribution of predictors' value
par(mfrow=c(2, 3))
hist(Diabetes$Pregnancies, breaks = 10, col = "coral2", main = "No. of Pregnancies", xlab = "Pregnancies")
hist(Diabetes$Glucose, breaks = 5, col = "gold1", main = "Glucose", xlab = "Glucose")
hist(Diabetes$BloodPressure, breaks = 5, col = "light green", main = "Blood Pressure", xlab = "Blood Pressure")
hist(Diabetes$SkinThickness, breaks = 10, col = "sky blue", main = "Skin Thickness", xlab = "Skin Thickness")
hist(Diabetes$Insulin, breaks = 10, col = "orange", main = "Insulin", xlab = "Insulin")
hist(Diabetes$BMI, breaks = 10, col = "purple", main = "BMI", xlab = "BMI")
hist(Diabetes$Age, breaks = 10, col = "pink", main = "Age", xlab = "Age")

#Plotting Correlation Matrix
cor(Diabetes[1:8])

library(ggcorrplot)
corr <- round(cor(Diabetes[1:8]), 1) 
ggcorrplot(corr,
           type = "lower",
           lab = TRUE, 
           lab_size = 3,  
           colors = c("red", "white", "cyan4"),
           title="Correlogram of Diabtes Dataset", 
           ggtheme = theme_bw)

## -----------------------
# Build Models
## -----------------------

# Split data into 80% training data and 20% testing data
set.seed(100)
sample_size = floor(0.8*nrow(Diabetes))

picked = sample(seq_len(nrow(Diabetes)),size = sample_size)
train.diabetes = Diabetes[picked,]
test.diabetes = Diabetes[-picked,]

y <- as.vector(train.diabetes$Outcome)
x <- model.matrix(Outcome~., train.diabetes)[,-1]

# Logistics regression
----------------------
logistics_regression <- glm(Outcome~., data = train.diabetes,family = "binomial")
summary(logistics_regression)


# Ridge regression
----------------------
install.packages("glmnet")
library(glmnet)

# choose the best lambda value with cross-validation method using cv.glmnet function
result.cv <- cv.glmnet(x, y, alpha = 0, family = "binomial")
plot(result.cv)

ridge_regression <- glmnet(x, y, alpha = 0, family = "binomial",
                lambda = result.cv$lambda.min, Intercept = TRUE)

ridge_regression$beta

# Predict outcome on Training dataset
ridge_predictTrain <- predict(ridge_regression, newx = as.matrix(train.diabetes[, c(-9)]), 
                                 s = "lambda.min", type = "class")

# Show confusion matrix
(ridge_predictTrain_result <- table(ridge_predictTrain, train.diabetes$Outcome))

# Overall error rate and accuracy
ridge_overall_accuracy <- (ridge_predictTrain_result[1] + ridge_predictTrain_result[4]) / sum(ridge_predictTrain_result) 
ridge_overall_error <- 1 - overall_accuracy


# Lasso regression
----------------------
lasso_regression <- glmnet(x, y, alpha = 1, family = "binomial",
                 lambda = result.cv$lambda.min, Intercept = TRUE)

lasso_regression$beta

# Predict outcome on Training dataset
lasso_predictTrain <- predict(lasso_regression, newx = as.matrix(train.diabetes[, c(-9)]), 
                              s = "lambda.min", type = "class")

# Show confusion matrix
(lasso_predictTrain_result <- table(lasso_predictTrain, train.diabetes$Outcome))

# Overall error rate and accuracy
lasso_overall_accuracy <- (lasso_predictTrain_result[1] + lasso_predictTrain_result[4]) / sum(lasso_predictTrain_result) 
lasso_overall_error <- 1 - overall_accuracy
