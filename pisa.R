## clean data and change the types of data
Pisa2009 <- Pisa2009[,-c(1)]
Pisa2009 <- Pisa2009[complete.cases(Pisa2009),]
Pisa2009 <- Pisa2009[, c(24, 1:23)] #move dependent variable to the first column
Pisa2009$raceeth <- as.factor(Pisa2009$raceeth)
Pisa2009$grade <- as.factor(Pisa2009$grade)
Pisa2009[5:24] <- lapply(Pisa2009[5:24], as.numeric)
Pisa2009[3] <- lapply(Pisa2009[3], as.numeric)

## create dummies for 'raceeth' and 'grade' and conduct feature selection
library(fastDummies)
Pisa2009 <- dummy_cols(Pisa2009, select_columns = c('raceeth', 'grade'), remove_first_dummy = FALSE)
Pisa2009[25:36] <- lapply(Pisa2009[25:36], as.numeric) #change the type of the newly created dummy variables
Pisa2009 <- Pisa2009[,-c(2,4)] #delete the original 'raceeth' and 'grade' column to avoid singularities
Pisa2009 <- Pisa2009[,-c(29,34)] #remove raceeth_white and grade_12 because they are the base levels (intercept)

## do univariate and bivariate analysis on data
# see all the scatter plot of the data set - can't see, there are too many data
# then check individual scatter plot for second order term and interaction term
plot(Pisa2009$grade, Pisa2009$readingScore)
plot(Pisa2009$male, Pisa2009$readingScore)
plot(Pisa2009$preschool, Pisa2009$readingScore)
plot(Pisa2009$expectBachelors, Pisa2009$readingScore)
plot(Pisa2009$motherHS, Pisa2009$readingScore)
plot(Pisa2009$motherBachelors, Pisa2009$readingScore)
plot(Pisa2009$motherWork, Pisa2009$readingScore)
plot(Pisa2009$fatherHS, Pisa2009$readingScore)
plot(Pisa2009$fatherBachelors, Pisa2009$readingScore)
plot(Pisa2009$fatherWork, Pisa2009$readingScore)
plot(Pisa2009$selfBornUS, Pisa2009$readingScore)
plot(Pisa2009$motherBornUS, Pisa2009$readingScore)
plot(Pisa2009$fatherBornUS, Pisa2009$readingScore)
plot(Pisa2009$englishAtHome, Pisa2009$readingScore)
plot(Pisa2009$computerForSchoolwork, Pisa2009$readingScore)
plot(Pisa2009$read30MinsADay, Pisa2009$readingScore)
plot(Pisa2009$minutesPerWeekEnglish, Pisa2009$readingScore)
plot(Pisa2009$studentsInEnglish, Pisa2009$readingScore)
plot(Pisa2009$schoolHasLibrary, Pisa2009$readingScore)
plot(Pisa2009$publicSchool, Pisa2009$readingScore)
plot(Pisa2009$urban, Pisa2009$readingScore)
plot(Pisa2009$schoolSize, Pisa2009$readingScore)
# did not find anything special

# see correlation of the data set
cor(Pisa2009) # expectBachelors, fatherBachelors and read30MinsADay have the strongest correlation with the dependent variable
hist(Pisa2009$studentsInEnglish, breaks = 30) # check the histograms of continuous variables (schoolSize, studentInEnglish and minutesPerWeekEnglish)
hist(Pisa2009$minutesPerWeekEnglish, breaks = 30)
hist(Pisa2009$schoolSize, breaks = 30)
# Some outliers exist. Nothing special. No transformation.

#______________________________________________________________

### first model: no interaction terms, no transformation, use lasso to do feature selection
## use LASSO to do feature selection
library(data.table)
Pisa2009 <- copy(Pisa2009)
library(glmnet)
x <- as.matrix(Pisa2009[,2:32])
y <- as.double(Pisa2009[,1])
set.seed(123)
lasso <- cv.glmnet(x, y, family="gaussian", alpha=1)
lasso$lambda.min
plot(lasso)
coef.glmnet(lasso, s = lasso$lambda.min)
# feature selected
Pisa20091 <- subset(Pisa2009, select = -c(preschool, motherWork, selfBornUS, motherBornUS, fatherBornUS, studentsInEnglish, schoolHasLibrary, urban))

## use selected features to build a linear regression model
m1 <- lm(readingScore ~ ., data = Pisa20091)
summary(m1)
# eliminate the insignificant terms one by one
Pisa20091 <- subset(Pisa20091, select = -c(motherHS))
m1 <- lm(readingScore ~ ., data = Pisa20091)
summary(m1)

Pisa20091 <- subset(Pisa20091, select = -c(fatherWork))
m1 <- lm(readingScore ~ ., data = Pisa20091)
summary(m1)

Pisa20091 <- subset(Pisa20091, select = -c(minutesPerWeekEnglish))
m1 <- lm(readingScore ~ ., data = Pisa20091)
summary(m1)

Pisa20091 <- subset(Pisa20091, select = -c(motherBornUS))
m1 <- lm(readingScore ~ ., data = Pisa20091)
summary(m1)

Pisa20091 <- subset(Pisa20091, select = -c(englishAtHome))
m1 <- lm(readingScore ~ ., data = Pisa20091)
summary(m1)

## check multicollinearity
library(car)
vif(m1)
# multicollinearity happens among different dummy variables of 'grade' - no need to worry about

## check residuals 
mean(m1$residuals)
plot(m1)
durbinWatsonTest(m1)

## do cross validation
rmse1 = sqrt(mean((Pisa20091$readingScore - fitted.values(m1))^2))
#rmse1 = 73.8
library(DAAG)
cv.lm(data = Pisa20091, form.lm = formula(readingScore ~., plotit = "Observed", m=5))
#ms = 346741
# adj R^2 = 0.31

## first model: no interaction terms, no transformation, use lasso to do feature selection
summary(m1)
# there seems to be a little bit multiplicative, will try to use log transformation on dependent variable
# will try to add more interaction terms

#______________________________________________________________

### second try: add some interaction terms and then do lasso

## duplicate data frame
library(data.table)
Pisa20092 <- copy(Pisa2009)

## create interaction terms based on knowledge and the first try
Pisa20092$read30MinsADay_schoolHasLibrary <- Pisa20092$read30MinsADay * Pisa20092$schoolHasLibrary
Pisa20092$read30MinsADay_minutesPerweekEnglish <- Pisa20092$read30MinsADay * Pisa20092$minutesPerWeekEnglish
Pisa20092$read30MinsADay_publicSchool <- Pisa20092$read30MinsADay * Pisa20092$publicSchool
Pisa20092$schoolHasLibrary_publicSchool <- Pisa20092$schoolHasLibrary * Pisa20092$publicSchool
Pisa20092$computerForSchoolwork_publicSchool <- Pisa20092$computerForSchoolwork * Pisa20092$publicSchool
Pisa20092$expectBachelors_publicSchool <- Pisa20092$expectBachelors * Pisa20092$publicSchool
Pisa20092$fatherBachelors_motherBachelors <- Pisa20092$fatherBachelors * Pisa20092$motherBachelors
Pisa20092$motherWork_motherBachelors <- Pisa20092$motherWork * Pisa20092$motherBachelors

## do lasso to select features
library(glmnet)
x2 <- as.matrix(Pisa20092[,2:40])
y2 <- as.double(Pisa20092[,1])
set.seed(123)
lasso2 <- cv.glmnet(x2, y2, family="gaussian", alpha=1)
lasso2$lambda.min
plot(lasso2)
coef.glmnet(lasso2, s = lasso2$lambda.min)

#feature selected 
Pisa20092 <- subset(Pisa20092, select = -c(selfBornUS, urban, read30MinsADay_publicSchool, schoolHasLibrary_publicSchool, computerForSchoolwork_publicSchool, expectBachelors_publicSchool))

#try to rebuild a model with interaction terms
m2 <- lm(readingScore ~ ., data = Pisa20092)
summary(m2)
#eliminate the insignificant variable one by one
Pisa20092 <- subset(Pisa20092, select = -c(computerForSchoolwork_publicSchool))
m2 <- lm(readingScore ~ ., data = Pisa20092)
summary(m2)

Pisa20092 <- subset(Pisa20092, select = -c(fatherBornUS))
m2 <- lm(readingScore ~ ., data = Pisa20092)
summary(m2)

Pisa20092 <- subset(Pisa20092, select = -c(fatherWork))
m2 <- lm(readingScore ~ ., data = Pisa20092)
summary(m2)

Pisa20092 <- subset(Pisa20092, select = -c(preschool))
m2 <- lm(readingScore ~ ., data = Pisa20092)
summary(m2)

Pisa20092 <- subset(Pisa20092, select = -c(motherHS))
m2 <- lm(readingScore ~ ., data = Pisa20092)
summary(m2)

Pisa20092 <- subset(Pisa20092, select = -c(studentsInEnglish))
m2 <- lm(readingScore ~ ., data = Pisa20092)
summary(m2)

Pisa20092 <- subset(Pisa20092, select = -c(motherBornUS))
m2 <- lm(readingScore ~ ., data = Pisa20092)
summary(m2)

Pisa20092 <- subset(Pisa20092, select = -c(englishAtHome))
m2 <- lm(readingScore ~ ., data = Pisa20092)
summary(m2)

## check multicollinearity
vif(m2)
# multicolinearity among different dummy variables of grade. read20MinsADay and read30MinsADay_schoolHasLibrary

## check residuals 
mean(m2$residuals)
plot(m2)
durbinWatsonTest(m2)

## do cross validation
rmse2 = sqrt(mean((Pisa20092$readingScore - fitted.values(m2))^2))
#rmse2 = 73.5
cv.lm(data = Pisa20092, form.lm = formula(readingScore ~., plotit = "Observed", m=5))
#ms = 335944
# adj R^2 = 0.314

## second model: have interaction terms, no transformation, use lasso to do feature selection
summary(m2)
#no great improvement compared to model 1

## eliminate outliers
# does not help much

#______________________________________________________________

### third try: use log transformation for the dependent variable
library(data.table)
Pisa20093 <- copy(Pisa2009)

## do log transformation 
Pisa20093$readingScore_log <- log(Pisa20093$readingScore)
Pisa20093 <- Pisa20093[,-c(1)]
Pisa20093 <- Pisa20093[,c(32, 1:31)]

## use lasso to select features
x3 <- as.matrix(Pisa20093[,2:32])
y3 <- as.double(Pisa20093[,1])
set.seed(123)
lasso3 <- cv.glmnet(x3, y3, family="gaussian", alpha=1)
lasso3$lambda.min
plot(lasso3)
coef.glmnet(lasso3, s = lasso3$lambda.min)
# feature selected 

Pisa20093 <- subset(Pisa20093, select = -c(preschool, motherWork, selfBornUS, motherBornUS, fatherBornUS, studentsInEnglish, schoolHasLibrary, urban))

## use selected features to build a linear regression model
m3 <- lm(readingScore_log ~ ., data = Pisa20093)
summary(m3)

Pisa20093 <- subset(Pisa20093, select = -c(motherHS))
m3 <- lm(readingScore_log ~ ., data = Pisa20093)
summary(m3)

Pisa20093 <- subset(Pisa20093, select = -c(fatherWork))
m3 <- lm(readingScore_log ~ ., data = Pisa20093)
summary(m3)

Pisa20093 <- subset(Pisa20093, select = -c(minutesPerWeekEnglish))
m3 <- lm(readingScore_log ~ ., data = Pisa20093)
summary(m3)

Pisa20093 <- subset(Pisa20093, select = -c(englishAtHome))
m3 <- lm(readingScore_log ~ ., data = Pisa20093)
summary(m3)

## check multicollinearity
vif(m3)
# multicolinearity happens among dummy variables of grade

## check residuals 
mean(m3$residuals)
plot(m3) # residuals are not so normal
durbinWatsonTest(m3)

## do cross validation
act_trans = exp(Pisa20093$readingScore_log)
fit_trans = exp(fitted.values(m3))
rmse3 = sqrt(mean((act_trans - fit_trans)^2))
#rmse3 = 74
# cannot do cross validation directly because the dependent variable is transformed. It is not in the same scale as m1 and m2.
# adj R^2 = 0.314
# no great improvement compared to model 1

#______________________________________________________________

### fourth try: use log transformation for the independent variable and the dependent variable
library(data.table)
Pisa20094 <- copy(Pisa2009)

## do log transformation 
Pisa20094$readingScore_log <- log(Pisa20094$readingScore)
Pisa20094$minutesPerWeekEnglish_log <- log(Pisa20094$minutesPerWeekEnglish + 1)
Pisa20094$schoolSize_log <- log(Pisa20094$schoolSize)
Pisa20094 <- Pisa20094[,-c(17,22)]
Pisa20094 <- Pisa20094[,-c(1)]
Pisa20094 <- Pisa20094[,c(which(colnames(Pisa20094)=="readingScore_log"),which(colnames(Pisa20094)!="readingScore_log"))] # move the column to the first column
cn <- which(colnames(Pisa20094) == 'readingScore_log' ) # check the column number of a column

## use lasso to select features
library(glmnet)
x4 <- as.matrix(Pisa20094[,2:32])
y4 <- as.double(Pisa20094[,1])
set.seed(123)
lasso4 <- cv.glmnet(x4, y4, family="gaussian", alpha=1)
lasso4$lambda.min
plot(lasso4)
coef.glmnet(lasso4, s = lasso4$lambda.min)
# feature selected
Pisa20094 <- subset(Pisa20094, select = -c(preschool, motherWork, selfBornUS, motherBornUS, fatherBornUS, studentsInEnglish, schoolHasLibrary, urban))

## use selected features to build a linear regression model
m4 <- lm(readingScore_log ~., data = Pisa20094)
summary(m4)

Pisa20094 <- subset(Pisa20094, select = -c(motherHS))
m4 <- lm(readingScore_log ~., data = Pisa20094)
summary(m4)

Pisa20094 <- subset(Pisa20094, select = -c(fatherWork))
m4 <- lm(readingScore_log ~., data = Pisa20094)
summary(m4)

Pisa20094 <- subset(Pisa20094, select = -c(englishAtHome))
m4 <- lm(readingScore_log ~., data = Pisa20094)
summary(m4)

## check multicollinearity
vif(m4)
# multicolinearity happens among dummy variables of grade

## check residuals 
mean(m4$residuals) 
plot(m4) # residuals are not so normal/still seems to be multiplicative 
durbinWatsonTest(m4)

## do cross validation
act_trans_4 = exp(Pisa20094$readingScore_log)
fit_trans_4 = exp(fitted.values(m4))
rmse4 = sqrt(mean((act_trans_4 - fit_trans_4)^2))
rmse4
#rmse3 = 73.7
# adj R^2 = 0.32
# no great improvement compared to model 1

#______________________________________________________________

### fifth try: use log transformation for  the dependent variable, and add interaction terms
library(data.table)
Pisa20095 <- copy(Pisa2009)

## do log transformation 
Pisa20095$readingScore_log <- log(Pisa20095$readingScore)
Pisa20095 <- Pisa20095[,-c(1)]
Pisa20095 <- Pisa20095[,c(which(colnames(Pisa20095)=="readingScore_log"),which(colnames(Pisa20095)!="readingScore_log"))] # move the column to the first column
cn <- which(colnames(Pisa20095) == 'readingScore_log' ) # check the column number of a column

## add interaction terms which are significant in m2
Pisa20095$read30MinsADay_schoolHasLibrary <- Pisa20095$read30MinsADay * Pisa20095$schoolHasLibrary
Pisa20095$read30MinsADay_minutesPerweekEnglish <- Pisa20095$read30MinsADay * Pisa20095$minutesPerWeekEnglish
Pisa20095$fatherBachelors_motherBachelors <- Pisa20095$fatherBachelors * Pisa20095$motherBachelors
Pisa20095$motherWork_motherBachelors <- Pisa20095$motherWork * Pisa20095$motherBachelors

## do lasso to select features
library(glmnet)
x5 <- as.matrix(Pisa20095[,2:36])
y5 <- as.double(Pisa20095[,1])
set.seed(123)
lasso5 <- cv.glmnet(x5, y5, family="gaussian", alpha=1)
lasso5$lambda.min
plot(lasso5)
coef.glmnet(lasso5, s = lasso5$lambda.min)

#feature selected 
Pisa20095 <- subset(Pisa20095, select = -c(preschool, selfBornUS, motherBornUS, fatherBornUS, studentsInEnglish, urban, read30MinsADay_minutesPerweekEnglish, motherWork_motherBachelors ))

## use selected features to build a linear regression model
m5 <- lm(readingScore_log ~., data = Pisa20095)
summary(m5)

Pisa20095 <- subset(Pisa20095, select = -c(motherHS))
m5 <- lm(readingScore_log ~., data = Pisa20095)
summary(m5)

Pisa20095 <- subset(Pisa20095, select = -c(motherWork))
m5 <- lm(readingScore_log ~., data = Pisa20095)
summary(m5)

Pisa20095 <- subset(Pisa20095, select = -c(fatherWork))
m5 <- lm(readingScore_log ~., data = Pisa20095)
summary(m5)

Pisa20095 <- subset(Pisa20095, select = -c(minutesPerWeekEnglish))
m5 <- lm(readingScore_log ~., data = Pisa20095)
summary(m5)

Pisa20095 <- subset(Pisa20095, select = -c(englishAtHome))
m5 <- lm(readingScore_log ~., data = Pisa20095)
summary(m5)

## check multicollinearity
vif(m5)
# multicolinearity happens among dummy variables of grade and interaction terms
# prune a interaction term and other insignificant terms

Pisa20095 <- subset(Pisa20095, select = -c(read30MinsADay_schoolHasLibrary))
m5 <- lm(readingScore_log ~., data = Pisa20095)
summary(m5)

Pisa20095 <- subset(Pisa20095, select = -c(schoolHasLibrary))
m5 <- lm(readingScore_log ~., data = Pisa20095)
summary(m5)

## check residuals 
mean(m5$residuals) 
plot(m5) # residuals are a little left skewed /still seems to be multiplicative 
durbinWatsonTest(m5)

## do cross validation
act_trans_5 = exp(Pisa20095$readingScore_log)
fit_trans_5 = exp(fitted.values(m5))
rmse5 = sqrt(mean((act_trans_5 - fit_trans_5)^2))
rmse5
#rmse3 = 73.9
# adj R^2 = 0.315
# no great improvement compared to model 1

### final result: Choose m2, the one with interaction terms.