library(ggplot2)
library(dplyr)
Dataset<- read.csv("CompleteResponses.csv")
attributes(Dataset)
str(Dataset)
is.na(Dataset)

# Change datatypes ####
Dataset$elevel<- as.ordered(Dataset$elevel)
str(Dataset)
Dataset$car<- as.factor(Dataset$car)
str(Dataset)
Dataset$zipcode<- as.factor(Dataset$zipcode)
str(Dataset)
Dataset$brand<- as.factor(Dataset$brand)


Dataset$brand_name <- apply(Dataset["brand"],
                                     
                                     MARGIN = 2,
                                     
                                     function(x) if_else(x == 0, "Acer", "Sony"))


#create ranges for age and salary ####
salarycat<- Dataset$salary
salarycat<- cut(salarycat,5)

agecat<- Dataset$age
agecat<- cut(agecat,4)

creditcat<- Dataset$credit
creditcat <- cut(creditcat, 5)

age# Create  plot ####
library("ggplot2")
ggplot(Dataset, aes(x = age, y = salary)) + geom_point()

ggplot(Dataset, aes(x = zipcode, y = elevel)) + geom_point()

ggplot(Dataset, aes(x = agecat, y = brand))+ 
  geom_point(position = "jitter")

ggplot(Dataset, aes(x = salarycat, y = brand))+ 
  geom_point(position = "jitter")

ggplot(Dataset, aes(x = zipcode, y = salarycat))+ 
  geom_point(position = "jitter")

ggplot(Dataset, aes(x = zipcode, y = elevel))+ 
  geom_point(position = "jitter") +
  facet_wrap("brand")

ggplot(Dataset, aes(x = creditcat, y = salary))+ 
  geom_point(position = "jitter") + 
  facet_wrap("brand")

ggplot(Dataset, aes(x = age, y = salary))+ 
  geom_point(position = "jitter") + 
  facet_wrap("brand")

ggplot(Dataset, aes(x = car, y = zipcode))+ 
  geom_point(position = "jitter") +
  facet_wrap("brand")

ggplot(Dataset, aes(x = brand, y = salary)) + 
  geom_boxplot()

#Create bar graph ####

library (gcookbook)

Dataset %>%
  group_by(brand) %>%
  summarize(mean_salary = mean(salary, na.rm = TRUE)) -> dataset2
            
Dataset %>%
  group_by(brand) %>%
  summarize(mean_credit = mean(credit, na.rm = TRUE)) -> dataset3

Dataset %>%
  group_by(brand) %>%
  summarize(mean_age = mean(age, na.rm = TRUE)) -> dataset4


ggplot(dataset2, aes(x = elevel , y = mean_salary, fill = brand)) + 
  geom_col(position = "dodge")     

ggplot(dataset2, aes(x = brand , y = mean_salary)) + 
  geom_col() 

ggplot(dataset2, aes(x = zipcode , y = mean_salary, fill = brand)) + 
  geom_col() 

ggplot(dataset2, aes(x = car , y = mean_salary, fill = brand)) + 
  geom_col()

ggplot(dataset2, aes(x = zipcode , y = mean_salary, fill = brand)) + 
  geom_col()

# Create bargraph of count ####
ggplot(Dataset, aes(x = brand)) +
  geom_histogram(stat = "count")

ggplot(Dataset, aes(x = car)) +
  geom_bar()

ggplot(Dataset, aes(x = zipcode)) +
  geom_bar()

ggplot(age, aes(x = age)) +
  geom_bar()

ggplot(Dataset, aes(x = agecat)) +
  geom_bar()

# Create histogram ####
hist(Dataset$salary)
hist(Dataset$age)
hist(Dataset$credit)
hist(Dataset$zipcode)
hist(Dataset$credit)
hist(Dataset$brand)
hist(Dataset$elevel)
hist(Dataset$brand)

ggplot(Dataset, aes(x = salary)) +
  geom_histogram(binwidth = 20000, fill = "white", colour = "black")
  
ggplot(Dataset, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "white", colour = "black")

ggplot(Dataset, aes(x = credit)) +
  geom_histogram(binwidth = 50000, fill = "white", colour = "black")

summary(Dataset)

# Train C5.0 model ####
str(Dataset)
library(caret)
library(lattice)
install.packages("C50")
library(C50)
install.packages("inum")
library(inum)
set.seed(123)
inTrain <- createDataPartition(y = Dataset$brand, p=.75,
                               list=FALSE)  
training <- Dataset[ inTrain,] 
testing <- Dataset[-inTrain,]

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 1) 

C5.0Fit <- train(brand ~ .,  
                data = training,
                method = "C5.0",  
                tuneLength = 2,         
                trControl = ctrl) 
C5.0Fit

modelLookup("C5.0")

# check errors on testing C5.0
predictions <- predict(object = C5.0Fit, newdata = testing)
testing$pred <- predictions
head(testing)

# metrics
PostResample(pred = predictions, obs = testing$brand)

# Train RF model ####
library(randomForest)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 1) 
mtryGrid <- expand.grid(mtry = 6)

RFFit <- train(brand ~ .,  
                 data = training,
                 method = "rf",  
                 tunegrid = mtryGrid,         
                 trControl = ctrl)
RFFit

# check errors on testing RF
predictions <- predict(object = RFFit, newdata = testing)
testing$pred <- predictions
head(testing)

testing$brand_name_pred. <- apply(testing["pred"],
                            
                            MARGIN = 2,
                            
                            function(x) if_else(x == 0, "Acer", "Sony"))

RF_Plot<- testing %>% ggplot(aes(x = age, y = salary)) +
  
  geom_point(aes(color = testing$brand_name_pred.)) +
  
  labs(title = "RF model with predictors Age and Salary") +
  
  scale_color_manual(values = c("royalblue4", "red")) +
  
  theme(element_blank())

RF_Plot
# metrics
postResample(pred = predictions, obs = testing$brand)

#Train knn model####
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 1) 
set.seed(123)
knnFit <- train(brand ~ .,  
                 data = training,
                 method = "knn",  
                 tuneLength = 15, 
                 preProcess =c("center", "scale"),
                 trControl = ctrl)

knnFit

# check errors on testing knnFit
predictions <- predict(object = knnFit, newdata = testing)
testing$pred <- predictions
head(testing)
# metrics
postResample(pred = predictions, obs = testing$brand)
                 




# Train svm model####
install.packages("e1071")
library("e1071")

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 1) 
set.seed(123)

SvmFit <- train(brand ~ .,  
                data = training,
                method = "svmRadial", 
                preProcess =c("center", "scale"),
                tuneLength = 15, 
                trControl = ctrl)

SvmFit

# check errors on testing SvmFit
predictions <- predict(object = SvmFit, newdata = testing)
testing$pred <- predictions
head(testing)
# metrics
postResample(pred = predictions, obs = testing$brand)
# Estimate variable importance####  
C5.0Imp<- varImp(C5.0Fit, scale = FALSE)
C5.0Imp

RFFitImp<- varImp(RFFit, scale = TRUE)
RFFitImp
plot(RFFitImp, top = 10)
varImp


#Plot training model####
plot(C5.0Fit)
plot(RFFit)

#Predict model####
Dataset<- read.csv("CompleteResponses.csv")
attributes(Dataset)
str(Dataset)
is.na(Dataset)

# Change datatypes for prediction model####
DataInc<- read.csv("SurveyIncomplete.csv")
attributes(DataSurveyIncomplete)
str(DataSurveyIncomplete)
is.na(DataSurveyIncomplete)
DataInc$elevel<- as.ordered(DataInc$elevel)
DataInc$car<- as.factor(DataInc$car)
DataInc$zipcode<- as.factor(DataInc$zipcode)
DataInc$brand<- as.factor(DataInc$brand)
summary(DataInc)

predictions <- predict(object = RFFit, newdata = DataInc)
DataInc$brand <- predictions
head(DataInc)

# metrics
# postResample(pred = predictions, obs = DataInc$brand)

summary(Dataset)

rbind()