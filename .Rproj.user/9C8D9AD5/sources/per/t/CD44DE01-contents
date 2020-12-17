# make sure environment is right
getwd()
setwd("D:/Master of Data Science/STAT7102/Project/Instagram Fake or Real Account/Instagram Fake Account Predictor")

# reading train and test data
train <- read.csv("D:/Master of Data Science/STAT7102/Project/Instagram Fake or Real Account/Instagram Fake Account Predictor/train.csv")
test <- read.csv("D:/Master of Data Science/STAT7102/Project/Instagram Fake or Real Account/Instagram Fake Account Predictor/test.csv")


dim(train) # 576 12
dim(test) # 120 12

head(train)
fix(train)

# see all columns
names(train)

hist(train$fake)
hist(test$fake)

summary(train)

# Load packages
install.packages("PerformanceAnalytics")
install.packages("corrplot")
install.packages("caret")
library(nlme)
library(glm2)
library(forecast)
library(lmtest)
library(foreach)
library(ggplot2)
library("PerformanceAnalytics")
library(corrplot)
source("http://www.sthda.com/upload/rquery_cormat.r")
library(dplyr)
library(lme4)
library(mgcv)
library(caret)


# make sure categorical variables are indeed factor variables
train$fake <- as.factor(train$fake)
train$profile.pic <- as.factor(train$profile.pic)
train$name..username <- as.factor(train$name..username)
train$external.URL <- as.factor(train$external.URL)
train$private <- as.factor(train$private)

test$fake <- as.factor(test$fake)
test$profile.pic <- as.factor(test$profile.pic)
test$name..username <- as.factor(test$name..username)
test$external.URL <- as.factor(test$external.URL)
test$private <- as.factor(test$private)

# plot the histogram of each variable
#plot(train$fake~train$description.length)
hist(as.numeric(train$profile.pic))
hist(train$nums.length.username)
hist(train$fullname.words)
hist(train$nums.length.fullname)
hist(as.numeric(train$name..username))
hist(train$description.length)
hist(as.numeric(train$external.URL))
hist(as.numeric(train$private1))
hist(train$X.posts)
hist(train$X.followers)
hist(train$X.follows)
hist(log(train$description.length))

# create another dataframe for having train data in numeric type
numeric_train <- mutate_all(train, function(x) as.numeric(as.character(x)))
rquery.cormat(numeric_train, type="flatten", graph=FALSE)

corr_col<- colorRampPalette(c("blue", "white", "red"))(20)
rquery.cormat(numeric_train, type="full", col=corr_col)


#### relationship plots between fake and other variables
## Factor variables
ggplot(data = train) + geom_count(mapping = aes(x = profile.pic, y = fake))
ggplot(data = train) + geom_count(mapping = aes(x = name..username, y = fake))
ggplot(data = train) + geom_count(mapping = aes(x = external.URL, y = fake))
ggplot(data = train) + geom_count(mapping = aes(x = private, y = fake))

## Continuous variables

# Histogram plots without log
ggplot(train, aes(x=nums.length.fullname, color=fake)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")
ggplot(train, aes(x=nums.length.username, color=fake)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")
ggplot(train, aes(x=fullname.words, color=fake)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")
ggplot(train, aes(x=X.posts, color=fake)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")
ggplot(train, aes(x=X.followers, color=fake)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")
ggplot(train, aes(x=X.follows, color=fake)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")
ggplot(train, aes(x=description.length, color=fake)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")

# Density plots with log 10 transformation
ggplot(train, aes(x=nums.length.fullname, color=fake)) +
  geom_histogram(fill="white", alpha=0.5, position="identity") + 
  geom_density(alpha=.5) + scale_x_log10() 
ggplot(train, aes(x=nums.length.username, color=fake)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density() + scale_x_log10()
ggplot(train, aes(x=fullname.words, color=fake)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density() + scale_x_log10()
ggplot(train, aes(x=X.posts, color=fake)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density() + scale_x_log10()
ggplot(train, aes(x=X.followers, color=fake)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density() + scale_x_log10()
ggplot(train, aes(x=X.follows, color=fake)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density() + scale_x_log10()
ggplot(train, aes(x=description.length, color=fake)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density() + scale_x_log10()


# Generalized Linear Model with Binomial distribution
fakefit1 <- glm2(fake~ ., family = "binomial", data = train)
summary(fakefit1)

# mutate: Add new log variables by preserving existing train dataframe
#train <- train %>% mutate(lognum1 = log10(nums.length.fullname + 1))
#train <- train %>% mutate(lognum2 = log10(nums.length.username + 1))
#train <- train %>% mutate(logwords = log10(fullname.words + 1))
#train <- train %>% mutate(logXPosts = log10(X.posts + 1))
#train <- train %>% mutate(logXFollowers = log10(X.followers + 1))
#train <- train %>% mutate(logXFollows = log10(X.follows+ 1))
#train <- train %>% mutate(logXdescription = log10(description.length+ 1))

# with all log transformed variables
fakefit2 <- glm2(fake~profile.pic+nums.length.username+X.posts+
                X.followers+X.follows+
                log10(description.length+ 1)+log10(nums.length.fullname + 1)+
                log10(nums.length.username + 1)+log10(fullname.words + 1)+
              log10(X.posts + 1)+log10(X.followers + 1)+ log10(X.follows + 1), 
                 family = "binomial", data = train)
summary(fakefit2)

# shortlisted models
fakefit3 <- glm2(fake~profile.pic+nums.length.username+X.posts+
                   X.follows+log10(description.length+ 1)+
                   log10(X.followers + 1)
                 , family = "binomial", data = train)
summary(fakefit3)

# test interaction 1
fakefit4 <- glm2(fake~profile.pic+nums.length.username+
                   X.follows+log10(description.length+ 1)+
                   log10(X.followers + 1)+
                   (private * X.posts)
                 , family = "binomial", data = train)
summary(fakefit4)

# test interaction 2
fakefit5 <- glm2(fake~profile.pic+nums.length.username+
                   X.follows+log10(description.length+ 1)+
                   log10(X.followers + 1)+private+
                   (external.URL * description.length)
                 , family = "binomial", data = train)
summary(fakefit5)

# general additive model
# should fit all s first and see the relationships
fakegam1 <- gam(fake~profile.pic+s(nums.length.username)+ s(X.posts)+
                s(X.followers) + s(X.follows) + s(description.length)+ 
                s(nums.length.fullname)+name..username+
                s(fullname.words, k = 9)+
                external.URL + private 
                , family = "binomial", data = train)
summary(fakegam1)
plot(fakegam1, ylim = c(-10, 10))

AIC(fakegam1)

fakegam2 <- gam(fake~profile.pic+s(nums.length.username)+s(X.posts)+
                  s(X.follows)+s(description.length)+
                  log10(X.followers+1) +
                  s(nums.length.fullname)
                , family = "binomial", data = train)
summary(fakegam2)
AIC(fakegam2)

fakegam3 <- gam(fake~profile.pic+s(nums.length.username)+s(X.posts)+
                  s(X.follows)+s(description.length)+
                  log10(X.followers+1)
                , family = "binomial", data = train)
summary(fakegam3)
AIC(fakegam3)

fakegam4 <- gam(fake~profile.pic+s(nums.length.username)+s(X.posts)+
                  s(X.follows)+s(description.length)+
                  s(X.followers)
                , family = "binomial", data = train)
summary(fakegam4)
AIC(fakegam4)

print.noquote(c(fakegam3$coef))

# model selection based on AIC
AIC(fakefit1, fakefit2, fakefit3, fakefit4, fakefit5, fakegam1, fakegam2, fakegam3, fakegam4)


write.table(AIC(fakefit1, fakefit2, fakefit3, 
                fakefit4, fakefit5, fakegam1, 
                fakegam2, fakegam3, fakegam4), "clipboard", sep="\t", row.names=FALSE)

# use predict() function to compute predicted probabilities
library(mgcv)
predicted <- predict.gam(fakegam3, newdata = test)
pred <- ifelse(predicted < 0, 0, 1)

# summarize results
confusionMatrix(as.factor(pred),as.factor(test$fake))






