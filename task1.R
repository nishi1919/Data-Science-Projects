# Task 1- The Sparks Foundation
# Prediction using Supervised ML
# Predict the percentage of an student based on the no. of study hours.

# Created by Nishi Agrawal




# Import all the library 

install.packages("dplyr")
library(dplyr)
install.packages("Hmisc")
library(Hmisc)
install.packages("caTools")
library(caTools)
install.packages("ggplot2")
library(ggplot2)



#  Read the Data.

data<-read.csv("C:/Users/mypc/Downloads/student_scores.csv", stringsAsFactors = FALSE)

str(data)

#to check for missing values
is.na(data)

# how many missing values
sum(is.na(data))

describe(data)

plot(data$Scores~data$Hours,xlab=" No. of Hours Studied", ylab ="Scores Achieved",main="Scores Achieved vs Hours Studied",col="blue",pch=20)




## train/ test split 

set.seed(42)

sampleSplit <- sample.split(Y=data$Scores, SplitRatio= 0.8)
train_set <- subset(x=data , sampleSplit==TRUE )
test_set <- subset(x=data , sampleSplit==FALSE )

# Train the model

model <- lm(formula=Scores~Hours, data =train_set )
summary(model)
model
abline(model)

#visulaise residuals

modelResiduals<- as.data.frame(residuals(model))
ggplot(modelResiduals, aes(residuals(model))) + geom_histogram(fill="red", color="black" )




#make predictions
pred <- predict(model ,test_set) 
pred

#evaluate predictions
modelEval <- cbind(test_set$Scores, pred)
colnames(modelEval) <- c('Actual', 'Predicted')
modelEval <- as.data.frame(modelEval)
head(modelEval)



# predicted score if a student studies for 9.25 hrs/ day?
Hours= 9.25
predicted_score<-predict(model,newdata = data.frame(Hours))
predicted_score

