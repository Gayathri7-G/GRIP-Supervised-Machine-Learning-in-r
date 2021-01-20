#Predict the percentage of an student based on the no. of study hours
#What will be predicted score if a student studies for 9.25 hrs/ day?
#Reading the csv data
library(caTools)
data<- read.csv("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv")
# viewing the first 25 entries of the data
head(data)
#summary of the data 
summary(data)
#ploting both variables i.e. Score and Hours
plot(x=data$Hours, y=data$Scores,
     xlab = "Hours", ylab = "Scores",
     main = "Scores V/s Hours", col = "violet")
cor(x=data$Hours, y=data$Scores) #shows positive correlation

#Linear Regression passed the formula and it passed 

modellm0 <- lm(Scores~Hours, data=data)
modellm0
summary(modellm0)
abline(modellm0, col="blue") #a=intercept; b=slope
plot(modellm0)

#names to access regession objects
names(modellm0)

#fitted valuesfor modellm0and plot fitted vs hours
modellm0$coefficients
modellm0$fitted.values
plot(data$Hours,modellm0$fitted.values)


#to predict score when student studies for 9.25hrs/day

#using coefficents
coefficients(modellm0) #to extract both slope and intercept
coef(modellm0)[1] #to extract intercept
coef(modellm0)[2] #to extract slope
coef(modellm0)[1]+coef(modellm0)[2]*9.25

#to use predict command
predict(modellm0,data.frame(Hours=9.25))

#REsult: the predicted score if a student studies for 9.25 hrs/ day is
#92.90985  percent (93 % approx)