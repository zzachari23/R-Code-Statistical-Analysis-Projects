#---------Project3---------#

#Installs Packages and Loads Libraries 
install.packages("broom")
install.packages("AICcmodavg")
install.packages("tidyverse")
install.packages("readr")
install.packages("car")
library(readr)
library(broom)
library(AICcmodavg)
library(tidyverse)
library(car)


#loads math file 
MATH_FILE <- read.csv("Desktop/student-mat.csv", sep = ";", header = TRUE)
View(MATH_FILE)
#loads Portuguese file 
PORT_FILE <- read.csv("Desktop/student-por.csv", sep = ";", header=TRUE)
View(PORT_FILE)

#Converts Dalc and Walc columns to factor type for math file
MATH_FILE$Dalc <- as.factor(MATH_FILE$Dalc)
MATH_FILE$Walc <- as.factor(MATH_FILE$Walc)


#Converts Dalc and Walc columns to factor type for port file 
PORT_FILE$Dalc <- as.factor(PORT_FILE$Dalc)
PORT_FILE$Walc <- as.factor(PORT_FILE$Walc)

#Converts freetime and goout columns to factor type for math file
MATH_FILE$freetime <- as.factor(MATH_FILE$freetime)
MATH_FILE$goout <- as.factor(MATH_FILE$goout)



#---------------------------------------------------------------------------#

#Section 1 Test of means 

#Question 1: Perform the appropriate t-test to determine if the means for 
#G1 grade and G2 grades are the same for the Portuguese language data set.

#Testing for normality using q-q plot 
qqnorm(PORT_FILE$G1)
qqline(PORT_FILE$G1)

qqnorm(PORT_FILE$G2)
qqline(PORT_FILE$G2)

#Paired T-Test
t.test(PORT_FILE$G1, PORT_FILE$G2, paired = TRUE)

#Question 2: Perform the appropriate t-test to determine if the means for G1
#grade and G2 grades are the same for the Mathematics dataset.

#Testing for normality using q-q plot 
qqnorm(MATH_FILE$G1)
qqline(MATH_FILE$G1)

qqnorm(MATH_FILE$G2)
qqline(MATH_FILE$G2)


#Paired T-test
t.test(MATH_FILE$G1, MATH_FILE$G2, paired = TRUE)


#Question 3: Perform the appropriate t-test to determine if mean G3 grades 
#for Mathematics are the same as the G3 grades for Portuguese.

#Testing for normality using q-q plot 
qqnorm(MATH_FILE$G3)
qqline(MATH_FILE$G3)

qqnorm(PORT_FILE$G3)
qqline(PORT_FILE$G3)


#Welch Two Sample T-test
t.test(MATH_FILE$G3, PORT_FILE$G3, var.equal = FALSE)

#Question 4: Perform the appropriate t-test to determine if the means of 
#G3 grades are the same for both Portuguese and Mathematics for students 
#appearing in both datasets.

#Merges the Mathematics and Portuguese language files 
merged_file <- merge(MATH_FILE,PORT_FILE,by=c("school","sex","age","address","famsize",
          "Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
View(merged_file)


#Testing for normality using q-q plot 
qqnorm(merged_file$G3.x)
qqline(merged_file$G3.x)

qqnorm(merged_file$G3.y)
qqline(merged_file$G3.y)

#Paired T-test 
t.test(merged_file$G3.x, merged_file$G3.y, paired = TRUE)


#---------------------------------------------------------------------------#
#Section 2 ANOVA 

#Question 1:
#Perform a 2-way ANOVA examining the effect of weekday and weekend alcohol
#consumption on G3 grades for the Mathematics data set.

#Based on the QQ-plot on the G3 column for the math data set we can say that 
#the data is normally distributed 
qqnorm(MATH_FILE$G3)
qqline(MATH_FILE$G3)

#Two-Way ANOVA Additive 
two_way_additive <- aov(G3 ~ Dalc + Walc, data = MATH_FILE)
summary(two_way_additive)

#Two-Way ANOVA Interaction 
two_way_interaction <- aov(G3 ~ Dalc * Walc, data = MATH_FILE)
summary(two_way_interaction)

#AIC 
model_set <- list(two_way_additive, two_way_interaction)
model_names <- c("two_way_additive", "two_way_interaction")
aictab(model_set, modnames = model_names)

#Best-Fit 
#The best performing model is the two-way ANOVA additive model. Based on the 
#summary of the model we do not see any statistical significance for workday  
#and weekend alcohol consumption. We have Pr>f values: 0.180, 0.751

#Tukey HSD Test - Since we do not reject the null hypothesis which means there
#is no statistical significance, we do not need to run the Tukey HSD Test.

#Question 2:
#Perform a 2-way ANOVA examining the effect of weekday and weekend alcohol 
#consumption on G3 grades for the Portuguese data set.

#Based on the QQ-plot on the G3 column for the port data set we can say that 
#the data is normally distributed 
qqnorm(PORT_FILE$G3)
qqline(PORT_FILE$G3)

#Two-Way ANOVA Additive 
two_way_additive <- aov(G3 ~ Dalc + Walc, data = PORT_FILE)
summary(two_way_additive)

#Two-Way ANOVA Interaction 
two_way_interaction <- aov(G3 ~ Dalc * Walc, data = PORT_FILE)
summary(two_way_interaction)

#AIC 
model_set <- list(two_way_additive, two_way_interaction)
model_names <- c("two_way_additive", "two_way_interaction")
aictab(model_set, modnames = model_names)

#Best-Fit 
#The best performing model is the two-way ANOVA additive model. Based on the 
#summary of the model we do see statistical significance for weekday  
#alcohol consumption. We have Pr>f value: 1.9e-06 (0.0000019). 

#Tukey HSD Test: Since we reject the null as one of our Pr>f values are
#less than our alpha of 0.05. 
TukeyHSD(two_way_additive)
#After running the Tukey HSD test, we see that the pairs that show statistical 
#significance are the 2-1(Weekday level) pair, 4-1(Weekday level) pair, and 
#4-2(Weekday level) pair. 

#---------------------------------------------------------------------------#

#Section 3 Correlational Analysis 
#Question 1: 
#Is age correlated (using a Pearson R) to G3 grade for Mathematics? 

#Testing for normality using q-q plot 
qqnorm(MATH_FILE$age)
qqline(MATH_FILE$age)

qqnorm(MATH_FILE$G3)
qqline(MATH_FILE$G3)


#Pearson R Test, calculates Correlation Coefficient using cor.test() method 
result = cor.test(MATH_FILE$age, MATH_FILE$G3, method = "pearson")
print(result)


#Question 2: 
#Is age correlated (using a Pearson R) to G3 grade for Portuguese language?

qqnorm(PORT_FILE$age)
qqline(PORT_FILE$age)

qqnorm(PORT_FILE$G3)
qqline(PORT_FILE$G3)

#Pearson R Test, calculates Correlation Coefficient using cor.test() method 
result = cor.test(PORT_FILE$age, PORT_FILE$G3, method = "pearson")
print(result)

#Question 3: 
#For the students appearing in both datasets, does the grade in Portuguese 
#language correlate to the grade in Mathematics (using a Pearson R)?

#Testing for normality using q-q plot 
qqnorm(merged_file$G3.y)
qqline(merged_file$G3.y)

qqnorm(merged_file$G3.x)
qqline(merged_file$G3.x)

#Pearson R Test, calculates Correlation Coefficient using cor.test() method 
result = cor.test(merged_file$G3.y, merged_file$G3.x, method = "pearson")
print(result)


#---------------------------------------------------------------------------#

#Section 4 Linear Regression Analysis

#Question 1: 
#Choose one of the correlational analyses you ran above to use for a linear
#regression analysis. What is your predictor variable and what is your response 
#variable? 

#Chosen correlational analysis: Is age correlated to G3 grade for Mathematics?

#Simple Linear Regression 
#x(Predictor Variable) = Age for Mathematics 
#y(Response Variable) = G3 grade for Mathematics 

#Scatter plot 
plot(MATH_FILE$age, MATH_FILE$G3, xlab="Age", ylab="G3 Grade") 
#After examining the scatter plot, it seems to be that the relationship between
#the variables do not look linear,  therefore this assumption does not pass.
#Therefore, we had to transform the data. 


#Transforms data by taking the log of both the predictor and response variable
#MATH_FILE$age <- log(MATH_FILE[,'age'])
#MATH_FILE$G3 <- log(MATH_FILE[,'G3'])


#Note: : Showed in officer hours, I tried transforming the data by 
#taking the log of the predictor and response variables, but the
#data still seemed to be non-linear. I was told to use the
#non-transformed data for the linear regression and the rest 
#of the assumptions.

#Linear Regression 
LR_Model <- lm(MATH_FILE$G3 ~ MATH_FILE$age, data = MATH_FILE)
summary(LR_Model)

#Checking residuals plots:checking homoscedasticity 
res <- resid(LR_Model)
plot(fitted(LR_Model), res)
abline(0,0)

#Box plot on residuals 
boxplot(res)

#Checking for normality using QQ plot 
qqnorm(res)
qqline(res)

#---------------------------------------------------------------------------#

#Section 5 ANOVA or Linear Regression Analysis

#Perform a 2-way ANOVA examining the effect of free time after school and 
#going out with friends on G3 grades for the Mathematics dataset.

#Based on the QQ-plot on the G3 column for the math data set we can say that 
#the data is normally distributed 
qqnorm(MATH_FILE$G3)
qqline(MATH_FILE$G3)


#Two-Way ANOVA Additive 
two_way_additive <- aov(G3 ~ freetime + goout, data = MATH_FILE)
summary(two_way_additive)

#Two-Way ANOVA Interaction 
two_way_interaction <- aov(G3 ~ freetime * goout, data = MATH_FILE)
summary(two_way_interaction)

#AIC 
model_set <- list(two_way_additive, two_way_interaction)
model_names <- c("two_way_additive", "two_way_interaction")
aictab(model_set, modnames = model_names)

#Best-Fit 
#The best performing model is the two-way ANOVA additive model. Based on the 
#summary of the model we do see statistical significance for going out with
#friends but not free time. We have Pr>f values: 0.0583,  0.0021.

#Tukey HSD Test: Since we reject the null as one of our Pr>f values are
#less than our alpha of 0.05. 
TukeyHSD(two_way_additive)
#After running the Tukey HSD test, we see that the pairs that show statistical 
#significance are the 5-2(goout level) pair and the 5-3(goout level) pair.




