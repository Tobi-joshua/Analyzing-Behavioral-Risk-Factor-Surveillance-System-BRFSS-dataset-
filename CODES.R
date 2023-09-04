# The Behavioral Risk Factor Surveillance System (BRFSS) dataset is a collection of public health surveys conducted from 2011 to 2015. These surveys involve data from approximately 400,000 people and aim to gather uniform, state-specific information on various health-related topics, including preventive health practices and risk behaviors associated with chronic diseases, injuries, and preventable infectious diseases in the adult population. Below are some key details about the dataset:
#   
#   Objective:
#   
#   The objective of the BRFSS is to collect state-specific data on health-related risk behaviors, chronic health conditions, and the use of preventive health services among U.S. residents.
# Topics Assessed:
#   
#   The factors assessed by the BRFSS include, but are not limited to:
#   Tobacco use
# Health care coverage
# HIV/AIDS knowledge or prevention
# Physical activity
# Fruit and vegetable consumption
# Data Collection Method:
#   
#   Data for this dataset are collected through telephone surveys.
# A random sample of adults (one per household) is surveyed.
# Dataset Coverage:
#   
#   The BRFSS dataset covers the years 2011 to 2015.
# It includes data from all 50 states in the United States, the District of Columbia, and three U.S. territories.
# Scale:
#   
#   BRFSS conducts more than 400,000 adult interviews each year, making it one of the largest continuously conducted health survey systems globally.
# Data Structure:
#   
#   Each year's dataset consists of several hundred columns.
# Detailed information about the dataset's columns and variables can be found in the annual code books.
# Data Format:
#   
#   The dataset is provided in CSV (Comma-Separated Values) format.
# Note that the CSV files were converted from a SAS data format using pandas, so there may be some data artifacts as a result.
# Acknowledgments:
#   
#   This dataset was released by the CDC (Centers for Disease Control and Prevention).


## Reading the data
data <- read.csv("BRFSS2015_650.csv")

## Viewing the data
View(data)

#  Q1: How many people reported their general health is excellent?
#  1 represents "EXCELLENT"
Q1 <- sum(data$GENHLTH == 1,na.rm = TRUE)



#Q2. What is the highest value for number of adult women in the household where
#someone has ever had a stroke? Summarise the value in a variable called
#max_numwomen

patients_with_stroke <- data[data$CVDSTRK3==1,]
max_numwomen<-max(patients_with_stroke$NUMWOMEN, na.rm = TRUE)
max_numwomen




#Q3: Compute the means and standard deviations for MENTHLTH comparing caregivers
#who managed personal care such as giving medications, feeding, dressing, or bathing
#and those who did not. The output should be a dataframe
  
CRGVPERS<-c(1,2)
mean_health <- c(
  round(mean(data[data$CRGVPERS==1,]$MENTHLTH,na.rm = TRUE),2),
  round(mean(data[data$CRGVPERS==2,]$MENTHLTH,na.rm = TRUE),2)
)
sd_health <- c(
  round(sd(data[data$CRGVPERS==1,]$MENTHLTH,na.rm = TRUE),2),
  round(sd(data[data$CRGVPERS==2,]$MENTHLTH,na.rm = TRUE),2)
)

Q3 <- data.frame(CRGVPERS,mean_health,sd_health)
Q3




#Q4: What is the median age when respondents were told they had diabetes for those
#living in Pennsylvania? Only calculate it for those who gave an age. The value should be
#called med.diab.age and be in a 1 x 1 dataframe

pat_living_in_penn <- data[data$X_STATE == 42,]
dia_pat_living_in_penn <- pat_living_in_penn[pat_living_in_penn < 98]
med.diab.age<-c(median(data$DIABAGE2,na.rm = TRUE))
Q4 <- data.frame(med.diab.age)
Q4





#Q5: Predict number of days in the past 30 days mental health was not good from marital
# status. Assign the summary of the model to Q5

library(tidyverse)
model <- lm(MENTHLTH ~ MARITAL, data = data)
Q5 <- summary(model)
Q5



# Q6: Use summarise to compare the mean number of days in the past 30 days mental
#health was not good by marital status and assign to Q6 as a dataframe. Round to two
#decimals. The mean should be called mean_mental. You should be able to confirm the
#results of Q5 after creating this

Q6 <- data %>% 
  group_by(MARITAL) %>% #The data is grouped by marital status
  summarise(mean_mental = round(mean(MENTHLTH),2)) # For each of the group, the mean number of days... is computed to 2 dp
Q6






#Q7 Calculate the means and standard deviations of MENTHLTH for those who have had a
#stroke and those who have not had a stroke only for those who do not have any kind of
#healthcare coverage
  
pat_without_health_cov <- data[data$HLTHPLN1==2,]
MENTHLTH_NO_COV <- c(1,2)
mean_mental <- c(
  round(mean(data$CVDSTRK3==1,na.rm = TRUE),2),
  round(mean(data$CVDSTRK3==2,na.rm = TRUE),2)
)
sd_mental <- c(
  round(sd(data$CVDSTRK3==1,na.rm = TRUE),2),
  round(sd(data$CVDSTRK3==2,na.rm = TRUE),2)
)
Q7 <- data.frame(MENTHLTH_NO_COV,mean_mental,sd_mental)
Q7





#Q8 Run an ANOVA comparing how many times per week respondents took part in the
#exercise they spent the most time doing in the past month by marital status, and assign
# the TukeyHSD post-hoc test to Q8

per_week_data <- data[data$EXEROFT1 %in% 1:199,]
Q8 <- aov(EXEROFT1~as.factor(MARITAL), data = per_week_data)
TukeyHSD(Q8)






#Q9: Calculate the variance in number of days a respondent drank alcohol in the past
#week for each type of physical activity or exercise spent in the past month for men

daily_cons_data <- data[data$ALCDAY5<=199,]
Q9 <- daily_cons_data %>%
  group_by(EXRACT11) %>%
  summarise(var_drinks = round(var(ALCDAY5, na.rm = TRUE),2))
attach(Q9)
Q9 <- Q9[order(-var_drinks),][1:6,]
Q9
detach(Q9)





# Q10: Address the values of any variables. For instance, is "none" equal to a value other
# than 0? Are there extra decimals implied?

#The selected variable is `ALCDAY5`
# 101 - 199     Days per week
# 201 - 299     Days in past 30 days
# 777           Don’t know/Not sure
# 888           No drinks in past 30 days
# 999           Refused
# BLANK         Not asked or Missing






# Q11: Remove any outliers. Briefly explain why you chose the method you used. Make
# sure to comment it out

boxplot<-boxplot(data$PHYSHLTH)$out
mtext(paste("Outliers :", paste(boxplot, 
                                collaspe=",")))

quartiles <- quantile(data$PHYSHLTH, probs=c(.25, .75), na.rm = TRUE)
IQR <- IQR(data$PHYSHLTH,na.rm = TRUE)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(data, data > Lower & data < Upper)
data_no_outlier





# Q12: Complete exploratory analyses doing appropriate visualizations with ggplot2.

ggplot(data = data, mapping = aes(x= NUMWOMEN, y = PHYSHLTH))+geom_point()





# Q13: Run basic descriptive statistics
summary(data$PHYSHLTH)





# Q14: Finally, run an appropriate regression predicting one of those variables. Identify the
# best model
model1<- lm(PHYSHLTH ~ MENTHLTH + NUMWOMEN + GENHLTH,data = data)
model2<- lm(PHYSHLTH ~ MENTHLTH + ALCDAY5 + GENHLTH,data = data)
model3<- lm(PHYSHLTH ~ MENTHLTH + MARITAL + EXEROFT1,data = data)

summary(model1)
summary(model2)
summary(model3)

## The best model is the model 3
best_model <- model3