# Formal analyis for study
# Filename - Formal_Analysis.R
# Date - 01/02/21
# Author - CA/MB

# libraries
library(tidyverse)
library(MASS)

# Load in data
survey_data <- read.csv("survey_data.csv")
dim(survey_data)

# Tidy data
survey_data$Statistics <- as.factor(survey_data$Statistics)
survey_data$Mathematics <- as.factor(survey_data$Mathematics)
survey_data$Nationality <- as.factor(survey_data$Nationality)
survey_data$Course <- as.factor(survey_data$Course)
survey_data$Specialist <- as.factor(survey_data$Specialist)
survey_data$Voluntary <- as.factor(survey_data$Voluntary)
survey_data$Gender <- as.factor(survey_data$Gender)

survey_data$Nationality <- factor(survey_data$Nationality,levels=levels(survey_data$Nationality)[c(1,4,2,3)],ordered=F)
survey_data$Course <- factor(survey_data$Course,levels=rev(levels(survey_data$Course)),ordered=F)
survey_data$Specialist <- factor(survey_data$Specialist,levels=rev(levels(survey_data$Specialist)),ordered=F)
survey_data$Voluntary <- factor(survey_data$Voluntary,levels=rev(levels(survey_data$Voluntary)),ordered=F)
survey_data$Year.of.Study <- factor(survey_data$Year.of.Study, ordered=F, 
                                    levels=c("Year 1", "Year 2", "Year 3", "Year 4", "Masters year"))


# Subsetted data for voluntary & non-specialist cases
survey_volun <- survey_data[survey_data$Voluntary=="Voluntary",]
survey_volun$Voluntary <- droplevels(survey_volun$Voluntary)
survey_spec <- survey_data[survey_data$Specialist=="Non-Specialist",]
survey_spec$Specialist <- droplevels(survey_spec$Specialist)

# Subsetted data excluding S1A
survey_performance <- survey_data[survey_data$Course!="Course 3",]
survey_performance$Course <- droplevels(survey_performance$Course)


##                 ##
# Table X intervals #
##                 ##

#spec vs non-spec
t.test(survey_data$SA.mean[survey_data$Specialist=="Specialist"],
       survey_data$SA.mean[survey_data$Specialist=="Non-Specialist"])

#volun vs non-volun
t.test(survey_data$SA.mean[survey_data$Voluntary=="Voluntary"],
       survey_data$SA.mean[survey_data$Voluntary=="Non-voluntary"])

#spec vs non spec for non-volun
t.test(survey_volun$SA.mean[survey_volun$Specialist=="Specialist"],
       survey_volun$SA.mean[survey_volun$Specialist=="Non-Specialist"])

#volun vs non-volun
t.test(survey_spec$SA.mean[survey_spec$Voluntary=="Voluntary"],
       survey_spec$SA.mean[survey_spec$Voluntary=="Non-voluntary"])


##             ##
# Figure 2 CI's #
##             ##

# Gender
t.test(survey_data$SA.mean[survey_data$Gender=="Female"],
       survey_data$SA.mean[survey_data$Gender=="Male"])

# Nationality ANOVA
summary(aov(SA.mean~Nationality,data=survey_data))

# Correlation coefficient CI for SA vs SE
cor.test(survey_data$SA.mean,survey_data$SE.mean)


##             ##
# Anxiety Model #
##             ##

# Fit Full linear model
# For stepAIC to work, need to remove missing values 
model_data <- na.omit(survey_data[,c("SA.mean","Gender","Age","Nationality","Year.of.Study",
                                     "SE.mean","Specialist","Voluntary")])

# MB Removed: ,"Highest.Qual","Course"

model_SA <- lm(SA.mean~Gender+Age+Nationality+Year.of.Study
               +SE.mean+Specialist+Voluntary,data=model_data)

# Run stepwise regression using AIC
step_model_SA <- stepAIC(model_SA,direction='both',trace=T)

# Check assumptions 
par(mfrow=c(2,2))
plot(step_model_SA)

summary(step_model_SA)
confint(step_model_SA)


##             ##
# Figure 3 CI's #
##             ##

# Create 2 datasets for each course
course1_data <- survey_performance[survey_performance$Course=="Course 1",]
course2_data <- survey_performance[survey_performance$Course=="Course 2",]
full_course_data <- survey_performance #rbind(course1_data,course2_data)

# Comparing performance between Courses
t.test(course1_data$Exam,course2_data$Exam)
boxplot(course1_data$Exam,course2_data$Exam)

# Gender
# Course 1
t.test(course1_data$Exam[course1_data$Gender=="Female"],
       course1_data$Exam[course1_data$Gender=="Male"])

# Course 2
t.test(course2_data$Exam[course2_data$Gender=="Female"],
       course2_data$Exam[course2_data$Gender=="Male"])

# Both Courses
t.test(full_course_data$Exam[full_course_data$Gender=="Female"],
       full_course_data$Exam[full_course_data$Gender=="Male"])


# Nationality ANOVA
# Course 1
summary(aov(Exam~Nationality,data=course1_data))

# Course 2
summary(aov(Exam~Nationality,data=course2_data))

# Both Courses
summary(aov(Exam~Nationality,data=full_course_data))


# Correlation coefficient CI for performance vs SE
# Course 1
cor.test(course1_data$Exam,course1_data$SE.mean)

# Course 2
cor.test(course2_data$Exam,course2_data$SE.mean)

# Both Courses
cor.test(full_course_data$Exam,full_course_data$SE.mean)


# Correlation coefficient CI for performance vs SA
# Course 1
cor.test(course1_data$Exam,course1_data$SA.mean)

# Course 2
cor.test(course2_data$Exam,course2_data$SA.mean)

# Both Courses
cor.test(full_course_data$Exam,full_course_data$SA.mean)


##          ##
# Exam Model #
##          ##

# Course 1 model 

# Fit Full linear model
# For stepAIC to work, need to remove missing values 
model1_data <- na.omit(course1_data[,c("SA.mean","Gender","Age","Nationality",
                                       "Highest.Qual","Course","SE.mean","Specialist","Exam")])

model_Exam_Course1 <- lm(Exam~Gender+Age+Nationality
                         +Highest.Qual+SE.mean+Specialist+SA.mean,data=model1_data)

# Run stepwise regression using AIC
step_model_Course1 <- stepAIC(model_Exam_Course1,direction='both',trace=T)

# Check assumptions 
par(mfrow=c(2,2))
plot(step_model_Course1)

summary(step_model_Course1)
confint(step_model_Course1)

# Course 2 model 

# Fit Full linear model
# For stepAIC to work, need to remove missing values 
model2_data <- na.omit(course2_data[,c("SA.mean","Gender","Age","Nationality","Year.of.Study",
                                       "Highest.Qual","SE.mean","Exam")])

model_Exam_Course2 <- lm(Exam~Gender+Age+Nationality+Year.of.Study
                         +Highest.Qual+SE.mean+SA.mean,data=model2_data)

# Run stepwise regression using AIC
step_model_Course2 <- stepAIC(model_Exam_Course2,direction='both',trace=T)

# Check assumptions 
par(mfrow=c(2,2))
plot(step_model_Course2)

summary(step_model_Course2)
confint(step_model_Course2)

# Check correlation bewteen S.Eff and S.Anx...
cor.test(full_course_data$SA.mean, full_course_data$SE.mean) # -0.5642336 
cor.test(course1_data$SA.mean, course1_data$SE.mean) # -0.5513009 
cor.test(course2_data$SA.mean, course2_data$SE.mean) # -0.6144155 


# Both course models 
model_data <- na.omit(full_course_data[,c("SA.mean","Gender","Age","Nationality","Year.of.Study",
                                          "Exam","Course","Specialist")])

# MB Removed: "Highest.Qual","Voluntary", "
model_data$Year.of.Study <- droplevels(model_data$Year.of.Study)


#table(model_data$Year.of.Study, model_data$Course)
#              Course 1 Course 2
# Year 1            113        3
# Year 2             13        0
# Year 3              5       23
# Masters year        0       16


model_Exam_Course_Full <- lm(Exam~Gender+Age+Nationality+Year.of.Study
                             +SA.mean+Course+Specialist,data=model_data)

# Run stepwise regression using AIC
step_model_Course_Full <- stepAIC(model_Exam_Course_Full,direction='both',trace=T)

# Check assumptions 
par(mfrow=c(2,2))
plot(step_model_Course_Full)

summary(step_model_Course_Full)
confint(step_model_Course_Full)


# MB Removed: "Year.of.Study" - see table above
model_data <- na.omit(full_course_data[,c("SA.mean","Gender","Age","Nationality",
                                          "Exam","Course","Specialist")])

model_Exam_Course_Full_SA <- lm(Exam~Gender+Age+Nationality
                                +SA.mean+Course+Specialist,data=model_data)

# Run stepwise regression using AIC
step_model_Course_Full_SA <- stepAIC(model_Exam_Course_Full_SA,direction='both',trace=T)

# Check assumptions
par(mfrow=c(2,2))
plot(step_model_Course_Full_SA)

summary(step_model_Course_Full_SA)
confint(step_model_Course_Full_SA)


# Self efficacy model
model_data <- na.omit(full_course_data[,c("SE.mean","Gender","Age","Nationality",
                                          "Exam","Course","Specialist")])

model_Exam_Course_Full_SE <- lm(Exam~Gender+Age+Nationality
                                +SE.mean+Course+Specialist,data=model_data)

# Run stepwise regression using AIC
step_model_Course_Full_SE <- stepAIC(model_Exam_Course_Full_SE,direction='both',trace=T)

# Check assumptions
par(mfrow=c(2,2))
plot(step_model_Course_Full_SE)

summary(step_model_Course_Full_SE)
confint(step_model_Course_Full_SE)




##############################################################################
# Analysis for MBs MEd Assignment - IGNORE BELOW HERE
table(survey_data$Nationality)
round(prop.table(table(survey_data$Nationality)),3)

table(survey_data$Voluntary)
round(prop.table(table(survey_data$Voluntary)),3)

summary(survey_data$SA.mean)
sqrt(var(survey_data$SA.mean))

table(survey_data$Nationality,survey_data$Voluntary)
table(survey_data$Nationality,survey_data$Specialist)


library(rockchalk)
Nationality <- combineLevels(survey_data$Nationality, levs = c("Chinese","Other"), newLabel = "Other World")

Qdata <- data.frame(Nationality=Nationality, 
                    Voluntary = survey_data$Voluntary)
Qdata <- Qdata[!is.na(Qdata[,1]),]
Qdata <- Qdata[!is.na(Qdata[,2]),]

g <- ggplot(Qdata, aes(Nationality))
g + geom_bar(aes(fill=Voluntary), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  theme(legend.title = element_blank())


g <- ggplot(Qdata, aes(Voluntary))
g + geom_bar(aes(fill=Nationality), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  theme(legend.title = element_blank())

chisq1a <- chisq.test(survey_data$Nationality,survey_data$Voluntary)
chisq1a$expected

chisq1b <- chisq.test(Nationality,survey_data$Voluntary)
chisq1b$expected

chisq2 <- chisq.test(survey_data$Nationality,survey_data$Specialist)
chisq2$expected

