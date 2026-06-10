#Load dataset
library(nlme) 
data(Theoph) 
head(Theoph) 
str(Theoph) 
summary(Theoph)

#Dimensions
dim(Theoph) 
nrow(Theoph) 
ncol(Theoph)

#Interpretation:There are 132 observations with 5 variables

#Datatypes
sapply(Theoph, class)

#Missing values
colSums(is.na(Theoph))# this is no null values 4

#Duplicates 
sum(duplicated(Theoph))#No duplicates

#Descriptive Statistics
summary(Theoph) 
mean(Theoph$conc) 
median(Theoph$conc) 
sd(Theoph$conc) 
min(Theoph$conc) 
max(Theoph$conc)

summary(Theoph$conc)

min(Theoph$conc)

sum(is.na(Theoph$conc))

sum(Theoph$conc <= 0)
#Histogram
hist(Theoph$conc,
     main = "Distribution of Theophylline Concentration",
     xlab = "Concentration (mg/L)",
     col = "lightblue")

#Boxplot
boxplot(Theoph$conc, main = "Boxplot of Concentration", 
        ylab = "Concentration")

#Normality Check 
qqnorm(Theoph$conc)
qqline(Theoph$conc, col = "red")

#Shapiro-Wilk Test
shapiro.test(Theoph$conc)

#Log Transformation
log_conc <- log(Theoph$conc)

summary(log_conc)

sum(is.na(log_conc))

sum(is.infinite(log_conc))

hist(log_conc, main = "Histogram of Log Concentration") 


log_conc <- log_conc[is.finite(log_conc)]

qqnorm(log_conc)
qqline(log_conc, col = "red")

shapiro.test(log_conc)
qqnorm(log_conc) 
qqline(log_conc, col = "red") 

shapiro.test(log_conc)

#Concentration vs Time
plot(Theoph$Time, Theoph$conc, 
     xlab = "Time (hours)", 
     ylab = "Concentration (mg/L)", 
     main = "Concentration vs Time")

#Concentration-Time Profiles
library(ggplot2) 
ggplot(Theoph, aes(x = Time, y = conc, 
                   colour = Subject, 
                   group = Subject)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(title = "Concentration-Time Profiles")

#Subject-wise Profiles
ggplot(Theoph, aes(Time, conc)) + 
  geom_line() + 
  facet_wrap(~Subject)

#Weight vs Concentration
ggplot(Theoph, aes(Wt, conc)) + 
  geom_point() + 
  geom_smooth(method = "lm")

#Correlation
cor(Theoph$Wt, Theoph$conc)

#Maximum Concentration
aggregate(conc~Subject,data=Theoph,max)

#Time of Maximum Concentration
library(dplyr)
Theoph%>%
  group_by(Subject)%>%
  slice_max(conc)

#Area under Curve
install.packages("pracma")
library(pracma)
library(dplyr)
AUC_results=Theoph%>%
  group_by(Subject)%>%
  summarise(AUC=trapz(Time,conc))
AUC_results

#Plot AUC
ggplot(AUC_results, 
       aes(Subject, AUC)) + 
  geom_bar(stat = "identity") + 
  theme_minimal()

#Log Concentration vs Time
ggplot(Theoph,aes(Time,aes(Time,log(conc),colour=Subject))+
         geom_line()+
         theme_minimal())

#Simple Regression(for demonstratio purpose)
model=lm(conc~Time,data=Theoph)
summary(model)



#Mixed effects Model
library(nlme) 
mixed_model <- lme( conc ~ Time, 
                    random = ~1 | Subject, 
                    data = Theoph ) 
summary(mixed_model)
