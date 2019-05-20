getwd()
# Importing Environment Pollution dataset

environment_problem_df <- read.csv("D:/data science/New folder/gases.csv", header = TRUE)
str(environment_problem_df)

# Using dplyr Package

library(dplyr)

# Data cleaning is performed and all NA's are removed from are dataset 

environment_problem_df <- na.omit(environment_problem_df)

# Checking structure of dataframe 

str(environment_problem_df)

#  Selecting three major columns i.e Gases, year when calculated and
#Total emiision in the same year

environment_problem_df <- environment_problem_df %>%
  select(Gases, year, Total.emissions)

# Grouping by the year and calculating total emmision by all the four gases 

environment_problem_df <- data.frame(Groupn= rep(c(environment_problem_df$year)),
                                     Total.emision = c(environment_problem_df$Total.emissions))
str(environment_problem_df)


###############################################################################################

# Importing rainfall dataset

rainfall_df <- read.csv("rainfall_new.csv")
str(rainfall_df)

# Extracting specific columns as per the need 
# In our case it is yearand the reading recorded by  The Belmullet observatory

rainfall_df <- subset(rainfall_df, select = c(year ,Belmullet ,Valentia.Observatory))

# Further cleaning the dataset by removing NA's

rainfall_df <- na.omit(rainfall_df)

# In our dataset there are multiple entries of the same year 
# Hence, Grouping them together and taking mean of all the values will certainly 
# make our hypothesis more accurate

rainfall_df$year <- sub("^(\\d{4}).*$", "\\1", rainfall_df$year)

rainfall_df <- rainfall_df %>% group_by(year) %>% summarize(Belmullet = mean(Belmullet))

str(rainfall_df)

#################################################################################################

# merging two data sets

rainfall_and_environment_df <- merge(environment_problem_df, rainfall_df, by.x = "Groupn", by.y = "year")

rainfall_and_environment_df <- aggregate(rainfall_and_environment_df[, 2:3], list(rainfall_and_environment_df$Groupn), mean)


# Changing columns name to more appropriate name

names(rainfall_and_environment_df)[1]<-paste("Year")
names(rainfall_and_environment_df)[2]<-paste("Total_Emission")
names(rainfall_and_environment_df)[3]<-paste("Rainfall_Readings")

# Groupimg the dataframe by Total emmission and 
# rainfall reading for a logical comparison between two
rainfall_and_environment_df$Condition <- ifelse(rainfall_and_environment_df$Total_Emission >25000, "high", "low")



#########################################################################################
#plotting histogram
library(lattice)
histogram(~Rainfall_Readings | Condition, data = rainfall_and_environment_df)
str(rainfall_and_environment_df)

str(rainfall_and_environment_df)


##########################################################################################
#plotting qq plot2 
install.packages("ggplot2")
library(ggplot2)

#using qqplot

with(rainfall_and_environment_df,
     qqplot(Rainfall_Readings[Condition == "high"],
            Rainfall_Readings[Condition == "low"], 
            main = "Comparing 2 samples", 
            xlab = "Higher Total Emission = Yes",
            ylab =  "Higher Total Emission = NO"))

# Using a QQ plot to check for normality

with(rainfall_and_environment_df, {
  qqnorm(Rainfall_Readings[Condition == "low"], 
         main = "Low")
})

# to  add normailty line and evalute 

with(rainfall_and_environment_df, {
  qqnorm(Rainfall_Readings[Condition == "low"], 
         main = "Low")
  qqline(Rainfall_Readings[Condition == "low"])
})


#############################################################################################

#installing ggpubr library 
install.packages("ggpubr")
library(ggpubr)

str(rainfall_and_environment_df)

#ploting a boxplot to see comparsion
ggboxplot(rainfall_and_environment_df, x = "Condition", y = "Rainfall_Readings",
          palette = c("#FFFF00", "#00FF00"),
          ylab = "Reading", xlab = "Pollution")


#############################################################################################

# Checking Normality

shapiro.test(rainfall_and_environment_df$Rainfall_Readings)

shapiro.test(rainfall_and_environment_df$Total_Emission)

#applying wilcox test
wilcox.test(rainfall_and_environment_df$Total_Emission)
wilcox.test(rainfall_and_environment_df$Rainfall_Readings)


# applying two sample t-test
rainfall.and.environment.t.test <- t.test(Rainfall_Readings ~ Condition, data = rainfall_and_environment_df )

rainfall.and.environment.t.test

rainfall.and.environment.t.test$p.value

# group means
rainfall.and.environment.t.test$estimate 

# confidence level
rainfall.and.environment.t.test$conf.int
attr(rainfall.and.environment.t.test$conf.int, "conf.level")

#calculating mean and sd 
mean1 <- mean(rainfall_and_environment_df$Total_Emission)
mean1
mean2 <- mean(rainfall_and_environment_df$Rainfall_Readings)
mean2
sd1 <- sd(rainfall_and_environment_df$Total_Emission)
sd1
sd2 <- sd(rainfall_and_environment_df$Rainfall_Readings)
sd2
avg = (sd1 + sd2)/2
delta1 = (mean1 - mean2)/avg
delta1

#calculating n sample 
library(pwr)
power.t.test(delta = 22.64, n = NULL, sig.level = 0.05, power = 0.90,type= "two.sample", alternative = "two.sided")



