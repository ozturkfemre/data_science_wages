#################
### Libraries ###
#################

library(corrplot)
library(nortest)
library(ISLR)
library(Hmisc)
library(caret)
library(dplyr)
library(ModelMetrics)
library(lmtest)
library(car)
library(olsrr)
library(tidyverse)
library(moments)
library(bestNormalize)
library(magrittr)

###################
### Data Import ###
###################


library(readr)
ds_salaries <- read_csv("dataset/ds_salaries.csv", 
                        col_types = cols(...1 = col_skip()))


ds_salaries

#####################
### Data Cleaning ###
#####################


library(tidyverse)
library(magrittr)

colnames(ds_salaries)

# I'll remove unnecessary columns like work_year, salary and salary currency. I will continue on usd based salaries.  

df <- ds_salaries %>% select("experience_level",   
                        "employment_type",
                        "job_title",   
                        "salary_in_usd",      
                        "employee_residence", 
                        "remote_ratio",
                        "company_location",
                        "company_size")  

df


# lets change data types into correct types
str(df)


df$experience_level <- as.factor(df$experience_level)
unique(df$experience_level)

df$employment_type <- as.factor(df$employment_type)
unique(df$employment_type)

df$job_title <- as.factor(df$job_title)
unique(df$job_title)

# Since there are too many titles, i will reduce them in some categories

job_categories <- c("Data Science", "Data Analytics", "Data Engineering", "Machine Learning", "Managerial", "Consultant")

data_science <- "Data Scientist|NLP"
data_analyst <- "Analyst|Analytics"
data_engineer <- "Data Engineer|ETL|Architect|Infrastructure"
ml_engineer <- "Machine Learning|ML|Big Data|AI"
manager <- "Manager|Head|Director|Lead|Principal|Staff"
consultant <- "Consultant|Freelance"

df <- df %>%
  mutate(
    job_category = case_when(
      str_detect(job_title, data_science) ~ "Data Science",
      str_detect(job_title, data_analyst) ~ "Data Analytics",
      str_detect(job_title, data_engineer) ~ "Data Engineering",
      str_detect(job_title, ml_engineer) ~ "Machine Learning",
      str_detect(job_title, manager) ~ "Managerial",
      str_detect(job_title, consultant) ~ "Consultant",
      TRUE ~ "Other"
    )
  )


unique(df$job_category)

# Now that we have categories, we can drop titles.
df <- df[-3]


df$employee_residence <- as.factor(df$employee_residence)
# Although we have lots of levels in here, we cannot reduce them. Because this will be beneficial infromation for our model.


boxplot(df$salary_in_usd) # we have lots of outliers that can be problematic for regression algorihms. Yet, we cannot remove them.
boxplot(df$remote_ratio) # this is numeric but behaves like categorical. thus i will convert it into factor

df$remote_ratio <- as.factor(df$remote_ratio)


df$company_location <- as.factor(df$company_location)
# Although we have lots of levels in here, we cannot reduce them. Because this will be beneficial infromation for our model.

df$company_size <- as.factor(df$company_size)


df$job_category <- as.factor(df$job_category)


# Now each variable is become what we want it to be. 

##########################
### Data Visualization ###
##########################
df$
# experience level 

ggplot(df, aes(x = experience_level)) +
  geom_bar() +
  labs(title = "Experience levels of people", subtitle = "Most of the people in the dataset are at senior experience level.")+
  xlab("Experience Level") +
  ylab("") +
  theme_minimal()
  

# employment type

ggplot(df, aes(x = employment_type)) +
  geom_bar() +
  labs(title = "Employment type of people", subtitle = "Almost all of the people's employment type in the dataset are full time.")+
  xlab("Employment Type") +
  ylab("") +
  theme_minimal()



# experience level X employment type

ggplot(df, aes(x = experience_level, fill = employment_type)) +
  geom_bar(position = "stack") +
  labs(title = "Employment Type Distribution within Experience Levels", subtitle = "Majority of all experience level is full time.", fill = "Employment Type")+
  xlab("Experience level") +
  ylab("") +
  theme_minimal()+
  theme(legend.position = "bottom")

# salary 

ggplot(df, aes(x = log(salary_in_usd))) +
  geom_histogram(binwidth = 0.09) +
  labs(title = "Salary Distribution in USD", subtitle = "Most of the people earns money between 10k and 12k.")+
  theme_minimal()+
  labs(title = "Salary Distribution") +
  ylab( "") +
  xlab("Salary in usd 8=8000")


# salary x employment type

ggplot(df, aes(x = employment_type, y = log(salary_in_usd))) +
  geom_boxplot() +
  labs(title = "Salary Distribution by Employment Type") +
  theme_minimal()


# salary x employment type

ggplot(df, aes(x = employment_type, y = salary_in_usd)) +
  geom_violin() +
  labs(title = "Salary Distribution by Employment Type (Violin Plot)") +
  theme_minimal()


# company size

ggplot(df, aes(x = "", fill = company_size)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Proportion of Company Sizes")+
  xlab("") +
  ylab("") +
  theme_minimal()+
  theme(legend.position = "bottom")


################################################################################
######################### Machine Learning Models ##############################
################################################################################

##########################
### Train - Test Split ###
##########################

df <- df[-c(4,6)]
str(df)

smp_size <- floor(0.75 * nrow(df)) 
set.seed(2021900444) 
train_ind <- sample(nrow(df), size = smp_size, replace = FALSE)
train <- df[train_ind, ]
test <- df[-train_ind, ]

#########################
### Linear Regression ###
#########################

model1 <- lm(salary_in_usd ~ experience_level + employment_type + remote_ratio + company_size + job_category, data = train)
summary(model1) # some variables are not significant

########################
### Assumption Check ###
########################


vif(model1) # no collinearity problem 


# normality of residuals 

fun <- dnorm(model1$residuals, mean = mean(model1$residuals), sd = sd(model1$residuals))

hist(model1$residuals, ylab = "Density", xlab = "Residuals", col="burlywood2",
     border="burlywood4", probability = T, ylim = c(0, max(fun)))


lines(density(model1$residuals), col = 9, lwd = 2) # does not seem like normal

shapiro.test(model1$residuals)


# Homoscedasticity

bptest(salary_in_usd ~ experience_level + employment_type + remote_ratio + company_size + job_category, data = train)



par(mfrow = c(2,2))
plot(model1)

# errors
predictions1 <- predict(model1,test)
mae1 <- mae(predictions1, test$salary_in_usd)


################################################################################

#######################
### Regression Tree ###
#######################

