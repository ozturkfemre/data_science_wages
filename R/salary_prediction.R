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

