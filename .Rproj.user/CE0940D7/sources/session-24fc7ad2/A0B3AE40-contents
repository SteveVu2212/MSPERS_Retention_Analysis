library("readxl")
library(tidyverse)
library(zoo)
library(profvis)
library(ggplot2)
library(data.table)
library(openxlsx)
library(data.table)

# Model's inputs
input_file <- "separation_rate_2022.xlsx"

init_male_separation_rate <- read_excel(input_file, sheet = "Separation_Rate_Male")
init_female_separation_rate <- read_excel(input_file, sheet = "Separation_Rate_Female")

init_retirement_rate_tier123 <- read_excel(input_file, sheet="Retirement_Rate_Tier123")
init_retirement_rate_tier4 <- read_excel(input_file, sheet="Retirement_Rate_Tier4")

salary_entry_2022 <- read_excel(input_file, sheet = "Start Salary") #change the 1st entry age from 19 to 20 to match with separation rate tables
headcount_matrix <- read_excel(input_file, sheet = "HeadCount Matrix")

# Model assumptions
model_period <- 30    #Projection period (typically 30 years)
min_age <- 20          #Age of the typical youngest member
max_age <- 120         #Max age from mortality assumptions
year_start <- 2022     #Year of the latest val report
min_year <- 1990       # Current year - max(yos)
max_year <- year_start + model_period + max_age - min_age

### Users' inputs
start_hist_year <- 2021
start_proj_year <- 2023
end_proj_year <- 2052
critical_year <- 2047

entry_year_range <- min_year:(year_start + model_period)
year_range <- min_year:max_year
age_range <- min_age:max_age
yos_range <- 0:70
retirement_age_range <- min_age:max_age

headcount <- headcount_matrix %>%
  pivot_longer(cols = -1, names_to = "yos", values_to = "count") %>%
  replace(is.na(.), 0) %>%
  mutate(
    yos = as.numeric(yos),
    current_year = year_start,
    entry_age = age - yos,
    entry_year = current_year - yos
  ) %>%
  filter(entry_age >= 18)

salary_entry <- headcount %>%
  filter(yos == 2) %>%
  left_join(salary_entry_2022, by="age") %>%
  mutate(entrant_dist = count / sum(count)) %>%
  rename(start_salary = salary) %>%
  select(entry_age, current_year = current_year.x, start_salary, count, entrant_dist)

all_entry_age <- as.vector(salary_entry$entry_age)

#Determine retirement eligibility
# Look at page 25 in the member handbook
check_normal_retirement_eligibility <- function(age, yos, entry_year){
  check_ <- ifelse(
    (entry_year <= 2007 & yos >= 25) |
      (entry_year <= 2007 & age >= 60 & yos >= 4) |
      (entry_year > 2007 & entry_year < 2011 & yos >= 25) |
      (entry_year > 2007 & entry_year < 2011 & age >= 60 & yos >= 8) |
      (entry_year >= 2011 & yos >= 30) |
      (entry_year >= 2011 & age >= 65 & yos >= 8),
    TRUE, FALSE)
  return(check_)
}

check_early_retirement_eligibility <- function(age, yos, entry_year){
  check_ <- ifelse(
    (entry_year >= 2011 & yos < 30 & yos >= 8 & age < 65 & age >= 60), # YOS < 30 & Age < 65
    TRUE, FALSE)
  return(check_)
}


check_retirement_eligibility <- function(age, yos, entry_year){
  check_ <- ifelse(
    check_normal_retirement_eligibility(age, yos, entry_year) == T | 
      check_early_retirement_eligibility(age, yos, entry_year) == T,
    TRUE, FALSE)
  return(check_)
}


#Determine retirement type
get_retirement_type <- function(age, yos, entry_year){
  retirement_type = ifelse(check_normal_retirement_eligibility(age, yos, entry_year) == T, "normal",
                           ifelse(check_early_retirement_eligibility(age, yos, entry_year) == T, "early", "none"))
  return(retirement_type)
}

#Separation Rates (only apply to active members)
#Because separation rates are subject to retirement eligibility, which depends on entry years (tiers), the separation table needs to take entry years into account
final_male_separation_rate <- init_male_separation_rate %>%
  pivot_longer(cols=-1, names_to = "yos", values_to = "male_separation_rate") %>%
  mutate(
    age = as.numeric(age),
    yos = as.numeric(yos),
    entry_age = age - yos
  ) %>%
  filter(entry_age %in% all_entry_age) %>%
  select(age, yos, male_separation_rate)

final_female_separation_rate <- init_female_separation_rate %>%
  pivot_longer(cols=-1, names_to = "yos", values_to = "female_separation_rate") %>%
  mutate(
    age = as.numeric(age),
    yos = as.numeric(yos),
    entry_age = age - yos
  ) %>%
  filter(entry_age %in% all_entry_age) %>%
  select(age, yos, female_separation_rate)

########################## Initial separation rate #############################
separation_rate <- expand_grid(age=age_range, yos=yos_range, entry_year=entry_year_range) %>%
  mutate(entry_age = age - yos) %>%
  filter(entry_age %in% all_entry_age) %>%
  arrange(entry_year, entry_age, age) %>%
  group_by(entry_year, entry_age) %>%
  left_join(final_male_separation_rate, by=c("age", "yos")) %>%
  left_join(final_female_separation_rate, by=c("age", "yos")) %>%
  left_join(init_retirement_rate_tier4, by="age") %>%
  left_join(init_retirement_rate_tier123, by="age") %>%
  replace(is.na(.), 0) %>%
  mutate(
    is_retirement = check_retirement_eligibility(age, yos, entry_year),
    
    male_separation_rate_ = ifelse(is_retirement == T & entry_year < 2011 & yos < 25, male_under_25yos,
                                   ifelse(is_retirement == T & entry_year < 2011 & yos >= 25, male_over_25yos,
                                          ifelse(is_retirement == T & entry_year >= 2011 & yos < 30, male_under_30yos,
                                                 ifelse(is_retirement == T & entry_year >= 2011 & yos >= 30, male_over_30yos,
                                                        male_separation_rate)))),
    
    female_separation_rate_ = ifelse(is_retirement == T & entry_year < 2011 & yos < 25, female_under_25yos,
                                     ifelse(is_retirement == T & entry_year < 2011 & yos >= 25, female_over_25yos,
                                            ifelse(is_retirement == T & entry_year >= 2011 & yos < 30, female_under_30yos,
                                                   ifelse(is_retirement == T & entry_year >= 2011 & yos >= 30, female_over_30yos,
                                                          female_separation_rate)))),
    
    sep_rate = (male_separation_rate_ + female_separation_rate_)/2,
    
    remaining_prob = cumprod(1 - lag(sep_rate, default = 0)),
    
    separation_prob = lag(remaining_prob, default = 1) - remaining_prob
    ) %>%
  ungroup() %>%
  select(entry_year, entry_age, age, yos, remaining_prob)

# Outputs
output <- separation_rate %>% 
  filter(entry_age == 30, entry_year == 2023)


write.xlsx(output, file="retention_data.xlsx")





