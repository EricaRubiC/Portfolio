# Codebook

# Libraries
library(tidyverse)
library(haven)
library(readxl)
install.packages("writexl")
library(writexl)

# Downloading IOM Data
Sudan_IOM <- read_excel("~/Downloads/Sudan Crisis - Returnee Reintegration and Perceptions Survey_Areas of Return _7-14Mar_2024.xlsx")
View(Sudan_IOM)

# Selecting desired variables, and cutting down the dataset

df_Sudan <- Sudan_IOM %>% select(
  3:6,      
  10:12,    
  14:15,     
  21:33,    
  64:65,
  113,
  142,
  190:191,
  202,
  212,
  232,
  235:237,
  252,
  259,
  270,
  281,
  292
)

# Cutting some variables out

df_Sudan <- df_Sudan %>% select(-c(2, 15:18))

# Reviewing Data
summary(df_Sudan)
colnames(df_Sudan)
lapply(df_Sudan, unique)

# Cleaning Data

# Change variable names to more tidy (no spaces)

colnames(df_Sudan) <- c(
  "admin1_pcode",
  "admin2_pcode",
  "month_registered_iom_ota",
  "age_respondent",
  "gender_respondent",
  "education_level_respondent",
  "boys_under_18",
  "girls_under_18",
  "origin_state_ss",
  "origin_county_ss",
  "origin_payam_ss",
  "residence_state_sudan",
  "residence_county_sudan",
  "current_state_ss",
  "current_county_ss",
  "current_payam_ss",
  "reason_current_location",
  "meals_per_day",
  "distance_to_market",
  "num_children",
  "livelihood_challenges_ss",
  "safety_return_ss",
  "reasons_unsafe",
  "risks_children",
  "risks_women_girls",
  "water_access",
  "children_school_sudan",
  "children_current_school",
  "reason_not_all_in_school",
  "received_support",
  "support_government",
  "support_un_ngo",
  "support_community",
  "support_other"
)

df_Sudan <- df_Sudan %>% select(-c(4, 17, 21, 30:34))

# Check and convert data types

df_Sudan$boys_under_18  <- as.numeric(df_Sudan$boys_under_18)
df_Sudan$girls_under_18 <- as.numeric(df_Sudan$girls_under_18)
df_Sudan$num_children   <- as.numeric(df_Sudan$num_children)

#Fixing gender variable
df_Sudan <- df_Sudan %>% 
  mutate(gender_respondent = recode(gender_respondent, 
                         "Male" = 0, 
                         "Female" = 1, 
                         .default = NA_real_))

#Fixing education variable
df_Sudan <- df_Sudan %>%
  mutate(education = case_when(
    education_level_respondent == "No formal schooling" ~ 0,
    education_level_respondent == "Started but did not complete primary school" ~ 1,
    education_level_respondent == "Finished primary school, but did not start secondary school" ~ 2,
    education_level_respondent == "Started but did not complete secondary school" ~ 3,
    education_level_respondent == "Finished secondary school" ~ 4,
    education_level_respondent == "University" ~ 5,
    TRUE ~ NA_real_  
  ))

# Fixing Meals Per Day

df_Sudan <- df_Sudan %>%
  mutate(meals_daily = case_when(
    meals_per_day == "I choose not to answer" ~ NA,
    meals_per_day == "Less than one meal per day" ~ 0,
    meals_per_day == "One" ~ 1,
    meals_per_day == "Two" ~ 2,
    meals_per_day == "Three or more" ~ 3,
  ))

df_Sudan$meals_daily <- factor(df_Sudan$meals_daily)

#Fixing Distance to Nearest Market

df_Sudan <- df_Sudan %>%
  mutate(distance_market = case_when(
    distance_to_market == "0-15 minutes" ~ 1,
    distance_to_market == "15-30 minutes" ~ 2,
    distance_to_market == "30-45 minutes" ~ 3,
    distance_to_market == "45 min-1 hour" ~ 4,
    distance_to_market == "1-2 hours" ~ 5,
    distance_to_market == "more than 2 hours" ~ 6,
    TRUE ~ NA_real_
  ))

df_Sudan$distance_market <- factor(df_Sudan$distance_market)

#Fixing Safety Upon Return

df_Sudan <- df_Sudan %>%
  mutate(safety_ss = case_when(
    safety_return_ss == "I choose not to answer" ~ NA,
    safety_return_ss == "Very unsafe" ~ 1,
    safety_return_ss == "Unsafe" ~ 2,
    safety_return_ss == "Neither safe nor unsafe" ~ 3,
    safety_return_ss == "Somewhat safe" ~ 4,
    safety_return_ss == "Very safe" ~ 5,
  ))

df_Sudan$safety_ss <- factor(df_Sudan$safety_ss)

#Fixing Why Unsafe

# - Keeping it as a character and tidying the data

df_Sudan <- df_Sudan %>%
  mutate(reasons_unsafe = case_when(
    reasons_unsafe == "a) Due to localized gang violence" ~ "Due to localized gang violence",
    reasons_unsafe == "b) Due to conflict among armed groups" ~ "Due to conflict among armed groups",
    reasons_unsafe == "d) Due to risks of gender-based violence e)  Due to discrimination" ~ "Due to gender-based violence and discrimination",
    reasons_unsafe == "e)  Due to discrimination a) Due to cattle-related violence" ~ "Due to discrimination and cattle-related violence",
    reasons_unsafe == "b) Due to conflict among armed groups a) Due to localized gang violence d) Due to risks of gender-based violence" ~ 
      "Due to conflict, gang violence, and gender-based violence",
    TRUE ~ "NA"
  ))

#Fixing risk_children

df_Sudan <- df_Sudan %>%
  mutate(
    Abduction_Trafficking = ifelse(str_detect(risks_children, "Abduction/Trafficking"), 1, 0),
    Abandonment = ifelse(str_detect(risks_children, "Abandonment"), 1, 0),
    Forced_Recruitment = ifelse(str_detect(risks_children, "Forced recruitment"), 1, 0),
    Exploitation = ifelse(str_detect(risks_children, "Exploitation"), 1, 0),
    GBV = ifelse(str_detect(risks_children, "GBV"), 1, 0),
    Other = ifelse(str_detect(risks_children, "Other"), 1, 0),
    None = ifelse(str_detect(risks_children, "None"), 1, 0),
    No_Answer = ifelse(str_detect(risks_children, "I choose not to answer"), 1, 0)
  )

# Fixing Protection Risks for Women and Girls
df_Sudan <- df_Sudan %>%
  mutate(
    Abduction_Trafficking_women = ifelse(str_detect(risks_women_girls, "Abduction/Trafficking"), 1, 0),
    Abandonment_women = ifelse(str_detect(risks_women_girls, "Abandonment"), 1, 0),
    Forced_Recruitment_women = ifelse(str_detect(risks_women_girls, "Forced recruitment"), 1, 0),
    Exploitation_women = ifelse(str_detect(risks_women_girls, "Exploitation"), 1, 0),
    GBV_women = ifelse(str_detect(risks_women_girls, "GBV"), 1, 0),
    Other_women = ifelse(str_detect(risks_women_girls, "Other"), 1, 0),
    None_women = ifelse(str_detect(risks_women_girls, "None"), 1, 0),
    No_Answer_women = ifelse(str_detect(risks_women_girls, "I choose not to answer"), 1, 0)
  )

# Re-coding Water Access

df_Sudan$water_access_num <- recode(df_Sudan$water_access,
                                    "Yes" = 1,
                                    "No" = 2,
                                    "Sometimes/inconsistently" = 3)


# Re-coding Children in school in Sudan

df_Sudan <- df_Sudan %>%
  mutate(child_school_sudan = recode(children_school_sudan,
                                      "Yes" = 1,
                                      "No" = 2,
                                      "Some of them, but not all" = 3,
                                      "I don’t have school-aged children" = 4))

# Re-coding Children currently in School

df_Sudan <- df_Sudan %>%
  mutate(child_current_school = recode(children_current_school,
                                              "Yes" = 1,
                                              "No" = 2,
                                              "Some of them, but not all" = 3,
                                              "I don’t have school-aged children" = 4))


# Re-coding Reason Not all in School

df_Sudan <- df_Sudan %>%
  mutate(
    Reason_Fees = ifelse(str_detect(reason_not_all_in_school, "a\\)"), 1, 0),
    Reason_Distance = ifelse(str_detect(reason_not_all_in_school, "b\\)"), 1, 0),
    Reason_Work = ifelse(str_detect(reason_not_all_in_school, "e\\)"), 1, 0),
    Reason_Other = ifelse(str_detect(reason_not_all_in_school, "f\\)"), 1, 0),
    Reason_NoAnswer = ifelse(str_detect(reason_not_all_in_school, "g\\)"), 1, 0)
  )

#Cutting out unnecessary variables and re-ordering variables

df_Sudan <- df_Sudan %>%
  select(
    admin1_pcode,
    admin2_pcode,
    month_registered_iom_ota,
    gender_respondent,
    education,                       
    origin_state_ss,
    origin_county_ss,
    origin_payam_ss,
    residence_state_sudan,
    residence_county_sudan,
    current_state_ss,
    current_county_ss,
    current_payam_ss,
    distance_market,               
    meals_daily,                   
    water_access_num,               
    num_children,
    boys_under_18,
    girls_under_18,
    safety_ss,                       
    reasons_unsafe,
    child_school_sudan,           
    risks_children,
    Abduction_Trafficking,
    Abandonment,
    Forced_Recruitment,
    Exploitation,
    GBV,
    Other,
    None,
    No_Answer,
    child_current_school,
    reason_not_all_in_school,
    Reason_Fees,
    Reason_Distance,
    Reason_Work,
    Reason_Other,
    Reason_NoAnswer,
    risks_women_girls,
    Abduction_Trafficking_women,
    Abandonment_women,
    Forced_Recruitment_women,
    Exploitation_women,
    GBV_women,
    Other_women,
    None_women,
    No_Answer_women
  )


# Checking
str(df_Sudan)
summary(df_Sudan)
colnames(df_Sudan)

# Making the Final Data Set df_Sudan_IOM

readr::write_csv(df_Sudan, "df_Sudan.csv")

