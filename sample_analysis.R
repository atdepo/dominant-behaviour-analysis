library(readxl)
library(dplyr)
library(networkD3)

column_types <- c("date", "text", "text","text", "text", "text", "numeric", "text", "numeric", "text", "text","text", "text")
column_names <- c("Timestamp_Risposta","Prolific_ID","Gender","Nationality","Role","Company_Size","Managing_Distributed_Teams_Familiarities","Years_of_experience_Managing","Evaluation_Team_Management","Team_Size","PMI_or_others_Certifications","CS_Familiarities", "CS_Experience")
survey_response <- read_excel("Selection Survey for Community Smells Recognition - Risposte.xlsx",col_types = column_types, col_names = column_names, skip=1)

#### Demographic Analysis ####

sample_familiarity <- survey_response %>%
  group_by(CS_Familiarities) %>%
  summarise(count = n())

sample_familiarity <- mutate(sample_familiarity, percentage = (count / 303) * 100)

nationality_sample <- survey_response %>%
  group_by(Nationality) %>%
  summarise(count = n())

nationality_sample <- arrange(nationality_sample, desc(count))

nationality_sample <- mutate(nationality_sample, percentage = (count / 303) * 100)

print(nationality_sample, n=40)


##### work role distribution#####

work_role <- survey_response %>%
  group_by(Role) %>%
  summarise(count = n())

work_role_counts <- arrange(work_role, desc(count))

work_role_counts <- mutate(work_role_counts, percentage = (count / 303) * 100)


#### Managers statistic ####

survey_response['PMI_or_others_Certifications'] <- survey_response['PMI_or_others_Certifications']  %>%  mutate(PMI_or_others_Certifications = if_else(!PMI_or_others_Certifications %in% c("No","Yes"),"Other Certifications", PMI_or_others_Certifications))

managers <- survey_response %>%
  filter(grepl("Manager", Role, ignore.case = TRUE))

lead <- survey_response %>%
  filter(grepl("Lead", Role, ignore.case = TRUE))

sm <- survey_response %>%
  filter(grepl("Scrum Master", Role, ignore.case = TRUE))

filtered_survey <- bind_rows(managers, lead, sm)

familiarity_manager_tot <- filtered_survey %>%
  group_by(CS_Familiarities) %>%
  summarise(count = n())

familiarity_manager_tot <- mutate(familiarity_manager_tot, percentage = (count / 140) * 100)

###### Non managers statistic#######

other_roles <- survey_response %>%
  anti_join(filtered_survey, by = "Prolific_ID") # The Prolific ID are anonymus in this version of the replication package

familiarity_others_tot <- other_roles %>%
  group_by(CS_Familiarities) %>%
  summarise(count = n())

familiarity_ohters_perc <- other_roles %>%
  group_by(CS_Familiarities) %>%
  summarise(perc = (n()/nrow(other_roles))*100)

fami_yes_others<- other_roles %>%
  filter(grepl("Yes", CS_Familiarities, ignore.case = TRUE))

fami_maybe_others <- other_roles %>%
  filter(grepl("Maybe", CS_Familiarities, ignore.case = TRUE))

fami_no_others <- other_roles %>%
  filter(grepl("No", CS_Familiarities, ignore.case = TRUE))


#####Pool of expert managers########### 

survey_response_managers_experts <- filtered_survey %>% filter(Managing_Distributed_Teams_Familiarities %in% c("3","4","5"), Years_of_experience_Managing %in% c("3 - 5 years","5 - 10 years", "> 10 years"), Evaluation_Team_Management %in% c("4","5"), PMI_or_others_Certifications %in% c("Yes","Other Certifications"))





