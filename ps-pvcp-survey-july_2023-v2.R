
# This is the R script for analysis of Survey conducted by
# Plateau State Potato Value Chain Support Project (AfDB Assisted)
# July, 2023

# Loading the package tidyverse

library(tidyverse)

#reading in the csv file
#this will open a pop-up through which you can locate the data
#on your local drive
pvcp_survey_data <- read_csv(file=file.choose())

#Taking a look at the imported dataset
library(skimr)
skim_without_charts(pvcp_survey_data)

#We now create variables relevant to the objectives 
#of our research
#Create a variable that categorizes the LGAs into 
#Core and Non-Core Potato Producing LGAs
pvcp_data <- pvcp_survey_data %>%
  mutate(core_potato_lga = ifelse(lga_residence == "Barkin Ladi"|
                                    lga_residence == "Bassa"|
                                    lga_residence == "Bokkos"|
                                    lga_residence == "Jos East"|
                                    lga_residence == "Jos North"| 
                                    lga_residence == "Jos South"|
                                    lga_residence == "Riyom"| 
                                    lga_residence == "Mangu"|
                                    lga_residence == "Pankshin", "Yes", "No"),
         #Create another variable for the proportion of money spent on
         #preparation of potato farm compared to other crops
         farm_prep_prop = cost_land_prep_planting/
           (cost_land_prep_planting + cost_land_prep_planting_other_crops) *100,
         #Create another variable for the proportion of fertilizer bought
         #compared to the total fertilizer needed
         fert_prop = fert_purchased/total_fert_bags_needed *100,
         #variable representing time to complete the survey
         time_taken = end - start,
         survey_day = weekdays(start))

#let's view our new dataset
skim_without_charts(pvcp_data)

#Finding the bussiest day of the survey
pvcp_data %>% 
  count(survey_day)


#Plotting Average Survey Completion Times by LGA
pvcp_data %>%
  select(lga_residence,time_taken) %>% 
  #We consider those with up to 100min survey time as outliers
  filter(time_taken < 100) %>%
  group_by(lga_residence)%>%
  summarize(mean_time = round(mean(time_taken, na.rm = T),2)) %>% 
  arrange(mean_time) %>% 
  ggplot(., 
         mapping = aes(x = fct_reorder(lga_residence, desc(mean_time), 
                                       .desc = TRUE), y = mean_time)) +
  geom_col(color = "steelblue", fill = "steelblue") +
  labs(title = "Average Survey Completion Time",
       y = "Time (minutes)",
       x = "Local Government Area", caption = "Source: PS-PVCP Survey, July, 2023") + 
  coord_flip()



#Plotting Average Survey Completion Times by Gender
pvcp_data %>%
  select(gender,time_taken) %>% 
  #We consider those with up to 100min survey time as outliers
  filter(time_taken < 100) %>%
  group_by(gender)%>%
  summarize(mean_time = round(mean(time_taken, na.rm = T),2)) %>% 
  arrange(mean_time) %>% 
  ggplot(., 
         mapping = aes(x = fct_reorder(gender, desc(mean_time), 
                                       .desc = TRUE), y = mean_time)) +
  geom_col(color = "steelblue", fill = "steelblue") +
  labs(title = "Average Survey Completion Time",
       y = "Time (minutes)",
       x = "Gender", caption = "Source: PS-PVCP Survey, July, 2023") + 
  coord_flip()


#Computing Average Farm Size by LGA
pvcp_data %>%
  select(lga_residence,farm_size_ha) %>% 
  group_by(lga_residence)%>%
  summarize(mean_size = round(mean(farm_size_ha, na.rm = T),2)) %>% 
  arrange(mean_size) %>% 
  ggplot(., 
         mapping = aes(x = fct_reorder(lga_residence, desc(mean_size), 
                                       .desc = TRUE), y = mean_size)) +
  geom_col(color = "steelblue", fill = "steelblue") +
  labs(title = "Average Potato Farm Size",
       y = "Farm Size (Ha)",
       x = "Local Government Area", caption = "Source: PS-PVCP Survey, July, 2023") + 
  coord_flip()

pvcp_data %>%
  select(core_potato_lga,farm_size_ha) %>% 
  group_by(core_potato_lga)%>%
  summarize(mean_size = round(mean(farm_size_ha, na.rm = T),2)) %>% 
  arrange(mean_size) %>% 
  ggplot(., 
         mapping = aes(x = fct_reorder(core_potato_lga, desc(mean_size), 
                                       .desc = TRUE), y = mean_size)) +
  geom_col(color = "steelblue", fill = "steelblue") +
  labs(title = "Average Potato Farm Size",
       y = "Farm Size (Ha)",
       x = "Core Potato Producing LGA", caption = "Source: PS-PVCP Survey, July, 2023") + 
  coord_flip()


pvcp_data %>%
  select(gender,farm_size_ha) %>% 
  group_by(gender)%>%
  summarize(mean_size = round(mean(farm_size_ha, na.rm = T),2)) %>% 
  arrange(mean_size) %>% 
  ggplot(., 
         mapping = aes(x = fct_reorder(gender, desc(mean_size), 
                                       .desc = TRUE), y = mean_size)) +
  geom_col(color = "steelblue", fill = "steelblue") +
  labs(title = "Average Potato Farm Size",
       y = "Farm Size (Ha)",
       x = "Gender", caption = "Source: PS-PVCP Survey, July, 2023") + 
  coord_flip()



#Proportion of Farmland Used for Cultivation of Potato
pvcp_data %>%
  mutate(potato_farm_prop = farm_size_ha/(farm_size_ha + 
                                            farm_size_others_ha)*100) %>%
  select(lga_residence,potato_farm_prop) %>% 
  group_by(lga_residence) %>%
  summarize(mean_farm_prop = round(mean(potato_farm_prop, na.rm = T),2)) %>%
  arrange(mean_farm_prop) %>%
  ggplot(., 
         mapping = aes(x = fct_reorder(lga_residence, desc(mean_farm_prop), 
                                       .desc = TRUE), y = mean_farm_prop)) +
  geom_col(color = "steelblue", fill = "steelblue") +
  labs(title = "Average Proportion of Farm Used for Potato Cultivation",
       y = "Percentage (%)",
       x = "Local Government Area", caption = "Source: PS-PVCP Survey, July, 2023") + 
  coord_flip()


pvcp_data %>%
  mutate(potato_farm_prop = farm_size_ha/(farm_size_ha + 
                                            farm_size_others_ha)*100) %>%
  select(core_potato_lga,potato_farm_prop) %>% 
  group_by(core_potato_lga) %>%
  summarize(mean_farm_prop = round(mean(potato_farm_prop, na.rm = T),2)) %>%
  arrange(mean_farm_prop) %>%
  ggplot(., 
         mapping = aes(x = fct_reorder(core_potato_lga, desc(mean_farm_prop), 
                                       .desc = TRUE), y = mean_farm_prop)) +
  geom_col(color = "steelblue", fill = "steelblue") +
  labs(title = "Average Proportion of Farm Used for Potato Cultivation",
       y = "Percentage (%)",
       x = "Core Potato Producing LGA", caption = "Source: PS-PVCP Survey, July, 2023") + 
  coord_flip()


pvcp_data %>%
  mutate(potato_farm_prop = farm_size_ha/(farm_size_ha + 
                                            farm_size_others_ha)*100) %>%
  select(gender,potato_farm_prop) %>% 
  group_by(gender) %>%
  summarize(mean_farm_prop = round(mean(potato_farm_prop, na.rm = T),2)) %>%
  arrange(mean_farm_prop) %>%
  ggplot(., 
         mapping = aes(x = fct_reorder(gender, desc(mean_farm_prop), 
                                       .desc = TRUE), y = mean_farm_prop)) +
  geom_col(color = "steelblue", fill = "steelblue") +
  labs(title = "Average Proportion of Farm Used for Potato Cultivation",
       y = "Percentage (%)",
       x = "Gender", caption = "Source: PS-PVCP Survey, July, 2023") + 
  coord_flip()


#Plotting a bar chart of proportion of amount spent per LGA
pvcp_data %>%
  select(lga_residence, farm_prep_prop) %>%
  group_by(lga_residence) %>%
  summarise(mean_prep_prop = mean(farm_prep_prop)) %>%
  ggplot(., 
         mapping = aes(x = fct_reorder(lga_residence, 
                                       desc(mean_prep_prop),
                                       .desc = TRUE), 
                       y = mean_prep_prop)) + 
  geom_col(color = "steelblue", fill = "steelblue") +
  labs(title = "Average Proportion of Money Spent on Farm Preparation 
       and Cultivation of Potato",
       y = "Percentage (%)",
       x = "Local Government", caption = "Source: PS-PVCP Survey, July, 2023") + 
  coord_flip()


pvcp_data %>%
  select(core_potato_lga, farm_prep_prop) %>%
  group_by(core_potato_lga) %>%
  summarise(mean_prep_prop = mean(farm_prep_prop)) %>%
  ggplot(., 
         mapping = aes(x = fct_reorder(core_potato_lga, 
                                       desc(mean_prep_prop),
                                       .desc = TRUE), 
                       y = mean_prep_prop)) + 
  geom_col(color = "steelblue", fill = "steelblue") +
  labs(title = "Average Proportion of Money Spent on Farm Preparation 
       and Cultivation of Potato",
       y = "Percentage (%)",
       x = "Core Potato Producing LGA", caption = "Source: PS-PVCP Survey, July, 2023") + 
  coord_flip()


pvcp_data %>%
  select(gender, farm_prep_prop) %>%
  group_by(gender) %>%
  summarise(mean_prep_prop = mean(farm_prep_prop)) %>%
  ggplot(., 
         mapping = aes(x = fct_reorder(gender, 
                                       desc(mean_prep_prop),
                                       .desc = TRUE), 
                       y = mean_prep_prop)) + 
  geom_col(color = "steelblue", fill = "steelblue") +
  labs(title = "Average Proportion of Money Spent on Farm Preparation 
       and Cultivation of Potato",
       y = "Percentage (%)",
       x = "Gender", caption = "Source: PS-PVCP Survey, July, 2023") + 
  coord_flip()



#Proportion of Fertilizer Purchased
#By LGA
pvcp_data %>%
  select(lga_residence, fert_prop) %>%
  #removing outliers
  filter(fert_prop > 0.05 & fert_prop < 100) %>%
  group_by(lga_residence) %>%
  summarise(mean_fert_prop = mean(fert_prop)) %>%
  ggplot(., 
         mapping = aes(x = fct_reorder(lga_residence, 
                                       desc(mean_fert_prop),
                                       .desc = TRUE), 
                       y = mean_fert_prop)) + 
  geom_col(color = "steelblue", fill = "steelblue") +
  labs(title = "Average Proportion of Total Required Fertilizer Purchased ",
       y = "Percentage (%)",
       x = "Local Government", caption = "Source: PS-PVCP Survey, July, 2023") + 
  coord_flip()

#By Gender
pvcp_data %>%
  select(gender, fert_prop) %>%
  #removing outliers
  filter(fert_prop > 0.05 & fert_prop < 100) %>%
  group_by(gender) %>%
  summarise(mean_fert_prop = mean(fert_prop)) %>%
  ggplot(., 
         mapping = aes(x = fct_reorder(gender, 
                                       desc(mean_fert_prop),
                                       .desc = TRUE), 
                       y = mean_fert_prop)) + 
  geom_col(color = "steelblue", fill = "steelblue") +
  labs(title = "Average Proportion of Total Required Fertilizer Purchased ",
       y = "Percentage (%)",
       x = "Gender", caption = "Source: PS-PVCP Survey, July, 2023") + 
  coord_flip()


pvcp_data %>%
  filter(farm_size_ha > 0.05) %>%
  mutate(seeds_per_ha = potato_seeds_kg/farm_size_ha/1000)%>%
  #removing outliers 
  #by standard practice, the requirement is 2.5t/ha max
  #we do not want to consider those with twice as much in our analysis
  filter(seeds_per_ha > 0.5 & seeds_per_ha < 10) %>%
  select(lga_residence, seeds_per_ha) %>%
  group_by(lga_residence) %>%
  summarise(mean_seeds_ha = mean(seeds_per_ha)) %>%
  ggplot(., 
         mapping = aes(x = fct_reorder(lga_residence, 
                                       desc(mean_seeds_ha),
                                       .desc = TRUE), 
                       y = mean_seeds_ha)) + 
  geom_col(color = "steelblue", fill = "steelblue") +
  labs(title = "Average Potato Seeds Per Hectare",
       y = "ton/ha",
       x = "Local Government", caption = "Source: PS-PVCP Survey, July, 2023") + 
  coord_flip()





pvcp_trimmed <- pvcp_data%>%
  mutate(seeds_per_ha = potato_seeds_kg/farm_size_ha/1000)%>%
  #removing outliers
  filter(seeds_per_ha < 5)


pvcp_data %>%
  #filter(farm_size_ha > 0.05) %>%
  filter(core_potato_lga == "Yes") %>%
  mutate(seeds_per_ha = potato_seeds_kg/farm_size_ha/1000)%>%
  #removing outliers 
  #by standard practice, the requirement is 2.5t/ha max
  #we do not want to consider those with twice as much in our analysis
  filter(seeds_per_ha > 0.5 & seeds_per_ha < 10) %>%
  select(lga_residence, seeds_per_ha) %>%
  #group_by(lga_residence) %>%
  summarise(mean_seeds_ha = mean(seeds_per_ha))



#Findings on adoption of Potato in the Southern Ecological Belt of the State
pvcp_data %>%
  mutate(potato_farm_prop = farm_size_ha/(farm_size_ha + 
                                            farm_size_others_ha)*100) %>%
  #select(core_potato_lga, lga_residence, farm_prep_prop, ) %>%
  filter(core_potato_lga == "No") %>%
  select(lga_residence, farm_prep_prop, potato_farm_prop) %>%
  group_by(lga_residence) %>%
  summarise(mean_farm_prop = mean(potato_farm_prop),
            mean_prep_prop = mean(farm_prep_prop))%>%
  arrange(-mean_farm_prop)



planting_mth_dist <- pvcp_data %>% 
  count(month_of_planting)%>% 
  arrange(-n) %>%
  ggplot(., aes(x="", y=n, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)
  

pvcp_data %>% 
  count(month_of_planting)%>% 
  arrange(n) %>%
  ggplot(., aes(x="", y=n, fill=month_of_planting)) +
  geom_bar(stat="identity", width=1, color = "black") +
  coord_polar("y", start=0) + theme_void() +
  labs(title = "A Pie Chart of Planting Times",
       caption = "Source: PS-PVCP Survey, July, 2023") +
  #geom_text(aes(label = month_of_planting),
   #         position = position_stack(vjust = 0.5)) +
  guides(fill = guide_legend(title = "Month"))
  

  
  
