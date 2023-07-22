
# This is the second script for analysis of Survey conducted by
# Plateau State Potato Value Chain Support Project (AfDB Assisted)
# July, 2023

# Loading the package tidyverse

library(tidyverse)

#reading in the csv file
pvcp_survey_data <- read_csv(file=file.choose())

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
         fert_prop = fert_purchased/total_fert_bags_needed *100)

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
  labs(title = "Average Percentage of Money Spent on Farm Preparation 
       \nand Cultivation of Potato by Local Government Area",
       y = "Average Percentage of Amount Spent",
       x = "Local Government") + coord_flip()


#Plotting a bar chart of proportion of amount spent per Core and None Core LGA
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
  labs(title = "Average Percentage of Money Spent on Farm Preparation 
       \nand Cultivation of Potato by Local Government Area Category",
       y = "Average Percentage of Amount Spent",
       x = "Core Potato Local Government") + coord_flip()


#Plotting a bar chart of proportion of amount spent per gender
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
  labs(title = "Average Percentage of Money Spent on Farm Preparation 
       \nand Cultivation of Potato by Gender",
       y = "Average Percentage of Amount Spent",
       x = "Gender") + coord_flip()


#Plotting a bar chart of proportion of amount spent per gender for non-core lgas
pvcp_data %>%
  select(gender, core_potato_lga, farm_prep_prop) %>%
  filter(core_potato_lga == "No") %>%
  select(gender, farm_prep_prop) %>%
  group_by(gender) %>%
  summarise(mean_prep_prop = mean(farm_prep_prop)) %>%
  ggplot(., 
         mapping = aes(x = fct_reorder(gender, 
                                       desc(mean_prep_prop),
                                       .desc = TRUE), 
                       y = mean_prep_prop)) + 
  geom_col(color = "steelblue", fill = "steelblue") +
  labs(title = "Average Percentage of Money Spent on Farm Preparation 
       \nand Cultivation of Potato by Gender in Non-Core LGAs",
       y = "Average Percentage of Amount Spent",
       x = "Gender") + coord_flip()

#By LGA
pvcp_data %>%
  select(lga_residence, fert_prop) %>%
  filter(fert_prop > 0.05 & fert_prop < 100) %>%
  group_by(lga_residence) %>%
  summarise(mean_fert_prop = mean(fert_prop)) %>%
  ggplot(., 
         mapping = aes(x = fct_reorder(lga_residence, 
                                       desc(mean_fert_prop),
                                       .desc = TRUE), 
                       y = mean_fert_prop)) + 
  geom_col(color = "steelblue", fill = "steelblue") +
  labs(title = "Average Percentage of Fertilizer Purchased Out of Total 
         \nFertilizer Needed",
       y = "Average Percentage of Fertilizer Purchased",
       x = "Local Government") + coord_flip()

#By Gender
pvcp_data %>%
  select(gender, fert_prop) %>%
  filter(fert_prop > 0.05 & fert_prop < 100) %>%
  group_by(gender) %>%
  summarise(mean_fert_prop = mean(fert_prop)) %>%
  ggplot(., 
         mapping = aes(x = fct_reorder(gender, 
                                       desc(mean_fert_prop),
                                       .desc = TRUE), 
                       y = mean_fert_prop)) + 
  geom_col(color = "steelblue", fill = "steelblue") +
  labs(title = "Average Percentage of Fertilizer Purchased Out of Total 
         \nFertilizer Needed",
       y = "Average Percentage of Fertilizer Purchased",
       x = "Gender") + coord_flip()

library(stringr)
pvcp_data %>%
  str_count(pesticides_used, "Glory")
