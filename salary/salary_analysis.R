library(ggplot2)
library(tidyverse)

#Read in and build up data
salary_df <- read.csv("./data/salary/salary.csv")

#Add in a full name column
salary_df <- salary_df %>% 
  mutate(full_name = paste(first_name, last_name, sep="_"))




#Get the overall fraction of locations
location_percentages <- table(salary_df$location) / length(salary_df$location) * 100
location_percentages

salary_df %>% 
  filter(group == "Faculty") %>%
  group_by(department) %>%
  summarize(faculty_count = n())  %>%
  filter(faculty_count > 20) %>%
  pull(department)


#Total University payout by year
salary_df %>% 
  group_by(year, location) %>%
  summarize(total=sum(comp, na.rm=T)) %>%
  ggplot() +
  geom_col(aes(x = year, y = total, fill=location)) +
  labs(title = "Total University Payout by Year",
       x = "Year",
       y = "Total Compensation")



#Add a column of how long each person has been at Purdue
salary_df <- salary_df %>%
  group_by(full_name) %>%
  arrange(year) %>%
  mutate(
    start_year = min(year),
    years_at_purdue = year - start_year + 1) %>%
  ungroup()

#Plot the salary versus the years at Purdue
salary_df %>% 
  filter(group == "Faculty") %>%
  group_by(location,years_at_purdue) %>%
  summarise(comp_mean = mean(comp), 
            comp_var = var(comp), 
            n = n(), .groups = 'drop') %>%
  mutate(se = sqrt(comp_var / n), 
         ymin = comp_mean - se, 
         ymax = comp_mean + se) %>%
  ggplot(aes(x = years_at_purdue, y = comp_mean, color = location)) +
    geom_line() +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) + 
    labs(
      title = "Mean Salary",
      x = "Years at Purdue",
      y = "Salary"
    )



salary_model <- glm(comp ~ years_at_purdue * location,
                    data = salary_df)

summary(salary_model)



unique(salary_df$department)


#
