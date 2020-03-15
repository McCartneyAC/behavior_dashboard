library(tidyverse)
library(readxl)
library(lubridate)
library(dplyr)
library(ggplot2)
library(magrittr)
library(forcats)
library(reshape2)
points<-read_xlsx("points_full.xlsx")
names(points)
points  %>% 
  mutate(day = factor(day))  %>% 
  mutate(week = factor(week)) %>% 
  select(date, day,week, meanR, meanT, meanF, mean, name) %>% 
  melt(id.vars = c("date", "day", "week", "name"))  %>%
  filter(variable != "mean") %>% 
  ggplot(aes(x = date, y = value, color = variable))  +
  geom_jitter(alpha = 0.4, width = 0.25, height = 0.25) +
  geom_smooth() +
  scale_color_inova() + 
  facet_wrap(~name) +
  theme_light() +
  labs(title= "Trajectory of all points", 
       subtitle = "2018-2019 Academic Year",
       y = "Average Daily Points"
  ) + 
  scale_y_continuous(limits = c(0.0, 3.0)) 



pointsgraph <- function(df) {
  students <- unique(df$name)
  for (i in students) {
    df %>%
      mutate(day = factor(day))  %>%
      mutate(week = factor(week)) %>%
      select(date, day, week, meanR, meanT, meanF, mean, name) %>%
      melt(id.vars = c("date", "day", "week", "name"))  %>%
      filter(variable != "mean") %>%
      filter(name == i) %>%
      ggplot(aes(x = date, y = value, color = variable))  +
      geom_jitter(alpha = 0.4,
                  width = 0.25,
                  height = 0.25) +
      geom_smooth() +
      scale_color_inova() +
      theme_light() +
      labs(title = "Trajectory of all points",
           subtitle = i,
           y = "Average Daily Points") +
      scale_y_continuous(limits = c(0.0, 3.0))
    ggsave(paste0(i, ".png"), device = "png")
  }

}
pointsgraph(points)
##########################

heatmapgraph <- function(df) {
  students <- unique(df$name)
  for (i in students) {
    df  %>%
      filter(name == i) %>%
      # mutate(date = mdy(date))  %>%
      mutate(day = fct_relevel(day, "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))  %>%
      mutate(day = factor(day))  %>%
      melt(id.vars = c("date", "day", "week", "name"))  %>%
      filter(variable == "mean") %>%
      mutate(value = as.numeric(value)) %>% 
      ggplot(aes(
        x = day,
        y = week,
        fill = value
      ))  +
      geom_tile(color = "black") +
      scale_fill_gradient(low = "#d52b1e",
                          high = "#6caddf",
                          limits = c(0, 3)) +
      scale_y_reverse() +
      theme_light() +
      geom_text(aes(label = round(value,2))) +
       labs(
         title = paste0(i, " Average Daily Points"),
         subtitle = "2018-2019 School Year",
         x = "Day"
       ) +
      scale_x_discrete(position = "top") 
   ggsave(paste0(i, ".png"), device = "png", width = 6.2, height = 6.2)
  }
}

heatmapgraph(points)





points  %>% 
  mutate(day = fct_relevel(day, "Monday","Tuesday", "Wednesday","Thursday","Friday"))  %>% 
  mutate(day = factor(day))  %>% 
  melt(id.vars = c("date", "day", "week", "name"))  %>% 
  filter(variable == "mean") %>%  
  mutate(value = as.numeric(value)) %>% 
  ggplot(aes(x = day, y = week, fill = value))  + 
  geom_tile(color = "black") + 
  # geom_text() +
  scale_fill_gradient(low="#d52b1e",  high="#6caddf", limits = c(0,3)) +
  scale_y_reverse() + 
  theme_light() + 
  # geom_text(aes(label = round(value,2)))+
  labs(
    title =  "Average Daily Points",
    subtitle = "2018-2019 School Year",
    x = "Day"
  ) + 
  scale_x_discrete(position = "top") +
  facet_wrap(~name)
