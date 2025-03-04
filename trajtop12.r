library(tidyverse)
library(lme4)
library(sjPlot)
library(glmmTMB)


bx <- mccrr::paste_data()
bx
top12 <- bx %>% 
  group_by(Name.of.Student.) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(12)
top12
top12 %>% 
  left_join(bx, by = "Name.of.Student.") %>% 
  mutate(day = mdy(Date.Record.Created)) %>% 
  mutate(month_yr = format_ISO8601(day, precision = "ym")) %>% 
  group_by(Name.of.Student., month_yr, .add = T) %>% 
  summarize(n_per_month = n(), n_total =first(n), .groups = "drop") %>% 
  mutate(month = ym(month_yr)) %>% 
  ggplot(aes(x = month, y = n_per_month, fill = Name.of.Student.)) +
  geom_col()+
  geom_smooth(method = "lm", se = F, color = "black")+
  facet_wrap(facets = ~fct_reorder(Name.of.Student.,n_total,.desc = TRUE)) +
  university::scale_fill_wm() + 
  guides(fill = FALSE)+
  scale_x_date(date_breaks = "month", date_labels = "%b") +
  theme_typewriter() + 
  labs(
    title = "Trajectory of Behavior reports",
    subtitle = "Top 12 reported students",
    y = "number of reports",
    x = ""
  )

# m1 <- top12 %>% 
#   left_join(bx, by = "Name.of.Student.") %>% 
#   mutate(day = mdy(Date.Record.Created)) %>% 
#   mutate(month_yr = format_ISO8601(day, precision = "ym")) %>% 
#   group_by(Name.of.Student., month_yr, .add = T) %>% 
#   summarize(n_per_month = n(), n_total =first(n), .groups = "drop") %>% 
#   mutate(month = ym(month_yr)) %>% 
#   lmer(n_per_month ~ 1 + month +(1)|Name.of.Student., data = . )
# m0 <- top12 %>% 
#   left_join(bx, by = "Name.of.Student.") %>% 
#   mutate(day = mdy(Date.Record.Created)) %>% 
#   mutate(month_yr = format_ISO8601(day, precision = "ym")) %>% 
#   group_by(Name.of.Student., month_yr, .add = T) %>% 
#   summarize(n_per_month = n(), n_total =first(n), .groups = "drop") %>% 
#   mutate(month = ym(month_yr)) %>% 
#   lmer(n_per_month ~ 1 + month +(1)|Name.of.Student., data = . )

analytic<-bx %>% 
  mutate(day = mdy(Date.Record.Created)) %>% 
  mutate(month_yr = format_ISO8601(day, precision = "ym")) %>% 
  group_by(Name.of.Student.) %>%
  mutate(n_total = n()) %>% 
  ungroup() %>% 
  group_by(Name.of.Student., month_yr, .add = T) %>% 
  summarize(n_per_month = n(), 
          n_total =first(n_total), 
          gender = first(gender),
          year = first(year),
          campus = first(campus),
          .groups = "drop") %>% 
  mutate(month = ym(month_yr)) %>% 
  mutate(mo_scaled = case_when(
    month_yr == "2024-08" ~ 0,
    month_yr == "2024-09" ~ 1,
    month_yr == "2024-10" ~ 2,
    month_yr == "2024-11" ~ 3,
    month_yr == "2024-12" ~ 4,
    month_yr == "2025-01" ~ 5,
    month_yr == "2025-02" ~ 6,
    month_yr == "2025-03" ~ 7,
  ))

analytic %>% 
  distinct(Name.of.Student., n_total) %>% 
  ggplot(aes(x = n_total)) + 
  geom_histogram(bins = 20,fill = "darkgreen", color = "white") + 
  theme_typewriter() 
m0 <- analytic %>%  # null model
  lmer(n_per_month ~ 1 +  (1 | Name.of.Student.), data = . )
m1 <- analytic %>%  # gaussian model
  lmer(n_per_month ~ 1 + mo_scaled +(1| Name.of.Student.), data = . )
m2 <-  analytic %>%   # poisson model
  glmer(n_per_month ~  mo_scaled + (1 | Name.of.Student.), 
        data = ., 
        family = poisson(link = "log"),
        control = glmerControl(optimizer = "bobyqa"))

sjPlot::tab_model(m0,m1,m2)





# zero inflated poisson ---------------------------------------------------
analytic
set.seed(02000) #Kyiv Ukraine 
#new_students <- paste0("Student_", sample(1000:9999, 79, replace = FALSE))  # Random IDs
#new_students
# Create a dataframe with these missing students, assigning them n_per_month = 0 for all months
# students_with_no_incidents <- expand_grid(
#   Name.of.Student. = new_students,
#   mo_scaled = unique(analytic$mo_scaled)
# ) %>%
#   mutate(n_per_month = 0)
# students_with_no_incidents
# # Combine this with the original dataset
# Data_complete <- bind_rows(analytic, students_with_no_incidents)
# Data_complete

# Fill in missing months for all students, assigning 0s where needed
Data_complete <- analytic %>%
  complete(Name.of.Student., mo_scaled, fill = list(n_per_month = 0)) 

Data_complete <- Data_complete %>% 
  filter(Name.of.Student. != "")


Data_complete <- Data_complete %>% 
  select(Name.of.Student., n_per_month, n_total, mo_scaled) %>% 
  left_join(bx, by = "Name.of.Student.") %>% 
  select(Name.of.Student., gender, year, campus, n_per_month, n_total, mo_scaled) %>% 
  filter(!is.na(mo_scaled)) %>% 
  mutate(n_total = if_else(is.na(n_total), 0, n_total))%>% 
  distinct(Name.of.Student., mo_scaled, .keep_all = TRUE) %>% 
  mutate(dnipro = if_else(campus == "Dnipro", 1, 0))


Data_complete %>% view()
Data_complete 
# modeling ----------------------------------------------------------------


m0 <- Data_complete %>%  # null
  lmer(n_per_month ~ 1 +  (1 | Name.of.Student.), data = . )
m1 <- Data_complete %>%  # gaussian
  lmer(n_per_month ~ 1 + mo_scaled +(1| Name.of.Student.), data = . )
m2 <-  Data_complete %>%  # poisson
  glmer(n_per_month ~  mo_scaled + (1 | Name.of.Student.), 
        data = ., 
        family = poisson(link = "log"),
        control = glmerControl(optimizer = "bobyqa"))
m3<- Data_complete %>%  # ZIP model
  glmmTMB(n_per_month ~ mo_scaled + (1 | Name.of.Student.), 
                       data = ., 
                       family = poisson, 
                       ziformula = ~ 1)  # The zero-inflation component
m4 <-  Data_complete %>%  #zip with covariates in main effect
   glmmTMB(n_per_month ~ mo_scaled + gender + dnipro + year + (1 | Name.of.Student.), 
              data = , 
              family = poisson, 
              ziformula = ~ 1)  # Zero-inflation model
m5 <- Data_complete %>%  # zip with covariates in main + zi
   glmmTMB(n_per_month ~ mo_scaled + gender + dnipro + year + (1 | Name.of.Student.), 
              data = , 
              family = poisson, 
              ziformula = ~ gender + dnipro + year)  # Zero-inflation model



sjPlot::tab_model(m0,m1,m2, m3) # null to zip

sjPlot::tab_model(m3, m4, m5) # zippity doo da


AIC(m4, m5)
BIC(m4, m5)

# final models
sjPlot::tab_model( m4, m5)

