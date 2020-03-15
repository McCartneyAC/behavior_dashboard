# ALL STUDENTS DATA SET:
tool  %>% 
  ggplot(aes(x = fct_reorder(name, mu_minutes), y = mu_minutes, fill = mu_minutes))  +
  geom_col() + 
  coord_flip() +
  scale_fill_gradient(high = "#d52b1e", low = "#6caddf") + 
  theme_light() + 
  geom_hline(yintercept = mean(tool$mu_minutes), color = "#004b8d", linetype = "dotted") + 
  geom_hline(yintercept = 6, color = "#004b8d", linetype = "dotted") + 
  labs(
    title = "Time out of Learning by Student",
    subtitle = "Averages for first interim period of quarter 2",
    x = "Student",
    y = "Average Minutes per Day out of Class",
    fill = "Minutes Missed", 
    caption = "'Theoretical Minimum' is how much a student should miss, 
on average, given 30 minutes of weekly counselor time. "
  ) + 
  annotate("text", x =10, y = 17, label = "TKS Average", color = "#004b8d") + 
  annotate("text", x =2, y = 9, label = "Theoretical 
Minimum", color = "#004b8d") + 
  guides(fill = FALSE)


# INdividual Student by class period:
nsdat <- nsdat  %>% 
  gather(-period, -day, key = "type", value = "minutes")


nsdat_grouped <-nsdat  %>% 
  group_by(period, type)  %>% 
  mutate(mu_mins = mean(minutes))   %>% 
  ungroup()  %>% 
  group_by(period)  %>% 
  mutate(mu_2 = mean(minutes)) %>% 
  select(period, day, type, mu_mins, mu_2)   %>% 
  distinct(mu_mins, .keep_all = TRUE)    


duke <- c(
  "#001A57",
  "#00539B",
  "#C84E00",
  "#E89923",
  "#FFD960",
  "#A1B70D",
  "#339898",
  "#1D6363",
  "#005587",
  "#0577B1",
  "#993399",
  "#E2E6ED",
  
  "#FCF7E5",
  "#988675",
  "#DAD0C6",
  "#E5E5E5")

nsdat_grouped  %>%
  ggplot(aes(x = period, y = mu_mins, fill = factor(type)))  + 
  geom_col()  + 
  coord_polar() + 
  scale_x_continuous(breaks = c(1:7)) + 
  theme_light() +
  geom_text(aes(label = as.character(round(mu_2,2)), y = mu_2*7), color = "#111111") +
  scale_fill_manual(values = duke) + 
  labs(
    title = "Average Daily Minutes Missed by Class Period",
    subtitle = "Name Of Student",
    x = "Period",
    y = "Average Daily Minutes Missed", 
    fill = "Type of Absence:"
  )