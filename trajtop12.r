library(tidyverse)
theme_typewriter <- function() {
  require(sysfonts)
  font_add_google("Special Elite")
  showtext::showtext_auto()
  ggplot2::theme_light()+
    ggplot2::theme(text = ggplot2::element_text(family = "Special Elite")) 
}

bx <- mccrr::paste_data() # shouldn't use this, but don't want to save database to hard drive 
top12 <- bx %>% 
  group_by(Name.of.Student.) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(12)
# top12
top12 %>% 
  left_join(bx, by = "Name.of.Student.") %>% 
  mutate(day = mdy(Date.Record.Created)) %>% 
  mutate(month_yr = format_ISO8601(day, precision = "ym")) %>% 
  group_by(Name.of.Student., month_yr, .add = T) %>% 
  summarize(n_per_month = n(), n_total =first(n), .groups = "drop") %>% # total reports plus reports per month. 
  mutate(month = ym(month_yr)) %>% # this is such a silly, hacky way of getting my() from mdy(). surely there's an easier way. 
  ggplot(aes(x = month, y = n_per_month, fill = Name.of.Student.)) +
  geom_col()+
  geom_smooth(method = "lm", se = F, color = "black")+
  facet_wrap(facets = ~fct_reorder(Name.of.Student.,n_total,.desc = TRUE)) + #order by descending of total n of incidents
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

