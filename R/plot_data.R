library(tidyverse)

dat <- read_rds("data/student_survey.rds")

ggplot(data = dat |> filter(year_term >= "fall 2022"), 
       aes(x = year_term, 
           y = value, 
           color = school, group = school,
           linetype = ban_schools)) +
  geom_line(size = 1) + 
  geom_vline(xintercept = 4.5, color = "black", linetype = "dashed") + 
   facet_wrap(vars(standard_question)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        strip.text = element_text(size = 14)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  labs(
    title = "Middle School Climate Survey Responses",
    x = NULL, y = "Construct Score")

ggsave("output/response_plot.png", width = 10, height = 7, bg = "white")