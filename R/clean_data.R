library(tidyverse)

raw <- read_csv("data-raw/SeattlePublicSchoolsAllDataLeadershipView16Nov2024.csv")

# standardize names to whats in the survey files
middle_schools <- c("aki kurose", "david t. denny int'l", "eckstein", "hamilton int'l", 
                    "jane addams", "madison", "mcclure", "meany", 
                    "mercer int'l", "eagle staff", "washington", "whitman")

ban_schools <- c("eagle staff", "hamilton int'l")
ban_term <- c("fall 2024") # update to include spring if it continues

# student/staff survey designation is in Cell A1. Column names are in row 2.
# Different years/survey groups are designated by a column where Row 2 will say
# something like `Fall 2024 - Student Survey (Fall 2024) - Student Survey
# 3rd-5th` then the rest of the rows in the column are blank and the data for
# that survey starts in the next column. So we need to identify the survey
# delimiting columns to chunk out the data.

type <- names(raw)[1] # Student / Staff Surveys
col_names <- tolower(raw[1,])

survey_index <- str_detect(col_names, "^(spring|fall)")

cols_survey_assigned <- col_names
survey_name = "school_name"
for (i in 1:length(survey_index)) {
  if(str_detect(col_names[i], "^(spring|fall)")) {survey_name = col_names[i]}
  cols_survey_assigned[i] = survey_name
  
}

# Now that we have identified which columns go with which survey, we can rename the cols in our data frame. 
# We will combine the survey and the question since questions are repeated across surveys into a single name. 
raw2 <- raw[2:nrow(raw),]

str_detect(col_names, "@") |> sum() # need to confirm that the delimiter we pick isn't used already so we can split easily on it
names(raw2) <- c("school", paste(cols_survey_assigned[-1], col_names[-1], sep = "@"))

ordered_year_terms <- c(
  "spring 2015", "spring 2016", "spring 2017", "spring 2018", "spring 2019", 
  "fall 2020", "spring 2021", "fall 2021", "spring 2022", 
  "fall 2022", "spring 2023", "fall 2023", "spring 2024", "fall 2024"
  #, "spring 2025" # add this in if updating in spring
)

dat <- raw2 |> 
  pivot_longer(-school) |> 
  separate(name, into = c("survey", "question"), sep = "@") |> 
  mutate(
    school = tolower(school),
    value = as.numeric(value),
    # annoying year_term sorting
    year_term = str_trim(str_extract(survey, "^[^-]+")),
    year_term = factor(year_term, levels = ordered_year_terms, ordered = TRUE),
    # identify the schools that banned
    ban_schools = school %in% ban_schools, 
    post_ban = year_term >= ban_term & ban_schools,
    # question standardizing
    question = str_remove(question, "^\\d+\\.\\s*"),
    standard_question = case_when(
      str_detect(question, "safety") ~ "positive behavior & safety", 
      str_detect(question, "inclusion") ~ "inclusionary practices", 
      str_detect(question, "belonging") ~ "belonging & relationships",
      str_detect(question, "social") ~ "social emotional learning",
      TRUE ~ question)
  ) |> 
  filter(!is.na(value)) |> 
  filter(school %in% middle_schools) |> 
  filter(question %in% c("positive behavior & safety",
                         "inclusionary practices",
                         "belonging & relationships",
                         "social emotional learning"))

write_rds(dat, "data/student_survey.rds")
