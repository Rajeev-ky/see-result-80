library(tidyverse)
library(readxl)
library(tidytext)


df <- read_excel("data/see_result_80.xlsx", sheet = "see80")
head(df)
tail(df)

########### plot - 1
df |> summarise(total_student = sum(total), .by = state) |>
  ggplot(aes(x = factor(state), y = total_student, fill = factor(state))) +
  geom_col() +
  geom_text(aes(label = scales::comma(total_student), vjust = 1), size = 5) +
  labs(
    x = "Province",
    y = "No of Students",
    fill = "Province"
  ) +
  scale_y_continuous(labels = scales::label_comma())

########### plot - 2
df |> mutate(state = as.character(state)) |>
  ggplot(aes(x = reorder_within(state,-total, total), y = total, fill = state)) +
  geom_col() +
  scale_x_reordered() +
  facet_wrap(~ grade_group, scales = "free") +
  theme_minimal() +
  labs(
    x = "Province",
    y = "No. of Students"
  ) +
  scale_y_continuous(labels = scales::label_comma())

########### plot - 3

df |>
  mutate(grade_group_cat = if_else(grade_group == "NG", "NG", "G"), state = as.character(state),.before = grade_group) |>
  summarise(total_pop = sum(total), .by = c(state, grade_group_cat)) |>
  ggplot(aes(x = state, y = total_pop, fill = grade_group_cat)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = scales::comma(total_pop),vjust = 1), position = position_dodge(width = 1), size = 3) +
  theme_minimal() +
  labs(
    x = "Province",
    y = "No. of Students",
    fill = "Is Graded?"
  ) +
  scale_y_continuous(labels = scales::label_comma())

########### plot - 4

df |>
  mutate(grade_group_cat = if_else(grade_group == "NG", "NG", "G"), state = as.character(state),.before = grade_group) |>
  summarise(pop = sum(total), .by = c(state, grade_group_cat)) |>
  mutate(percent = round(pop/sum(pop),2), .by = state) |>
  ggplot(aes(x = state, y = percent, fill = grade_group_cat)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = scales::percent(percent), vjust = 1), position = position_dodge(width = 1), size = 4) +
  theme_minimal() +
  scale_y_continuous(labels = scales::label_percent())

########### plot - 5

df |>
  select(-total) |>
  pivot_longer(cols = c(male, female, other), names_to = "gender", values_to = "pop") |>
  filter(gender != "other") |>
  ggplot(aes(x = as.character(state), y = pop, fill = gender)) +
  geom_col(position = "dodge") +
  facet_wrap(~ grade_group, scales = "free") +
  theme_minimal() +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    x = "Province",
    y = "No. of Students",
    fill = "Gender"
  )

########### plot - 6

df |>
  select(-total) |>
  pivot_longer(cols = c(male, female, other), names_to = "gender", values_to = "pop") |>
  filter(gender != "other") |>
  ggplot(aes(x = as.character(state), y = pop, fill = gender)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = scales::comma(pop), vjust = 1), position = position_dodge(width = 1), size = 2) +
  theme_minimal() +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    x = "Province",
    y = "No. of Students",
    fill = "Gender"
  ) +
  facet_wrap(~ grade_group, scales = "free") +
  theme(
    strip.text.x = element_text(face = "bold")
  )


########### plot - 7

df |>
  select(-total) |>
  pivot_longer(cols = c(male, female, other), names_to = "gender", values_to = "pop") |>
  filter(gender != "other") |>
  summarise(popul = sum(pop), .by = c(state, gender)) |>
  ggplot(aes(x = as.character(state), y = popul, fill = gender)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(x = "Province", y = "No. of students", fill = "Gender") +
  scale_y_continuous(labels = scales::label_comma())


############ plot - 8
library(tidyverse)
library(readxl)
library(ggpol)
library(signs)
pd <- read_excel("data/pop78.xlsx")


pd1 <- pd |> 
  pivot_longer(cols = `00-04`:`95 +`, names_to = "age", values_to = "population") |> 
  mutate(newpop = if_else(Sex == "female", -population, population), pop_1000 = newpop/1000) |> 
  mutate(per = population * 100 /sum(population), .by = c(Province, age,Sex))

pd1 |> ggplot(aes(x = age, y = pop_1000, fill = Sex)) +
  geom_bar(stat = "identity", width = 0.8, position = "identity") +
  theme_minimal() +
  coord_flip() +
  labs(y = "Population in thousands", x = "Age", title = "") +
  scale_y_continuous(labels = abs(pretty(pd1$pop_1000)), breaks = pretty(pd1$pop_1000)) +
  facet_wrap(~ Province, nrow = 2) +
  theme(axis.text = element_text(size = 6))


