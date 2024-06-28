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
  summarise(pop = sum(total),
            .by = c(state, grade_group_cat)) |>
  mutate(percent = pop/sum(pop), .by = state)
