#FAS 1001 - research TP 2

#libraries
library(tidyverse)
library(dplyr)
library(haven)

#US survey for mental health status based on individual's emotions (ranking out of 27)
file_path <- "/Users/yoshili/fas_1001_Li/depression screener CDC.xpt"
depression_screener <- read_xpt(file_path) #open xpt file in R

#secret little thing: assigning random US states to each observation in random_screener to merge by state in the end

random_screener$State <- sample(state.name, 50, replace = FALSE)
random_screener <- random_screener |>
  select(-origin_state)

#cleaning
depression_screener_clean <- na.omit(depression_screener) |>
  select(-SEQN) 
#there's too many observations so I'm going to have R randomly select 51 of them, one per state
random_screener <- depression_screener_clean |>
  sample_n(50, replace = FALSE)
#addition to obtain score out of 27
screener_sums <- rowSums(random_screener, na.rm = TRUE)
print(screener_sums)

#displaying individuals' scores
random_screener <- random_screener %>%
  mutate(score = rowSums(select(., starts_with("DPQ"))))
random_screener <- random_screener %>%
  mutate(percent_depressed = score / 27)
random_screener <- random_screener %>%
  mutate(percentage = percent_depressed * 100)
#after performing all operations, delete the unneeded columns
random_screener <- random_screener |>
  select(-score, -percent_depressed)

random_screener <- random_screener |>
  rename(percent_depressed = percentage)
  
#probably could've just used the pipe over and over again
#but i forgot and got too lazy to change it so eh :P


#rate of death by suicide in US
suicide_rate <- `Multiple.Cause.of.Death,.1999.2020` #file imported directly from CDC data bank
rm(`Multiple.Cause.of.Death,.1999.2020`)

#cleaning
suicide_rate_clean <- na.omit(suicide_rate) |> #omitting cells with NA 
  select(-Notes, -State.Code, -Crude.Rate) #omitting irrelevant variables

#obtaining rate of death by suicide
column_sums <- colSums(suicide_rate_clean[, -which(names(suicide_rate_clean) == "State")], na.rm = TRUE)
print(column_sums) #sum deaths by suicide: 840204, US population sum: 6746356647

#joining the two cleaned datasets
effect <- left_join(random_screener, suicide_rate_clean, by = "State")

effect <- effect |>
  select(State, everything()) #to put State in the front 
effect <- effect |>
  select(-DPQ010, -DPQ020, -DPQ030, -DPQ040, -DPQ050, -DPQ060, -DPQ070, -DPQ080, -DPQ090, -DPQ100)

effect <- effect |>
  rename(Deaths_by_suicide = Deaths)

effect <- effect |>
  mutate(Rate_of_suicide = Deaths_by_suicide/Population)

#let's create a graph thingamajig
library(ggplot2)

s <- ggplot(data = effect,
       mapping = aes(x = State, y = percent_depressed)) +
  geom_point(color = "deepskyblue") +
  geom_smooth(method = "lm", se = FALSE)

s

s <- s +
  labs(x = "State", y = "Percentage of depression") +  # axis labels
  ggtitle("La santé mentale et le suicide")  # add title

s



