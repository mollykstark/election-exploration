library(tidyverse)
raw_data <- read.csv("https://raw.githubusercontent.com/alex/nyt-2020-election-scraper/master/all-state-changes.csv")

# basic dataframe exploration
num_col <- ncol(data)
num_rows <- nrow(data)
num_states <- length(unique(data$state))
num_time_stamps <- length(unique(data$timestamp))

# formatting: splitting name and ev count, add biden and trump vote columns
data <- raw_data %>% 
  separate(state, into=c("state", "ev"), " \\(") %>% 
  mutate(ev = parse_number(ev)) %>% 
  mutate(biden_votes = if_else(leading_candidate_name == "Biden", # condition
                               leading_candidate_votes, #if true
                               trailing_candidate_votes), #if false
         trump_votes = total_votes_count - biden_votes
         )

# number of timestamps for each state
timestamps_by_state <- data %>%
  group_by(state) %>%
  count()

#time where biden took the lead in georgia
ga_lead_time <- data %>% 
  filter(state == "Georgia", leading_candidate_name == "Biden") %>%
  filter(timestamp == min(timestamp)) %>% 
  pull(timestamp)

#time where biden took the lead in any state
biden_ahead_time <- data %>% 
  filter(leading_candidate_name == "Biden") %>%
  filter(timestamp == min(timestamp)) %>% 
  select(state, timestamp)

#difference in votes in each state at most recent timestamp
vote_diff <- data %>% 
  group_by(state) %>% 
  filter(timestamp == max(timestamp)) %>% 
  mutate(vote_diff = biden_votes - trump_votes,
         pct_diff = vote_diff / total_votes_count)

vote_diff_plot <- ggplot(vote_diff) + 
  geom_col(mapping = aes(x = vote_diff,
                         y = reorder(state, vote_diff),
                         fill = leading_candidate_name)) +
  scale_fill_manual(values=c("blue", "red")) +
  labs(y = "State", x = "Vote Difference", fill = "Candidate",
       title = "Vote difference at most recent time stamp")

vote_pct_plot <- ggplot(vote_diff) + 
  geom_col(mapping = aes(x = pct_diff, y = reorder(state, pct_diff)))

