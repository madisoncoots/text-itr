# Training the model to be used by the contextual bandit
# Model framework taken from: https://arxiv.org/pdf/1003.0146.pdf

library(tidyverse)
library(janitor)

data_path <- "/Users/madisoncoots/Documents/harvard/research/fishbane/Replication package/Field experiment data and code/TextMessageData.dta"

raw_text_messages <- read_dta(data_path) %>%
  clean_names()

model_data <- raw_text_messages %>%
  filter(new_form == 1, # want to ensure we're isolating the effects of texts
         cell_phone == 1) %>% 
  select(-new_form,
         -cell_phone) %>%
  mutate(texted = 
           (consequences == 1) |
           (planmaking == 1) |
           (combination == 1)) %>%
  select(-consequences,
         -planmaking,
         -combination)

text_model_data <- model_data %>%
  filter(texted) %>%
  select(-texted)

no_text_model_data <- model_data %>%
  filter(!texted) %>%
  select(-texted)

train_pct <- 0.3
n_train_text <- round(train_pct * nrow(text_model_data))
n_train_no_text <- round(train_pct * nrow(no_text_model_data))

set.seed(1)
text_train_idc <- sample(1:nrow(text_model_data), n_train_text, replace = FALSE)
no_text_train_idc <- sample(1:nrow(no_text_model_data), n_train_no_text, replace = FALSE)

text_train_data <- text_model_data[text_train_idc,]
no_text_train_data <- no_text_model_data[no_text_train_idc,]

# Revise this -- probably want to do some  data cleaning 
# to combine the location indicators into one variable and use it
model_formula <- fta ~ female + age + past_fta + past_summons +
  nb_past_fta + nb_past_summons + predicted_fta

text_model <- glm(model_formula, data = text_train_data, family = "binomial")
no_text_model <- glm(model_formula, data = no_text_train_data, family = "binomial")
