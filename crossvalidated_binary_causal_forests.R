# libraries and setup
library(evalITR)
library(dplyr)
library(haven)
library(grf)

madison_path <- "/Users/madisoncoots/Documents/harvard/research/fishbane/Replication package/Field experiment data and code/TextMessageData.dta"
jack_path <- "../Replication package/Field experiment data and code/TextMessageData.dta"

df <- read_dta(madison_path)

# subset to only those with the newform?
df <- df[df$NewForm == 1,]

# Create Treatment column (original does not work for some reason)
df$Treatment <- df$consequences + df$planmaking + df$combination

# Flip FTA because a success is actually appearing
df$success <- 1 - df$FTA

control <- df[df$Treatment == 0,]
treated_overall <- df[df$Treatment == 1,]
cons <- df[df$consequences == 1,]
plan <- df[df$planmaking == 1,]
combo <- df[df$combination == 1,]

# covariate set, leave one borough out
# offense-based covariates
X_offense <- c("StatenIsland", "Bronx", "Brooklyn", "Queens", "Park", "Alcohol", "Marijuana",
               "DisorderlyActive", "DisorderlyPassive", "Disorderly", "Bike", "Noise",
               "MotorVehicle", "Urination")
# individual-vased covariates
X_perp <- c("Female", "Age", "PastFTA", "NbPastFTA", "NbPastSummons",  "PastSummons", 
            "MedianIncZip", "PctBlackZip", "PctHispZip", "PctBelowPovZip", "DOW")
# all covariates
X_full <- c(X_offense, X_perp)

# Need to remove NAs from data!!!!!

# fit forests
# trees <- 50
# 50 -> 28s
# 100 -> 33s 
# 500 -> 110s
# forest_bin_full <- causal_forest(df_2[X_full], as.vector(df_2$success), 
#                                  as.vector(df_2$Treatment), num.trees = trees)
# forest_bin_offense <- causal_forest(df[X_offense], as.vector(df$success),
#                                     as.vector(df$Treatment), num.trees = trees)
# forest_bin_perp <- causal_forest(df[X_perp], as.vector(df$success), 
#                                  as.vector(df$Treatment), num.trees = trees)
# 
# # Prediction test -- delete later
# predict(forest_bin_full, newdata = df[X_full], estimate.variance = TRUE)

k_fold_causal_forest <- function(k, n_trees, data, covariates, itr_type, save_path) {
  shuffled_idx <- sample(1:nrow(data), nrow(data))
  k_folds <- split(shuffled_idx, cut(seq_along(shuffled_idx), k, labels = FALSE))
  papes <- data.frame()
  out_df <- data.frame()
  for (i in 1:k) {
    eval_row_idx <- unlist(k_folds[i])
    eval_rows <- data[eval_row_idx,]
    train_row_idx <- unlist(k_folds[-i])
    train_rows <- data[train_row_idx,]
    forest <- causal_forest(train_rows[covariates], as.vector(train_rows$success), 
                            as.vector(train_rows$Treatment), num.trees = n_trees)
    predict_output <- predict(forest, newdata = eval_rows[covariates], estimate.variance = TRUE)
    pred <- predict_output$predictions
    pred_var <- predict_output$variance.estimates
    if (!is.na(save_path)) {
      out_df <- rbind(out_df,
                      data.frame(tau = pred, 
                                 tau_var = pred_var,
                                 original_treat = eval_rows$Treatment,
                                 original_outcome = eval_rows$success)
                      )
    }
    # generate ITR
    itr <- generate_ITR(pred, pred_var, itr_type)
    # evaluate ITR (get PAPE)
    pape <- evaluate_itr(eval_rows$Treatment, itr, eval_rows$success)
    papes <- rbind(papes, pape)
  }
  if (!is.na(save_path)) {
    write.csv(out_df, save_path)
  }
  return(papes)
}

generate_ITR <- function(pred, pred_var, itr_type) {
  if (itr_type == "simple") {
    itr <- as.numeric(pred > 0)
  } else {
    itr <- as.numeric(pred - (1.96 * sqrt(pred_var)) > 0)
  }
  return(itr)
}

evaluate_itr <- function(treatment, itr, result) {
  return(PAPE(treatment, itr, result, centered = FALSE))
}

# Calls

# Strict --- 
strict_full_100 <- k_fold_causal_forest(5, 100, df, X_full, "strict", NA)
strict_full_500 <- k_fold_causal_forest(5, 500, df, X_full, "strict", NA)
# strict_full_1000 <- k_fold_causal_forest(5, 1000, df, X_full, "strict", NA)
strict_offense_100 <- k_fold_causal_forest(5, 100, df, X_offense, "strict", NA)
strict_offense_500 <- k_fold_causal_forest(5, 500, df, X_offense, "strict", NA)
# strict_offense_1000 <- k_fold_causal_forest(5, 1000, df, X_offense, "strict", NA)
strict_defendant_100 <- k_fold_causal_forest(5, 100, df, X_perp, "strict", NA)
strict_defendant_500 <- k_fold_causal_forest(5, 500, df, X_perp, "strict", NA)
# strict_defendant_1000 <- k_fold_causal_forest(5, 1000, df, X_perp, "strict", NA)
# Simple --- 
simple_full_100 <- k_fold_causal_forest(5, 100, df, X_full, "simple", NA)
simple_full_500 <- k_fold_causal_forest(5, 500, df, X_full, "simple", NA)
# simple_full_1000 <- k_fold_causal_forest(5, 1000, df, X_full, "simple", NA)
simple_offense_100 <- k_fold_causal_forest(5, 100, df, X_offense, "simple", NA)
simple_offense_500 <- k_fold_causal_forest(5, 500, df, X_offense, "simple", NA)
# simple_offense_1000 <- k_fold_causal_forest(5, 1000, df, X_offense, "simple", NA)
simple_defendant_100 <- k_fold_causal_forest(5, 100, df, X_perp, "simple", NA)
simple_defendant_500 <- k_fold_causal_forest(5, 500, df, X_perp, "simple", NA)
# simple_defendant_1000 <- k_fold_causal_forest(5, 1000, df, X_perp, "simple", NA)

# Evaluating mean PAPEs
mean_pape_strict_full_100 <- strict_full_100 %>% 
  summarize(mean_pape = mean(pape),
            mean_sd = mean(sd)) %>% 
  mutate(forest = "strict_full_100")
mean_pape_strict_offense_100 <- strict_offense_100 %>% 
  summarize(mean_pape = mean(pape),
            mean_sd = mean(sd)) %>% 
  mutate(forest = "strict_offense_100")
mean_pape_strict_defendant_100 <- strict_defendant_100 %>% 
  summarize(mean_pape = mean(pape),
            mean_sd = mean(sd)) %>% 
  mutate(forest = "strict_defendant_100")
mean_pape_simple_full_100 <- simple_full_100 %>% 
  summarize(mean_pape = mean(pape),
            mean_sd = mean(sd)) %>% 
  mutate(forest = "simple_full_100")
mean_pape_simple_offense_100 <- simple_offense_100 %>% 
  summarize(mean_pape = mean(pape),
            mean_sd = mean(sd)) %>% 
  mutate(forest = "simple_offense_100")
mean_pape_simple_defendant_100 <- simple_defendant_100 %>% 
  summarize(mean_pape = mean(pape),
            mean_sd = mean(sd)) %>% 
  mutate(forest = "simple_defendant_100")

mean_pape_strict_full_500 <- strict_full_500 %>% 
  summarize(mean_pape = mean(pape),
            mean_sd = mean(sd)) %>% 
  mutate(forest = "strict_full_500")
mean_pape_strict_offense_500 <- strict_offense_500 %>% 
  summarize(mean_pape = mean(pape),
            mean_sd = mean(sd)) %>% 
  mutate(forest = "strict_offense_500")
mean_pape_strict_defendant_500 <- strict_defendant_500 %>% 
  summarize(mean_pape = mean(pape),
            mean_sd = mean(sd)) %>% 
  mutate(forest = "strict_defendant_500")
mean_pape_simple_full_500 <- simple_full_500 %>% 
  summarize(mean_pape = mean(pape),
            mean_sd = mean(sd)) %>% 
  mutate(forest = "simple_full_500")
mean_pape_simple_offense_500 <- simple_offense_500 %>% 
  summarize(mean_pape = mean(pape),
            mean_sd = mean(sd)) %>% 
  mutate(forest = "simple_offense_500")
mean_pape_simple_defendant_500 <- simple_defendant_500 %>% 
  summarize(mean_pape = mean(pape),
            mean_sd = mean(sd)) %>% 
  mutate(forest = "simple_defendant_500")

# Organizing them into a table
bind_rows(mean_pape_strict_full_100,
          mean_pape_strict_offense_100,
          mean_pape_strict_defendant_100,
          mean_pape_simple_full_100,
          mean_pape_simple_offense_100,
          mean_pape_simple_defendant_100,
          mean_pape_strict_full_500,
          mean_pape_strict_offense_500,
          mean_pape_strict_defendant_500,
          mean_pape_simple_full_500,
          mean_pape_simple_offense_500,
          mean_pape_simple_defendant_500) %>%
  arrange(desc(mean_pape))

save_path <- '/Users/madisoncoots/Documents/harvard/coursework/gov2003/text-itr/cv_output/'

# Re-running with save paths to save outputs
k_fold_causal_forest(5, 100, df, X_perp, "strict", paste(save_path, "strict_defendant_100.csv", sep = ""))
k_fold_causal_forest(5, 100, df, X_perp, "simple", paste(save_path, "simple_defendant_100.csv", sep = ""))

