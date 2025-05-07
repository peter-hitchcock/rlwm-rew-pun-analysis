# BRMS models  
which_model <- 6
#cluster <- 0 #^PUT BACK - but change for PR run locally  
testing <- 1

# ^ SETTINGS  
sapply(c(
  "data.table",
  "dplyr", 
  "ggplot2", 
  "stringr",
  "foreach", 
  "doParallel", 
  "truncnorm", 
  "testit",
  "Rsolnp",
  "mvtnorm", 
  "brms"
), 
require, character=TRUE)
sf <- function() sapply(paste0("./Functions/", list.files("./Functions/", recursive=TRUE)), source) # Source all fxs
sf()

if (testing==1) cluster <- 0

if (cluster==1) {
  registerDoParallel(cores=40)  
} else {
  registerDoParallel(cores=round(detectCores()*2/3))
}

# Read in the learning and test data with questionnaire sums added to it in `key-results-paper_2-19-24.Rmd`
learn_df <- read.csv("../data/learn_df_with_qairre_data.csv")
test_df <- read.csv("../data/test_df_with_qairre_data.csv")

# For testing neutral preference, subset to just the error df  
learn_error_df <- learn_df %>% filter(correct == 0)
test_error_df <- test_df %>% filter(correct == 0)

IDs <- unique(learn_df$ID)
# To prevent overwriting on save  
rand_part <- round(runif(1, 1e4, 9e4), 0)

# Set up save paths
path <- "../model_res/brms_res/"

this_fit <- NULL

# model_str <- "test"
# this_fit <- brm(correct ~ scale(depr_anx_sum) + 
#               (1|ID),
#             data = learn_df %>% filter(phase==1), 
#             family = bernoulli(link = "logit"), 
#             warmup=10, iter=20, chains=2, cores=2,
#             control= list(adapt_delta = 0.9))


if (which_model == 1) {
  
  #model_str <- "full_correct_perf_model_phase1"
  # Divergent transitions even w 4k samples so fitting simpler model 
  # this_fit <- brm(
  #   correct ~ scale(depr_anx_sum)*scale(set_size) + (scale(depr_anx_sum)*scale(set_size)|ID),
  #     data = learn_df %>% filter(phase==1), family = bernoulli(link = "logit"), 
  #     warmup=2e3, 
  #     iter=4e3, 
  #     chains=5, 
  #     cores=5,
  #     control= list(adapt_delta = 0.9))
  
  # this_fit <- brm(
  #   correct ~ scale(depr_anx_sum)*scale(set_size) + (scale(depr_anx_sum) + scale(set_size)|ID),
  #     data = learn_df %>% filter(phase==1), family = bernoulli(link = "logit"),
  #     warmup=2e3,
  #     iter=4e3,
  #     chains=5,
  #     cores=5,
  #     control= list(adapt_delta = 0.9))
  
  model_str <- "reduced_perf_model_phase1"
  # Still divergent so simplify further
  this_fit <- brm(
    #correct ~ scale(depr_anx_sum)*scale(set_size) + (1|ID),
    correct ~ scale(depr_anx_norm_sum)*scale(set_size) + (1|ID), # Revision corrected  
    data = learn_df %>% filter(phase==1), family = bernoulli(link = "logit"),
    warmup=2e3,
    iter=4e3,
    # warmup=5,
    # iter=10,
    chains=5,
    cores=5,
    control= list(adapt_delta = 0.9))
  
}

if (which_model == 2) {
  # Simpler model as in 1  
  # model_str <- "full_correct_perf_model_phase2"
  # this_fit <- brm(
  #   correct ~ scale(depr_anx_sum)*scale(set_size) + (scale(depr_anx_sum) + scale(set_size)|ID),
  #   data = learn_df %>% filter(phase==2), family = bernoulli(link = "logit"), 
  #   warmup=2e3, 
  #   iter=4e3, 
  #   chains=5, 
  #   cores=5,
  #   control= list(adapt_delta = 0.9))
  
  # Also still divergent so further simplified here
  model_str <- "reduced_correct_perf_model_phase2"
  this_fit <- brm(
    #correct ~ scale(depr_anx_sum)*scale(set_size) + (1|ID),
    correct ~ scale(depr_anx_norm_sum)*scale(set_size) + (1|ID),
    data = learn_df %>% filter(phase==2), family = bernoulli(link = "logit"), 
    warmup=2e3, 
    iter=4e3, 
    chains=5, 
    cores=5,
    control= list(adapt_delta = 0.9))
  
}

# No clear predictions about how set size will interact with 
# depr/axn to influence test because assume no WM contribution, so just test simple effect on correct
if (which_model == 3) {
  
  # model_str <- "full_correct_test_model"
  # this_fit <- brm(
  #   correct ~ scale(depr_anx_sum) + (scale(depr_anx_sum) |ID),
  #   data = test_df, family = bernoulli(link = "logit"), 
  #   warmup=2e3, 
  #   iter=4e3, 
  #   chains=5, 
  #   cores=5,
  #   control= list(adapt_delta = 0.9))
  
  # Reduced bc of divergence  
  model_str <- "reduced_correct_test_model"
  this_fit <- brm(
    #correct ~ scale(depr_anx_sum) + (1 |ID),
    correct ~ scale(depr_anx_norm_sum) + (1 |ID),
    data = test_df, family = bernoulli(link = "logit"), 
    warmup=2e3, 
    iter=4e3, 
    chains=5, 
    cores=5,
    control= list(adapt_delta = 0.9))
  
}

if (which_model == 4) {
  
  model_str <- "neutral_pref_learn_model"
  this_fit <- brm(
    #neutral ~ scale(depr_anx_sum) + (scale(depr_anx_sum) |ID),
    neutral ~ scale(depr_anx_norm_sum) + (scale(depr_anx_norm_sum) |ID),
    data = learn_error_df, 
    family = bernoulli(link = "logit"), 
    warmup=2e3, 
    iter=4e3, 
    chains=5, 
    cores=5,
    control= list(adapt_delta = 0.9))
  
}

if (which_model == 5) {
  
  # Reduced due to divergence 
  # model_str <- "neutral_pref_test_model"
  # this_fit <- brm(
  #   neutral ~ scale(depr_anx_sum) + (scale(depr_anx_sum) |ID),
  #   data = test_error_df, 
  #   family = bernoulli(link = "logit"), 
  #   warmup=2e3, 
  #   iter=4e3, 
  #   chains=5, 
  #   cores=5,
  #   control= list(adapt_delta = 0.9))
  
  # The one that was run 
  # model_str <- "neutral_pref_test_model"
  # this_fit <- brm(
  #   neutral ~ scale(depr_anx_sum) + (1 |ID),
  #   data = test_error_df, 
  #   family = bernoulli(link = "logit"), 
  #   warmup=2e3, 
  #   iter=4e3, 
  #   chains=5, 
  #   cores=5,
  #   control= list(adapt_delta = 0.9))
  
  # Want to look at map estimate, so see if can get convergence w/
  # longer warmup and more samples  
  # however still divergence even with this many 
  # model_str <- "neutral_pref_test_model"
  # this_fit <- brm(
  #   neutral ~ scale(depr_anx_sum) + (scale(depr_anx_sum) |ID),
  #   data = test_error_df,
  #   family = bernoulli(link = "logit"),
  #   warmup=15e3,
  #   iter=18e3,
  #   chains=5,
  #   cores=8,
  #   control= list(adapt_delta = 0.9))
  
  # Trying intercept to 0 — this one worked  
  model_str <- "neutral_pref_test_model_0int"
  this_fit <- brm(
    #neutral ~ scale(depr_anx_sum) + (0 + scale(depr_anx_sum) |ID),
    neutral ~ scale(depr_anx_norm_sum) + (0 + scale(depr_anx_norm_sum) |ID),
    data = test_error_df,
    family = bernoulli(link = "logit"),
    warmup=15e3,
    iter=18e3,
    chains=5,
    cores=8,
    control= list(adapt_delta = 0.9))
  
}

# Run the same models with RRS  
if (which_model == 6) {
  
  model_str <- "RUM_reduced_perf_model_phase1"
  # Still divergent so simplify further
  this_fit <- brm(
    correct ~ scale(rrs_sum)*scale(set_size) + (1|ID),
    data = learn_df %>% filter(phase==1), family = bernoulli(link = "logit"),
    warmup=2e3,
    iter=4e3,
    chains=5,
    cores=5,
    control= list(adapt_delta = 0.9))
  
}

if (which_model == 7) {
  
  model_str <- "RUM_reduced_correct_perf_model_phase2"
  this_fit <- brm(
    correct ~ scale(rrs_sum)*scale(set_size) + (1|ID),
    data = learn_df %>% filter(phase==2), family = bernoulli(link = "logit"), 
    warmup=2e3, 
    iter=4e3, 
    chains=5, 
    cores=5,
    control= list(adapt_delta = 0.9))
  
}

# No clear predictions about how set size will interact with 
# depr/axn to influence test because assume no WM contribution, so just test simple effect on correct
if (which_model == 8) {

  model_str <- "RUM_reduced_correct_test_model"
  this_fit <- brm(
    correct ~ scale(rrs_sum) + (1 |ID),
    data = test_df, family = bernoulli(link = "logit"), 
    warmup=2e3, 
    iter=4e3, 
    chains=5, 
    cores=5,
    control= list(adapt_delta = 0.9))
  
}

if (which_model == 9) {
  
  model_str <- "RUM_neutral_pref_learn_model"
  this_fit <- brm(
    neutral ~ scale(rrs_sum) + (scale(rrs_sum) |ID),
    data = learn_error_df, 
    family = bernoulli(link = "logit"), 
    warmup=2e3, 
    iter=4e3, 
    chains=5, 
    cores=5,
    control= list(adapt_delta = 0.9))
  
}

if (which_model == 10) {
  
  model_str <- "RUM_neutral_pref_test_model"
  this_fit <- brm(
    neutral ~ scale(rrs_sum) + (1 |ID),
    data = test_error_df, 
    family = bernoulli(link = "logit"), 
    warmup=2e3, 
    iter=4e3, 
    chains=5, 
    cores=5,
    control= list(adapt_delta = 0.9))
  
}

# Estimate time effect separately in phase 1 and 2  
if (which_model == 11) {
  
  model_str <- "neutral_pref_time_effect_phase1"
  this_fit <- brm(
    neutral ~ scale(stim_iter) + (scale(stim_iter) |ID),
    data = learn_error_df %>% filter(phase==1), 
    family = bernoulli(link = "logit"), 
    warmup=2e3, 
    iter=4e3, 
    chains=5, 
    cores=5,
    control= list(adapt_delta = 0.9))
}

if (which_model == 12) {
  
  model_str <- "neutral_pref_time_effect_phase2"
  this_fit <- brm(
    neutral ~ scale(stim_iter) + (scale(stim_iter) |ID),
    data = learn_error_df %>% filter(phase==2), 
    family = bernoulli(link = "logit"), 
    warmup=2e3, 
    iter=4e3, 
    chains=5, 
    cores=5,
    control= list(adapt_delta = 0.9))
  
}

if (which_model == 13) {
  
  model_str <- "neutral_pref_test_effect"
  this_fit <- brm(
    neutral ~ 1 + (1 |ID),
    data = test_error_df, 
    family = bernoulli(link = "logit"), 
    warmup=2e3, 
    iter=4e3, 
    chains=5, 
    cores=5,
    control= list(adapt_delta = 0.9))
  
}


# R1  
if (which_model == 14) {
  
  # Reduced model given divergences w phase RE   
  model_str <- "neutral_pref_test_effect"
  this_fit <- brm(
    neutral ~ phase + (1 |ID),
    data = test_error_df, 
    family = bernoulli(link = "logit"), 
    warmup=2e3, 
    iter=4e3, 
    chains=5, 
    cores=5,
    control= list(adapt_delta = 0.9))
  
}

# To add  Neutral preference in phase 1 and phase 2  

cat("\n\n**Model**", model_str)


model_chains <- as_draws(this_fit)

all_chains_df <- foreach (i = 1:length(model_chains)) %do% {
  data.table(as_draws(this_fit)[[i]] %>% bind_rows())
} %>% bind_rows()

write.csv(all_chains_df, paste0(paste0(path, model_str, "__", rand_part, "__.csv")))
write.csv(data.frame(rhat(this_fit)), paste0(paste0(path, "Rhat", model_str, "__", rand_part, "__.csv")))
