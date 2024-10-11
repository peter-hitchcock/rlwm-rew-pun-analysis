RunSimLearnTest <- function(learn_df, helpers, test_df, n_iters=1) {
  ### Simulate on one subject's data on learning + test, given a set of parameters ###
    
  assert("Helpers set to fit on wrong data", helpers$on_what == "learn_test")
  
  # The prep df files should be created on a project-specific basis but often 
  # some variable renaming is needed before starting to optimize real data or simulated data for par recov
  if (sim_opt == "sim") {
    out <- PrepDfForModel(learn_df, test_df, helpers) # This can be helpful when   
  } else if (sim_opt == "par_recov") {
    out <- PrepDfForModelPRSim(learn_df, test_df, helpers)  
  }
  
  helpers <- out$helpers
  learn_df <- out$learn_df
  
  test_df <- out$test_df
  
  parameters <- this_fit_df %>% select(helpers$par_names)
  
  sim_out_one_subj <- foreach (i = 1:n_iters) %do% {
    
    if (helpers$which_model == "MODELNAME") {
      this_sim <- MODELNAME(parameters, learn_df, helpers, test_df)  
    }
    
    this_sim  
  } 
  
  # Bind up to send out 
  sim_learn_df <- 
    foreach (i=1:n_iters) %do% {data.frame(sim_out_one_subj[[i]]$sim_learn_df, "iter"=i)} %>% bind_rows()
  sim_test <- 
    foreach (i=1:n_iters) %do% {data.frame(sim_out_one_subj[[i]]$sim_test_df, "iter"=i)} %>% bind_rows()
  
  sim_out_one_subj <- list("sim_learn"=sim_learn_df, "sim_test"=sim_test)
  
sim_out_one_subj
}