PrepDfForModel <- function(learn_df, test_df, helpers) {
  ### Get the learn and test df ready for fitting
  # Learn df and test df are pt data and the task is structured so that, 
  # with each stimulus set (A and B), there is a single set size containing 
  # stimuli repeatedly learned about (in learning phases 1 and 2 only) and in which actions 
  # are chosen (in  learning phases 1 and 2, and test phases 1 and 2). All models will 
  # assume that this learning/choosing operates independently from other set sizes.
  
  # Thus, for opt/sim, we want to return data structures with the 10 {set size, set idx} 
  # pairs (5 set sizes in sets A and B). Each must have an identifier for phase 1 vs. phase 2
  # so that in phase 2 we can modulate (typically reset) the working memory contirbution at phase 2
  # as well as an identifier for test so that we can stop performing RL and WM updates at test ###
  
  # Finally, to improve speed during optimization we want lists rather than dataframes 
  
  # Thus, first we will add identifiers for learning vs testa and filter by set idx (A/B) 
  
  # Oops — didn't distinguish set A from set B in the test data, so add that label here 
  test_df[1:30, "stim_set"] <- "A"
  test_df[31:60, "stim_set"] <- "B"
  test_df[61:90, "stim_set"] <- "A"
  test_df[91:120, "stim_set"] <- "B"
  
  # Add unique stim per set and against learn df checks  
  # ** to do
  
  learn_df_B <- learn_df %>% filter(stim_set == "B")
  test_df_B <- test_df %>% filter(stim_set == "B")
  
  set_A <- data.frame(learn_df %>% filter(stim_set == "A"), 
                      test_df %>% filter(stim_set == "A"))
  
  set_B <- data.frame(learn_df %>% filter(stim_set == "B"), 
                      test_df %>% filter(stim_set == "B"))
  
  browser()
  # Now if we split on set size we'll have lists 
  
  
  # Finally we go through each of these lists and reduce it to itself vector length 
  # lists for fitting 
  
  
  
}