RunRLWMPRewRLOffToMax <- function(parameters, block, helpers, prior_Q_RL_values=NULL, eta=.5) {
  ### Run through modified RLWM model ### 
  
  learn_block <- block %>% filter(type=="learning")
  test_block <- block %>% filter(type=="test")
  
  # Unpack lists by assigning to function environment.. # 
  list2env(helpers, environment())
  
  if (sim_opt=="opt") { # Hack to allow optim to work
    pars <- list()
    for (i in 1:length(par_names)) pars[[par_names[[i]]]] <- parameters[i]
    parameters <- pars
  }
  
  list2env(parameters, environment())
  
  # Block contingencies and data structure #
  # Block in the model files refers to a unique {set size, stim pair} of 10 stim iters — corresponding 
  # to the same images and hence same correct key/values as in the standard task (but with wm values reset at the appropriate times)
  assert(length(unique(block$set_size))==1)
  assert(length(unique(block$stim_set))==1)

  # Create a stim key to be used to identify states within the block *for both train and test* and used to idx value matrix rows 
  unique_stim <- unique(block$stim)
  stim_key <- list("stim"=unique_stim, "stim_id"=1:length(unique_stim))
  
  # Set size  
  n_s <- block$set_size[1]
  
  # Create an id for each stimulus the length of block for learn...
  these_learn_stim <- learn_block$stim
  learn_stim_id <- these_learn_stim
  
  for (i in 1:length(unique(these_learn_stim))) learn_stim_id[these_learn_stim==unique_stim[i]] <- i
  learn_stim_id <- as.numeric(learn_stim_id)
  
  # ... and test 
  these_test_stim <- test_block$stim
  test_stim_id <- these_test_stim
  
  for (i in 1:length(unique(these_test_stim))) test_stim_id[these_test_stim==unique_stim[i]] <- i
  test_stim_id <- as.numeric(test_stim_id)

  # Spot check   
  # data.frame(learn_block %>% select(stim), learn_stim_id)
  # data.frame(test_block %>% select(stim), test_stim_id)
  
  if (par_recovery != "y") assert(n_s==length(unique(block$stim))) # Should be equal to the number of unique stim  

  these_delays <- learn_block$delay
  
  these_stim_iters <- learn_block$stim_iter
  
  # Code up the stim for learn   
  if (sim_opt == "opt") {
    # These are redundant bc it's a deterministic task  
    these_rewards <- as.numeric(learn_block$correct)
    # Sign these negatively so they can be directly incorporated for RL  
    these_punishments <- -as.numeric(learn_block$worst)
    
    # Pull actions for training and test 
    these_actions <- learn_block$action
    these_test_actions <- test_block$action
  }
  
  init_rl_value <- 0
  # Set goal-oriented WM values that allow distinguishing nothing in WM from 
  # neutral 
  init_wm_value <- 1/3
  
  Q_mat_rl <- matrix(init_rl_value, n_s, 3)
  Q_mat_wm <- matrix(init_wm_value, n_s, 3)
  Q_mat_wm_init_values <- matrix(init_wm_value, n_s, 3)
  
  # Preallocate storers #
  if (sim_opt == "sim") {
    # For sim we'll abritarily always let action 1 == correct, action 2 == middle, action 3 == worst,
    # corresponding to 1,2,3 row indices respectively 
    actions <- rep(NA, nrow(learn_block))
    these_rewards <- rep(NA, nrow(learn_block))
    these_neutrals <- rep(NA, nrow(learn_block))
    these_punishments <- rep(NA, nrow(learn_block))
    
    rpes <- rep(NA, nrow(learn_block))
    rpe_grp <- rep(NA, nrow(learn_block))
    wm_capacities <- rep(NA, nrow(learn_block))
    
    # Test versions
    test_actions <- rep(NA, nrow(test_block))
    these_test_rewards <- rep(NA, nrow(test_block))
    these_test_neutrals <- rep(NA, nrow(test_block))
    these_test_punishments <- rep(NA, nrow(test_block))
  }
  
  # For optimization
  if (sim_opt == "opt") {
    learn_neg_log_liks <- rep(NA, nrow(learn_block)) 
    test_neg_log_liks <- rep(NA, nrow(test_block))
  }
  
  debug_info <- list()
  
  # Find the first phase 2 learning trial so can reset WM (need to confirm this works with higher set size)
  first_phase_2_trial <- min(which(learn_block$phase==2))
  
  # Make sure what's labeled phase 1 indeed has same number of diff stim (5 stim iters each )
  if (n_s > 1) {
    assert(table(learn_stim_id[1:(first_phase_2_trial-1)])[1]==table(learn_stim_id[1:(first_phase_2_trial-1)])[2])
  }
  
  ## Loop through learning trials  
  for (tib in 1:nrow(learn_block)) {
    
    # At the new phase, save the phase 1 RL values (to use them 
    # for phase 1 testing) and reset the Q mat WM values  
    if (tib == first_phase_2_trial) {
      phase_1_Q_mat_rl <- Q_mat_rl
      Q_mat_wm <- Q_mat_wm_init_values 
    }
    
    # Pull trial-level information 
    stim <- learn_stim_id[tib]
    
    # CHOICE: Get the action values from RL and WM this trial.. # 
    rl_values <- Q_mat_rl[stim, ]
    wm_values <- Q_mat_wm[stim, ]
    
    # .. pass each through softmax .. (beta hard coded at high value)  
    rl_liks <- unlist(RunLSESoftmax(rl_values))#, debug_info)
    wm_liks <- unlist(RunLSESoftmax(wm_values))#, debug_info)
    
    # Set working memory usage on a trial-wise basis based on delay, 
    # such that usage goes down with higher delay — where usage gives some
    # fraction of rho (full rho when kappa/delay_denom >= 1)
    delay_t <- these_delays[tib]
    if (!is.na(delay_t)) {
      curr_delay <- delay_t#+1  
    } else {
      curr_delay <- 0
    }
    
    rho <- min(1, rho) # The optimizer is having an issue where rho is exceeding 1 by miniscule amt so setting this and then discarding any fits with that true. Pt 12 is one pt for whom happening 
    
    # How many correct items are stored in WM?
    n_correct_items_stored <- length(which(Q_mat_wm > init_wm_value))
    
    corr_items_times_delay <- n_correct_items_stored*curr_delay
    
    ## Dynamically set the proportion of WM capacity used  
    wm_capacity_prop <- NULL 
    
    k_over_nd <- kappa/corr_items_times_delay
    
    if (n_correct_items_stored < kappa) {
      wm_capacity_prop <- 1
    } else {
      # WM capacity constraint - k/nd goes to Inf and thus WM capacity prop goes to 1 
      # whenever n correct items or delay = 0
      wm_capacity_prop <- min(1, k_over_nd)   
    }
    
    if (sim_opt=="sim") wm_capacities[tib] <- wm_capacity_prop
    
    rho_t <- NULL
    epsilon_t <- NULL 
    
    ## Dyanmically set rho and epsilon
    
    # Rho models ind diffs in WM usage that's not dynamically updated. However, we implement a rule so when there's no delay 
    # and correct item is in WM, WM ind diffs don't come into play — we can just assume people have the right item in WM and use it 
    # If delay is NA then it's the first trial of phase, so don't change rho from param because both RL and WM contain no info 
    # (/ if it's first trial of phase 2 WM_liks won't get used anyway — see conditional below)  
    
    # Also scale eps by delay — to model interference accessing value-based representations to convert them into choices
    # scale epsilon by delay  
    if (is.na(delay_t)) {
      rho_t <- rho 
      epsilon_t <- epsilon
    } else if (delay_t == 0) { 
      epsilon_t <- epsilon # Small bug/illogical model point fixed 8/1/24 — now keep epsilon as equal to the parameter when there's no delay irrespective of reward and only scale it by delay if delay > 0 (so epsilon doesn't go to 0)  
      
      if (tib > 1 && these_rewards[tib-1] == 1) { 
        rho_t <- 1
      } else {
        rho_t <- rho
      }

    } else {
      # Otherwise rho is the param value — so ind diffs come into play  
      rho_t <- rho 
      epsilon_t <- min(1, epsilon*delay_t)
    }
    
    W_wm <- as.numeric(rho_t * wm_capacity_prop)
    
    # Mix RL and WM likelihoods although not after the block break (stim iter = 6) 
    # bc that's tantamount to mixing in noise  
    if (these_stim_iters[tib] == 6) {
      directed_choice_probs <- rl_liks 
    } else {
      directed_choice_probs <- W_wm*wm_liks + (1-W_wm)*rl_liks
    }
    
    # Add in undirected noise/lapse rate 
    probs <- epsilon_t*rep(1/3, 3) + (1-epsilon_t)*directed_choice_probs
    
    # Softmax is so high that the prob will sometimes underflow even with LSE trick 
    # so when that's the issue replace with a tiny value  
    replace_probs <- 0
    threshold <- .000000001
    
    if (probs[1] < threshold || probs[2] < threshold || probs[3] < threshold) {
      probs[which(probs < threshold)] <- threshold
    }
       
    if (sim_opt=="opt") {
      trial_nlls <- -log(probs)
      assert("Probabilities must sum to 1", sum(probs) > .99999999 && sum(probs) < 1.00000001)
      assert("NLL cannot include NaN", all(!is.nan(trial_nlls)))
    }
    
    # If simulating, simulate an action..
    if (sim_opt=="sim") {
      # As above, for sim we abritarily always let action 1 == correct, action 2 == middle, action 3 == worst,
      # corresponding to 1,2,3 row indices respectively 
      rand_value <- runif(1, 0, 1)
      if (rand_value < probs[1]) {
        action <- 1
        actions[tib] <- action
        these_rewards[tib] <- 1
        these_neutrals[tib] <- 0
        these_punishments[tib] <- 0
      } else if (rand_value < probs[1] + probs[2]) {
        action <- 2
        actions[tib] <- action
        these_rewards[tib] <- 0
        these_neutrals[tib] <- 1
        these_punishments[tib] <- 0
      } else {
        action <- 3
        actions[tib] <- action
        these_rewards[tib] <- 0
        these_neutrals[tib] <- 0
        these_punishments[tib] <- -1
      }
    } else { # If optimizing, just pull from data
      action <- these_actions[tib]
    } # End sim-opt conditioanl
    
    # Code a valenced outcome variable to use as the overall reward signal 
    if (these_rewards[tib] != 0) {
      outcome <- these_rewards[tib]
    } else if (these_punishments[tib] != 0) {
      outcome <- these_punishments[tib] 
    } else {
      outcome <- 0
    }
    
    # LEARN # 
    # Working memory 
    Q_mat_wm[stim, action] <- outcome
    
    # Add a bonus to any items that are clearly not punishment  
    if (outcome == 1) {
      # A neg bonus to non-rew items since those then more likely to be punish 
      Q_mat_wm[stim, -action] <- Q_mat_wm[stim, -action] - not_pun_bonus
    }
    
    if (outcome == 0) {
      # To neutral in the case that's the chosen action
      Q_mat_wm[stim, action] <- not_pun_bonus
    }

    if (outcome == -1) {
      # To the non punish indices whenever punish  
      Q_mat_wm[stim, -action] <- Q_mat_wm[stim, -action] + not_pun_bonus
    }

    # Bound all WM values at 1
    gr1_idx <- which(Q_mat_wm[stim, ] > 1)
    Q_mat_wm[stim, gr1_idx] <- 1
    
    # Define this trial's alpha  
    alpha_t <- NULL
    chosen_rl_q_value <- Q_mat_rl[stim, action]
    rpe <- outcome - chosen_rl_q_value
    
    if (rpe < 0) {
      alpha_t <- alpha_neg 
    } else {
      alpha_t <- alpha_pos
    }
    
    # Set up reinforcement learning 
    alpha_scaled_down <- NULL 
    ## Implement a rule so RL doesn't get enlisted if there's no delay and the prior trial was correct. 
    # Otherwise  implement standard RL  
    # If delay is NA then it's the first trial of phase, so learn by standard RL  
    if (is.na(delay_t)) {
      rl_out <- LearnByRLStandard(outcome, stim, action, Q_mat_rl, alpha_t)
    # Otherwise if delay is 0 and prior trial correct, don't enlist RL  
    } else { 
        
      rl_off_t <- 1#NULL
      #rl_off_t <- if_else(delay_t == 0, rl_off, rl_off/delay_t)
      
      alpha_scaled_down <- alpha_t * (1-rl_off_t)
      
      rl_out <- LearnByRLStandard(outcome, stim, action, Q_mat_rl, alpha=alpha_scaled_down) 
      
    } 
    
    if (is.na(delay_t)) assert(these_stim_iters[tib] == 1 || these_stim_iters[tib] == 6)
    
    # # Reinforcement learning 
    # rl_out <- LearnByRLStandard(outcome, stim, action, Q_mat_rl, alpha)
    Q_mat_rl <- rl_out[["Q_mat_tplus1"]]
    rpe <- rl_out[["rpe"]]
    
    # Decay working memory # 
    Q_mat_wm <- Q_mat_wm + phi*(Q_mat_wm_init_values-Q_mat_wm)
    
    # STORE # 
    if (sim_opt=="sim") {
      rpes[tib] <- rpe
    }
    
    if (sim_opt=="opt") learn_neg_log_liks[tib] <- trial_nlls[action] # For optimization
    
  } # End loop through trials 
  
  if (sim_opt=="sim") {
    #Code rpe group
    rpe_grp[which(rpes < 0)] <- "negative"
    rpe_grp[which(rpes > 0)] <- "positive"
    rpe_grp[which(rpes == 0)] <- "none"
  }
  
  # Label the final RL Q matrix   
  final_Q_mat_rl <- Q_mat_rl

  first_phase_2_test_trial <- min(which(test_block$phase==2))
  
  ## Loop through test trials  
  for (testib in 1:nrow(test_block)) { 
    
    test_stim <- test_stim_id[testib] 
    
    # Find q-values for the trial, drawing from those that had been learned in phase 1 or phase 2 
    # for the appropriate test phase where that information was accessible  
    these_test_q_rl_vals <- NULL 
    if (testib < first_phase_2_test_trial) {
      these_test_q_rl_vals <- phase_1_Q_mat_rl[test_stim, ]
    } else {
      these_test_q_rl_vals <- final_Q_mat_rl[test_stim, ]
    }
    
    # Test directed probs just based on RL   
    test_probs <- unlist(RunLSESoftmax(these_test_q_rl_vals))
    
    if (sim_opt=="sim") {
      # As above, for sim we abritarily always let action 1 == correct, action 2 == middle, action 3 == worst,
      # corresponding to 1,2,3 row indices respectively 
      test_rand_value <- runif(1, 0, 1)
      if (test_rand_value < test_probs[1]) {
        test_action <- 1
        test_actions[testib] <- test_action
        these_test_rewards[testib] <- 1
        these_test_neutrals[testib] <- 0
        these_test_punishments[testib] <- 0
      } else if (test_rand_value < test_probs[1] + test_probs[2]) {
        test_action <- 2
        test_actions[testib] <- test_action
        these_test_rewards[testib] <- 0
        these_test_neutrals[testib] <- 1
        these_test_punishments[testib] <- 0
      } else {
        test_action <- 3
        test_actions[testib] <- test_action
        these_test_rewards[testib] <- 0
        these_test_neutrals[testib] <- 0
        these_test_punishments[testib] <- -1
      }
    } else {
      test_action <- these_test_actions[testib]
    }
    if (sim_opt=="opt") {
      test_trial_nlls <- -log(test_probs)
      test_neg_log_liks[testib] <- test_trial_nlls[test_action]
    }
    
  }
  
if (sim_opt=="sim") {
  assert("Choices must be *either* reward, neutral, or punishment", 
         all(these_rewards+these_neutrals+(-these_punishments)==1))
  
  learn_df <- data.frame(
    "actions"=actions,
    "corrects"=these_rewards,
    "worsts"=-these_punishments, # sign back to 1 for just indicating whether worst or not
    "neutrals"=these_neutrals,
    "rpes"=as.numeric(rpes),
    "rpe_grp"=rpe_grp,
    "stim_iter"=these_stim_iters,
    "type"="learning",
    "delay"=these_delays,
    "wm_capacities"=wm_capacities
  )
  
  test_df <- data.frame(
    "actions"=test_actions,
    "corrects"=these_test_rewards,
    "worsts"=-these_test_punishments, # sign back to 1 for just indicating whether worst or not
    "neutrals"=these_test_neutrals,
    "rpes"=NA,
    "rpe_grp"=NA,
    "stim_iter"=NA,
    "type"="test",
    "delay"=NA,
    "wm_capacities"=NA
  )
  
  out_df <- rbind(learn_df, test_df)
  out <- data.frame(out_df, parameters, "set_size"=n_s, "phase"=block$phase, 
                    "stim_set"=block$stim_set, "stim"=block$stim)
  
} else {
  # If optimization, output negative log likelihood so can
  # minimize this 
  neg_log_liks <- c(learn_neg_log_liks, test_neg_log_liks)
  
  assert(!is.na(neg_log_liks))
  out <- sum(neg_log_liks) 
}
out
}