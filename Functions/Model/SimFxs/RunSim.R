RunSim <- function(this_fit_df, this_subj, helpers, n_iters=1) {
  ### Simulate on one subject's data both training and test ###
  
  if (helpers$par_recovery != "y") {
    parameters <- this_fit_df %>% select(helpers$par_names)
  } else if (helpers$par_recovery == "y") {
    parameters <- this_fit_df 
  }
  
  sim_out_one_subj <- foreach (i = 1:n_iters) %do% {
    
    if (helpers$which_model == "RunBasicRLWM") {
      this_sim <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWM(parameters, x, helpers)) 
      
      #this_sim <- RunBasicRLWM(parameters, this_comb_df, helpers)  
    }
    
    if (helpers$which_model == "RunBasicRLWMDelayWM") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWM(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunBasicRLWMDelayWMCoop") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMCoop(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunBasicRLWMDelayWMCoopEta") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMCoopEta(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunBasicRLWMDelayWMCoopEtaNoImmedDecay") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMCoopEtaNoImmedDecay(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunBasicRLWMDelayWMCoopEtaRule") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMCoopEtaRule(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunBasicRLWMDelayWMCoopEtaRuleLapse") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMCoopEtaRuleLapse(parameters, x, helpers)) 
    }
  
    if (helpers$which_model == "RunBasicRLWMDelayWMCoopEtaRuleBeta") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMCoopEtaRuleBeta(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunBasicRLWMDelayWMRLEnlistRuleBeta") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleBeta(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunBasicRLWMDelayWMRLEnlistRule") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRule(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunBasicRLWMDelayWMRLEnlistRuleParam") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParam(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunBasicRLWMDelayWMRLEnlistRuleParamSS1Rule") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1Rule(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleNewDecay") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleNewDecay(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenom") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenom(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRho") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRho(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoTitrateWM") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoTitrateWM(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoTestEps") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoTestEps(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoPers") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoPers(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoPessWMInits") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoPessWMInits(parameters, x, helpers)) 
    }
  
    if (helpers$which_model == "RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLR") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLR(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRInitWM0") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRInitWM0(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRInitWMFree") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRInitWMFree(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRUnchosenWMUpdate") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRUnchosenWMUpdate(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRUnchosenUpdates") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRUnchosenUpdates(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRUnchosenUpdatesPar") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRUnchosenUpdatesPar(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "FinalModelLesionRLOff") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) FinalModelLesionRLOff(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "FinalModelLesionRLOffToMax") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) FinalModelLesionRLOffToMax(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "FinalModelLesionUnchosenUpdateOff") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) FinalModelLesionUnchosenUpdateOff(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "FinalModelLesionDontUseNonUpdateRule") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) FinalModelLesionDontUseNonUpdateRule(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "FinalModelLesionAlphaNeg") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) FinalModelLesionAlphaNeg(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunBasicRLWMDelayWMRLEnlistRuleParamCapDenomRuleRhoValencedLRUnchosenUpdatesParContRLOffNoSS1Rule") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamCapDenomRuleRhoValencedLRUnchosenUpdatesParContRLOffNoSS1Rule(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRUnchosenUpdatesParContRLOff") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRUnchosenUpdatesParContRLOff(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunRLWMP") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunRLWMP(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunRLWMPRew") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunRLWMPRew(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunRLWMPRewLesionRLOff") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunRLWMPRewLesionRLOff(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunRLWMPRewRLOffToMax") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunRLWMPRewRLOffToMax(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunRLWMPRewLesionNotPun") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunRLWMPRewLesionNotPun(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunRLWMPRewLesionAlphaNeg") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunRLWMPRewLesionAlphaNeg(parameters, x, helpers)) 
    }
    
    if (helpers$which_model == "RunHWMPRew") {
      this_sim <- 
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunHWMPRew(parameters, x, helpers)) 
    }
    
  this_sim  
  } 
  
  # Bind up to send out 
  sim_df <- 
    foreach (i=1:n_iters) %do% { data.frame(sim_out_one_subj[[i]] %>% bind_rows(), "iter"=i) } %>% bind_rows()
  sim_df$ID <- this_subj$ID[1]
  
sim_df
}