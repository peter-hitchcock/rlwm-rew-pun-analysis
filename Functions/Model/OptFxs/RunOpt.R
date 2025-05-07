RunOpt <- function(subj_combined_df, helpers, mle_model_res=NULL) {
  ### Set up to run all model blocks, where a block refers to all the learning 
  # and test data for a {set size, stim set} combo — eg. set size 5 for stim set A — which picks out 
  # a unique set of 5 stim that actions are made for in train and test  ###

  LoopForOpt <- function(parameters, this_subj, helpers) {
    
    # Find negative log likelihood for all blocks #
    if (helpers$which_model=="RunBasicRLWM") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWM(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWM") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWM(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMCoop") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMCoop(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMCoopEta") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMCoopEta(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMCoopEtaNoImmedDecay") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMCoopEtaNoImmedDecay(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMCoopEtaRule") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMCoopEtaRule(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMCoopEtaRuleLapse") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMCoopEtaRuleLapse(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMCoopEtaRuleBeta") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMCoopEtaRuleBeta(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleBeta") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleBeta(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRule") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRule(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParam") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParam(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1Rule") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1Rule(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleNewDecay") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleNewDecay(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenom") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenom(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomNoDecay") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomNoDecay(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRho") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRho(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoTitrateWM") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoTitrateWM(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoTestEps") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoTestEps(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoPers") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoPers(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoPessWMInits") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoPessWMInits(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLR") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLR(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRInitWM0") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRInitWM0(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRInitWMFree") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRInitWMFree(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRUnchosenUpdates") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRUnchosenUpdates(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRUnchosenUpdatesParContRLOff") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRUnchosenUpdatesParContRLOff(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRUnchosenUpdatesParContRLOff") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRUnchosenUpdatesParContRLOff(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamCapDenomRuleRhoValencedLRUnchosenUpdatesParContRLOffNoSS1Rule") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamCapDenomRuleRhoValencedLRUnchosenUpdatesParContRLOffNoSS1Rule(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunCompleteModel") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunCompleteModel(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunFullModel") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunCompleteModel(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunRLWMP") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunRLWMP(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunRLWMPRewFreeBeta") {
      
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunRLWMPRewFreeBeta(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunRLWMPRewNoAlphaNeg") {
      
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunRLWMPRewNoAlphaNeg(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunRLWMPRew") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunRLWMPRew(parameters, x, helpers)) 
      
      if (helpers$run_empirical_bayes == 1) {
        
        penalty <- -log(mvtnorm::dmvnorm(x=unlist(parameters), mean=param_means, sigma=param_covar))
        # Add penalty here 
        if (penalty > 1e9) penalty <- 1e9 
        
        # Needs to be off in s.R before going to cluster
        # if (helpers$emp_bayes_verbose == 1) { 
        #   print_nll <- 1 
        # cat("\n Penalty", penalty)
        # cat("\n NLL otherwise", sum(unlist(opt_out)))
        # }
        
        opt_out <- sum(unlist(opt_out)) + penalty  
        
      }
      
    }
    
    if (helpers$which_model=="RunRLWMPRewRLCoop") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunRLWMPRewRLCoop(parameters, x, helpers)) 
      
    }
    
    if (helpers$which_model=="RunHWMPRew") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunHWMPRew(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunHWMPRewNoCKOff") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunHWMPRewNoCKOff(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunHWMPRew0Inits") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunHWMPRew0Inits(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunRLWMPNeutBonus") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunRLWMPNeutBonus(parameters, x, helpers)) 
    }
    
    if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRUnchosenUpdatesPar") {
      opt_out <-
        lapply(split(this_subj, list(this_subj$set_size, this_subj$stim_set)), function(x) RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRUnchosenUpdatesPar(parameters, x, helpers)) 
      
      if (helpers$run_empirical_bayes == 1) {
        
        penalty <- -log(mvtnorm::dmvnorm(x=unlist(parameters), mean=param_means, sigma=param_covar))
        # Add penalty here 
        if (penalty > 1e9) penalty <- 1e9 
        
        # Needs to be off in s.R before going to cluster
        # if (helpers$emp_bayes_verbose == 1) { 
        #   print_nll <- 1 
          cat("\n Penalty", penalty)
          cat("\n NLL otherwise", sum(unlist(opt_out)))
        # }
        
        opt_out <- sum(unlist(opt_out)) + penalty  
        
      }
    }
    
    if (helpers$print_nll == 1) print(sum(as.numeric(unlist(opt_out))))
    
  # Return summed nll to optimizaiton function #
  sum(as.numeric(unlist(opt_out)))
  }
  
  epsilon_custom_ub <- 1 # Changed from RunBasicRLWM0
  kappa_custom_lb <- 2
  kappa_custom_ub <- 5
  kappa_init <- runif(1, kappa_custom_lb, kappa_custom_ub)
  
  epsilon_init <- runif(1, 0, epsilon_custom_ub)
  
    if (helpers$run_empirical_bayes == 0) { 
      
      all_results_general <- foreach (i = 1:helpers$mle_iters) %do% {
        
        if (helpers$which_model=="RunBasicRLWM") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 3
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            kappa_custom_lb,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        } 
        
        
        if (helpers$which_model=="RunBasicRLWMDelayWM") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 3
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        } 
        
        if (helpers$which_model=="RunBasicRLWMDelayWMCoop") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 3
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        } 
       
        if (helpers$which_model=="RunBasicRLWMDelayWMCoopEta") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 4
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        } 
        
        if (helpers$which_model=="RunBasicRLWMDelayWMCoopEtaNoImmedDecay") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 4
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        } 
        
        if (helpers$which_model=="RunBasicRLWMDelayWMCoopEtaRule") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 4
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        } 
        
        if (helpers$which_model=="RunBasicRLWMDelayWMCoopEtaRuleLapse") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 5
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        } 
        
        if (helpers$which_model=="RunBasicRLWMDelayWMCoopEtaRuleBeta") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 5
          
          params <- c(
            kappa_init,
            runif(1, 2, 100),
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            0,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            100,
            rep(1, n_standard_pars)
          )
          
        } 
        
        if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleBeta") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 3
          
          params <- c(
            kappa_init,
            runif(1, 2, 20),
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            0,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            100,
            rep(1, n_standard_pars)
          )
          
        } 
        
        if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRule") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 3
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        } 
        
        if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParam") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 4
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        } 
        
        if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1Rule") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 4
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        } 
        
        
        if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleNewDecay") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 4
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        }
        
        if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenom") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 4
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        }
        
        if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRho") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 5
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        }
        
        if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoTitrateWM") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 5
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        }
        
        if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoTestEps") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 5
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        }
        
        if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoPessWMInits") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 5
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        }
        
        if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoPers") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 6
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        }
        
        if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLR") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 6
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        }
        
        if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRInitWM0") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 6
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        }
        
        if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRInitWMFree") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 7
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        }
        
        if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRUnchosenUpdates") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 6
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        }
        
        if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRUnchosenUpdatesPar") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 7
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
          # new_params <- c(
          #   runif(1, 1, 5),
          #   runif(n_standard_pars, 0, 1)
          # )  
          
        }
        
        if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRUnchosenUpdatesParContRLOff") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 7
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
          # new_params <- c(
          #   runif(1, 1, 5),
          #   runif(n_standard_pars, 0, 1)
          # )  
          
        }
        
        if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamCapDenomRuleRhoValencedLRUnchosenUpdatesParContRLOffNoSS1Rule") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 7
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
          # new_params <- c(
          #   runif(1, 1, 5),
          #   runif(n_standard_pars, 0, 1)
          # )  
          
        }
        
        if (helpers$which_model=="RunCompleteModel") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 7
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        }
        
        if (helpers$which_model=="RunFullModel") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 7
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        }
        
        if (helpers$which_model=="RunRLWMP") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 7
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        }
        
        if (helpers$which_model=="RunRLWMPRew") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 7
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        }
        
        if (helpers$which_model=="RunRLWMPRewRLCoop") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 7
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        }
        
        if (helpers$which_model=="RunRLWMPRewNoAlphaNeg") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 6
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        }
        
        if (helpers$which_model=="RunRLWMPRewFreeBeta") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 7
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1),
            50
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars),
            0
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars), 
            100
          )
          
        }
        
        if (helpers$which_model=="RunHWMPRew") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 7
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        }
        
        if (helpers$which_model=="RunHWMPRewNoCKOff") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 6
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        }
        
        if (helpers$which_model=="RunHWMPRew0Inits") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 7
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        }
        
        if (helpers$which_model=="RunRLWMPNeutBonus") {
          
          # Set to control all standard range pars  
          n_standard_pars <- 7
          
          params <- c(
            kappa_init,
            runif(n_standard_pars, 0, 1)
          )
          
          names(params) <- helpers$par_names 
          
          lower_bound <- c(
            1,
            rep(0, n_standard_pars)
          )
          
          upper_bound <- c(
            kappa_custom_ub,
            rep(1, n_standard_pars)
          )
          
        }
        res <-
          #tryCatch(error=function(cnd) NA,
          Rsolnp::solnp(
            params,
            fun = function(params) { LoopForOpt(params, subj_combined_df, helpers) },
            LB = lower_bound,
            UB = upper_bound,
            control = list(trace = 0, maxit=5e4) )
        #), silent=TRUE)
        if (helpers$print_nll == 1) { cat("\nRes"); print(res) }
        
        res
        } # End of mle iters
    
    } else if (helpers$run_empirical_bayes == 1) { 
      
      all_eb_results <- foreach (i %in% 1:helpers$eb_iters) %do% {

      # Get means and covariance matrix from MLE fit 
      param_means <- unlist(colMeans(mle_model_res %>% select(helpers$par_names)))
      param_covar <- cov(mle_model_res %>% select(helpers$par_names))
      #cat("\n Inside run opt")
      # Testing if can get here 
      
      if (helpers$which_model=="RunBasicRLWMDelayWMRLEnlistRuleParamSS1RuleCapDenomRuleRhoValencedLRUnchosenUpdatesPar") {
        
        # Set to control all standard range pars  
        n_standard_pars <- 7
        
        
        lower_bound <- c(
          1,
          rep(0, n_standard_pars)
        )
        
        upper_bound <- c(
          kappa_custom_ub,
          rep(1, n_standard_pars)
        )
        
        new_params <- c(
          runif(1, 1, 5),
          runif(n_standard_pars, 0, 1)
        )
        
        names(new_params) <- helpers$par_names 
        
        
      }
      
      
      if (helpers$which_model=="RunRLWMPRew") {
        
        n_standard_pars <- 7
        
        
        lower_bound <- c(
          1,
          rep(0, n_standard_pars)
        )
        
        upper_bound <- c(
          kappa_custom_ub,
          rep(1, n_standard_pars)
        )
        
        new_params <- c(
          runif(1, 1, 5),
          runif(n_standard_pars, 0, 1)
        )
        
        names(new_params) <- helpers$par_names 
        
      }
      
      # if (helpers$which_model == "RunMQLearnerDiffDecayToPessPriorCKDecay") {
      # 
      # }
      
      # Rerun as empirical Bayes
      eb_res <- 
        Rsolnp::solnp(
          new_params,
          fun = function(new_params) { LoopForOpt(new_params, subj_combined_df, helpers) },
          LB = lower_bound,
          UB = upper_bound,
          control = list(trace = 0, maxit=5e4)
        )
      
      eb_res
      } #End of EB run s 
      
    }

  if (helpers$run_empirical_bayes == 0) {
    all_results_general <- all_results_general
  } else {
    all_results_general <- all_eb_results  
  }
  
  # ... and loop through list to take out any NAs 
  collect_valid <- list()
  for (li in 1:length(all_results_general)) {
    this_opt <- all_results_general[[li]]
    if (!is.null(names(this_opt))) {
      collect_valid[[li]] <- this_opt
    }
  }
  
# Return just the ones without NAs
collect_valid
  
}