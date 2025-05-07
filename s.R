which_model <- 46
cluster <- 1#0 #^PUT BACK - but change for PR run locally  
testing <- 0
# Streamlined version for public repo - 10/11/24  
sim_opt <- "opt" 
lesion_sim <- "n" # Whether to run final model with lesions to show key components — these are then saved with a different name  
par_recovery <- "n"
if (par_recovery == "y") {
  sim_multiv_gauss <- 1
  n_pr_pars <- 100 # How many parameter sets to simulate/try to recov
  cluster <- 0 # Running this locally because it's not comp intensive, so use local # of cores
  # * Also make sure testing is set correctly! * — as there is a PR conditional that uses it    
  sim_opt <- "" # Will get dynamically set in the recovery routine — so for now skip those parts of script 
}
# Split halves of data for optimization?   
split_half <- "n" # Should usually be turned off! By putting to empty or "n"  
if (split_half == "y") {
  print("Make sure you meant to run split half!!!")
  which_half <- "B" # Applicable for split half only 
}
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
  "mvtnorm"
), 
require, character=TRUE)
sf <- function() sapply(paste0("./Functions/", list.files("./Functions/", recursive=TRUE)), source) # Source all fxs
sf()

if (sim_opt=="sim") cluster <- 0
if (testing==1) cluster <- 0

if (cluster==1) {
  registerDoParallel(cores=40)  
} else {
  #registerDoParallel(cores=round(detectCores()*2/3))
  registerDoParallel(cores=round(detectCores()*1/3))
}

# Read in the data, either with or without delay  
learn_df <- read.csv("../data/learn_df.csv")
test_df <- read.csv("../data/test_df.csv")

if (split_half == "y") {
  learn_df <- learn_df %>% filter(stim_set==which_half)
  test_df <- test_df %>% filter(stim_set==which_half)
}
# Hack to allow feeding in joint dataset (just need stim iter for learn df)
test_df$stim_iter <- rep(NA, nrow(test_df))
test_df$delay <- rep(NA, nrow(test_df)) 

IDs <- unique(learn_df$ID)
# To prevent overwriting on save  
rand_part <- round(runif(1, 1e4, 9e4), 0)

# Put some routine parameters into a list called helpers to send into functions  
helpers <- list()
# ALWAYS START THIS AT 0 because will always start w running MLE first — then this will get 
# turned on when actually ready to run empirical Bayes  
helpers$run_empirical_bayes <- 0
helpers$sim_opt <- sim_opt
helpers$print_nll <- 0
helpers$turn_on_debug_info <- 1
# Whether want to run empirical Bayes
helpers$empirical_bayes <- 1
helpers$mle_iters <- 20
helpers$eb_iters <- 20
helpers$par_recovery <- par_recovery

# Set up save paths
best_path <- "../model_res/opt/best/BEST_"
all_path <- "../model_res/opt/all/ALL_"
sim_path <- "../model_res/sims/SIM_"

# Paths for reading in extant files  
bp <- "../model_res/opt/best/"
allp <- "../model_res/opt/all/"
sp <- "../model_res/sims/"
# Read model function 
rm <- function(path, model_str) read.csv(paste0(path, model_str))

par_names <- NULL 

# Get vars needed for opt/sim to subset  
opt_vars <- c("correct", "neutral", "worst", "action", # <- opt only vars
              "stim", "stim_iter", "set_size", "phase", "stim_set", "ID", "delay") # <- experimental params
sim_vars <- c("stim", "stim_iter", "set_size", "phase", "stim_set", "ID", "delay")

if (which_model == 35) {
  helpers$which_model <- "RunRLWMPRew"
  
  helpers$par_names <- c(
    # Kappa has non-standard range and must always come first  
    "kappa",
    # Remaining have standard ranges 
    "alpha_pos",
    "alpha_neg",
    "phi",
    "rho",
    "rl_off",
    "epsilon",
    "not_pun_bonus"
  )
  
  if (sim_opt=="sim" || 
      # .. or to define range for recovery  
      par_recovery == "y") {
    fit_df <- read.csv("../model_res/opt/best/BEST__m35_RunRLWMPRew-8-1-24-epsilonfixed.csv")
  }
  
}

# Same as m34 but only bonusing neutral  
if (which_model == 36) {
  helpers$which_model <- "RunRLWMPNeutBonus"
  
  helpers$par_names <- c(
    # Kappa has non-standard range and must always come first  
    "kappa",
    # Remaining have standard ranges 
    "alpha_pos",
    "alpha_neg",
    "phi",
    "rho",
    "rl_off",
    "epsilon",
    "not_pun_bonus"
  )
  
}

# Same as m35 but set RL off to 0  
if (which_model == 37) {
  helpers$which_model <- "RunRLWMPRewLesionRLOff"
  
  helpers$par_names <- c(
    # Kappa has non-standard range and must always come first  
    "kappa",
    # Remaining have standard ranges 
    "alpha_pos",
    "alpha_neg",
    "phi",
    "rho",
    "rl_off",
    "epsilon",
    "not_pun_bonus"
  )
 
  if (sim_opt=="sim" || 
      # .. or to define range for recovery  
      par_recovery == "y") {
    fit_df <- read.csv("../model_res/opt/best/BEST__m35_RunRLWMPRew-8-1-24-epsilonfixed.csv")
  }
  
}

# Same as m35 but set RL off to 1    
if (which_model == 38) {
  helpers$which_model <- "RunRLWMPRewRLOffToMax"
  
  helpers$par_names <- c(
    # Kappa has non-standard range and must always come first  
    "kappa",
    # Remaining have standard ranges 
    "alpha_pos",
    "alpha_neg",
    "phi",
    "rho",
    "rl_off",
    "epsilon",
    "not_pun_bonus"
  )
  
  if (sim_opt=="sim" || 
      # .. or to define range for recovery  
      par_recovery == "y") {
    fit_df <- read.csv("../model_res/opt/best/BEST__m35_RunRLWMPRew-8-1-24-epsilonfixed.csv")
  }
  
}

# Same as m35 but with not pun lesion 
if (which_model == 39) {
  helpers$which_model <- "RunRLWMPRewLesionNotPun"
  
  helpers$par_names <- c(
    # Kappa has non-standard range and must always come first  
    "kappa",
    # Remaining have standard ranges 
    "alpha_pos",
    "alpha_neg",
    "phi",
    "rho",
    "rl_off",
    "epsilon",
    "not_pun_bonus"
  )
  
  if (sim_opt=="sim" || 
      # .. or to define range for recovery  
      par_recovery == "y") {
    fit_df <- read.csv("../model_res/opt/best/BEST__m35_RunRLWMPRew-8-1-24-epsilonfixed.csv")
  } 
}

# Same as m35 but with not alpha neg set to alpha pos  
if (which_model == 40) {
  helpers$which_model <- "RunRLWMPRewLesionAlphaNeg"
  
  helpers$par_names <- c(
    # Kappa has non-standard range and must always come first  
    "kappa",
    # Remaining have standard ranges 
    "alpha_pos",
    "alpha_neg",
    "phi",
    "rho",
    "rl_off",
    "epsilon",
    "not_pun_bonus"
  )
  
  if (sim_opt=="sim" || 
      # .. or to define range for recovery  
      par_recovery == "y") {
    fit_df <- read.csv("../model_res/opt/best/BEST__m35_RunRLWMPRew-8-1-24-epsilonfixed.csv")
  } 
}

# Same as m35 but with a biased choice kernel a la Collins 24 
if (which_model == 41) {
  helpers$which_model <- "RunHWMPRew"
  
  helpers$par_names <- c(
    # Kappa has non-standard range and must always come first  
    "kappa",
    # Remaining have standard ranges 
    # "alpha_pos",
    # "alpha_neg",
    "learning_bias",
    "alpha_ck",
    "phi",
    "rho",
    "ck_off",
    "epsilon",
    "not_pun_bonus"
  )
  
  if (sim_opt=="sim" || 
      # .. or to define range for recovery  
      par_recovery == "y") {
    fit_df <- read.csv("../model_res/opt/best/BEST__m41_RunHWMPRew.csv")
  }
  
}

# Same as m41 but w CK inits=0 for most direct comparison to m35 
if (which_model == 42) {
  helpers$which_model <- "RunHWMPRew0Inits"
  
  helpers$par_names <- c(
    # Kappa has non-standard range and must always come first  
    "kappa",
    # Remaining have standard ranges 
    # "alpha_pos",
    # "alpha_neg",
    "learning_bias",
    "alpha_ck",
    "phi",
    "rho",
    "ck_off",
    "epsilon",
    "not_pun_bonus"
  )
  
}

## Models run for R1 ## 
# Same as m35 but w beta as a free par  
if (which_model == 43) {
  helpers$which_model <- "RunRLWMPRewFreeBeta"
  
  helpers$par_names <- c(
    # Kappa has non-standard range and must always come first  
    "kappa",
    # Remaining have standard ranges 
    "alpha_pos",
    "alpha_neg",
    "phi",
    "rho",
    "rl_off",
    "epsilon",
    "not_pun_bonus",
    "beta"
  )
  
}

if (which_model == 44) {
  helpers$which_model <- "RunRLWMPRewNoAlphaNeg"
  
  helpers$par_names <- c(
    # Kappa has non-standard range and must always come first  
    "kappa",
    # Remaining have standard ranges 
    "alpha_pos",
    "phi",
    "rho",
    "rl_off",
    "epsilon",
    "not_pun_bonus"
  )
  
}

if (which_model == 45) {
  helpers$which_model <- "RunHWMPRewNoCKOff"
  
  helpers$par_names <- c(
    # Kappa has non-standard range and must always come first  
    "kappa",
    # Remaining have standard ranges 
    # "alpha_pos",
    # "alpha_neg",
    "learning_bias",
    "alpha_ck",
    "phi",
    "rho",
    #"ck_off",
    "epsilon",
    "not_pun_bonus"
  )
  
}

# Same as m35 but with a cooperative rather than competitive WM contribution a al Collins 18 PNAS 
# (albeit it still effectively leads to a lower RL learning rate when WM is high)  
if (which_model == 46) {
  helpers$which_model <- "RunRLWMPRewRLCoop"
  
  helpers$par_names <- c(
    # Kappa has non-standard range and must always come first  
    "kappa",
    # Remaining have standard ranges 
    "alpha_pos",
    "alpha_neg",
    "phi",
    "rho",
    "eta",
    "epsilon",
    "not_pun_bonus"
  )

}

#### OPTIMIZATION TEST BED ####  
if (testing == 1) {
  
  helpers$mle_iters <- 10
  i <- 11
  
  # Get empirical data  
  this_learn_df <- learn_df %>% filter(ID == IDs[i]) %>% select(all_of(opt_vars))
  this_test_df <- test_df %>% filter(ID == IDs[i])  %>% select(all_of(opt_vars))
  this_learn_df$type <- "learning"
  this_test_df$type <- "test"
  one_comb_df <- rbind(this_learn_df, this_test_df)
  
  # Run test optimization 
  if (sim_opt == "opt") {
    helpers$print_nll <- 1 # Print nll by default on test runs 
    tmp_opt <- RunOpt(one_comb_df, helpers) 
    print(tmp_opt)  
  } 
}

#### FOR REAL RUNS AFTER TESTING ####  
if (testing == 0) {
  
  ## BEGIN REAL OPTIMIZATION ### 
  if (sim_opt=="opt") { # Kick off optimization 
    
    # Create random string to append to save to prevent overwriting then create final file name  
    if (split_half == "y") {
      this_best_path <- paste0(best_path, "_", helpers$which_model, "_SPLIT_HALF_", which_half, rand_part, ".csv")
    } else {
      this_best_path <- paste0(best_path, "_", helpers$which_model, rand_part, ".csv")  
    }
    
    this_all_path <- paste0(all_path, "_", helpers$which_model, rand_part, ".csv")
    
    model_res_full <- 
      foreach (i = 1:length(IDs)) %dopar% { # PUT BACK  
      #foreach (i = 3) %do% {
        # Get empirical data for this pt 
        this_learn_df <- NULL; this_test_df <- NULL
        this_learn_df <- learn_df %>% filter(ID == IDs[i]) %>% select(all_of(opt_vars))
        this_test_df <- test_df %>% filter(ID == IDs[i]) %>% select(all_of(opt_vars))
        this_learn_df$type <- "learning"
        this_test_df$type <- "test"
        one_comb_df <- rbind(this_learn_df, this_test_df)
        
        out <- NULL
        
        # Run optimization, trying to catch errors if it breaks 
        out <- 
          tryCatch( #^PUT BACK
           error=function(cnd) NA,
           RunOpt(one_comb_df, helpers) 
        )
        
        res_df <- NULL
        
        # Save out the results if the optimization ran successfully 
        
        # This generates an if "(!is.na(out)) has more than one elem" warning when 
        # iter is > 1 but that's okay because it's typically only one-dimension if got 
        # snagged on NA (and even if not will break below anyway)
        #if (!is.na(out)) {
          res_df <- 
            lapply(out, function(x) {
              
              value_vec <- x$values
              # Following RSolnp CRAN documentation the final value is the optimized one
              nll <- value_vec[length(value_vec)]
              # 0 = converged
              convergence <- x$convergence 
              pars <- x$pars 
              
              mini_df_out <- data.table(t(pars), convergence, nll)
              
              mini_df_out
          }) %>% bind_rows()
           
          res_df$ID <- unique(this_learn_df$ID)
          
       #} # End is.na conditional; otherwise res_df is NULL so should still save without issue 
      
      res_df
      } %>% bind_rows() # End foreach 
    
    ## Save out optimization ## 
    # Save out full first in case find best fit breaks — *These are full MLE fits — no empirical bayes yet*   
    write.csv(model_res_full, this_all_path) ##   
    
    # Find the best fit in case of local minima 
    model_res <- data.frame(model_res_full %>% group_by(ID) %>% slice(which.min(nll))) 
    write.csv(model_res, this_best_path)
  }
  ## END REAL OPTIMIZATION ### 
} # End ! test conditional
# BEGIN SIMULATION ###

if (sim_opt == "sim") { # Kick off sim
  
  sim_full <- NULL
  sim_full <-
    foreach (i = 1:length(IDs)) %dopar% {
    #foreach (i = 16) %do% {
      #i <- 3
      # Get data for this pt
      this_learn_df <- NULL; this_test_df <- NULL; this_fit_df <- NULL; sim_out <- NULL
      
      this_learn_df <- learn_df %>% filter(ID == IDs[i]) %>% select(all_of(sim_vars))
      this_test_df <- test_df %>% filter(ID == IDs[i])  %>% select(all_of(sim_vars))
      this_learn_df$type <- "learning"
      this_test_df$type <- "test"
      one_comb_df <- rbind(this_learn_df, this_test_df)
      # Pts best-fit parameters for this model 
      this_fit_df <- fit_df %>% filter(ID==IDs[i]) 
      
      sim_out <-
        RunSim(this_fit_df, one_comb_df, helpers, n_iters=50)
      
    sim_out
    }
  
  full_sim <- list()
  
  # Store sim results in list
  for (s in 1:length(sim_full)) {
    full_sim[[s]] <- sim_full[[s]]
  }
  
  full_sim_df <- full_sim %>% bind_rows()
  
  # Create random string to append to save to prevent overwriting then create final file name  
  if (lesion_sim == "y") {
    this_sim_path <- paste0(sim_path, "_", helpers$which_model, "_LESION_SIM_", rand_part, ".csv")
  } else {
    this_sim_path <- paste0(sim_path, helpers$which_model, rand_part, ".csv")
  }
  ## Save out sim ##
  write.csv(full_sim_df, this_sim_path)

} # End if sim conditional
## END SIMULATION ###


# #### BEGIN PARAMETER RECOVERY ####  
if (par_recovery == "y") {

  ## Set up paths  
  #   pr_sim_path <- "../model_res/par_recov/pr_sims/SIM_"
  pr_opts_all_path <- "../model_res/par_recov/pr_res/par_recov_ALL_"
  pr_opts_best_path <- "../model_res/par_recov/pr_res/par_recov_BEST_"

 just_pars <- fit_df %>% select(helpers$par_names)

  ## Set paths ##
  # pr_sim_path <- paste0(pr_sim_path, helpers$on_what, "_", helpers$which_model, "_", rand_part, ".csv")
  pr_best_path <- paste0(pr_opts_best_path, "_", helpers$which_model, "_", rand_part, ".csv")
  pr_all_path <- paste0(pr_opts_all_path, "_", helpers$which_model, "_", rand_part, ".csv")

  if (sim_multiv_gauss == 0) {
    ## Draw a normally distributed range of parameter values based on the empirical stats ##
    distributed_pars <- foreach (i = 1:ncol(just_pars)) %do% {
      
      this_par <- unlist(just_pars[i])
      
      this_par_sim <- rtruncnorm(n_pr_pars, 
                                 a=min(this_par), b=max(this_par), 
                                 mean=mean(this_par), sd=sd(this_par))
      
      this_par_sim
    }  
    # Package up in dataframe  
    distributed_pars <-
      data.frame(distributed_pars %>% bind_cols()) %>% setNames(names(just_pars))
    
  } else { # Draw from multivaraite  
    
    distributed_pars <- data.frame(rmvnorm(105, mean=colMeans(just_pars), sigma=cov(just_pars)))
    
    # No negative values
    distributed_pars[distributed_pars < 0] <- 0
    
    # Adjust any params out of range
    distributed_pars[which(distributed_pars$kappa > 5), "kappa"] <- 5
    distributed_pars[which(distributed_pars$kappa < 1), "kappa"] <- 1
    
    distributed_pars[which(distributed_pars$phi > 1), "phi"] <- 1
    distributed_pars[which(distributed_pars$rho > 1), "rho"] <- 1
    distributed_pars[which(distributed_pars$rl_off > 1), "rl_off"] <- 1
    #distributed_pars[which(distributed_pars$unchosen_update > 1), "unchosen_update"] <- 1
    distributed_pars[which(distributed_pars$unchosen_update > 1), "not_pun_bonus"] <- 1
  }
  
  helpers$sim_opt <- "sim"
#   ## SIMULATE using those params ... ##
  pr_sims_full <-
    # Loop through the number of recoveries to run 
    foreach (j = 1:n_pr_pars) %do% { 
    #foreach (j = 1:2) %do% {

      # Draw a row of random pars.
      these_pars <- distributed_pars[j, ]#just_pars[sample(nrow(just_pars), 1), ]
      
      #print(these_pars)
      # .. and contingencies 
      IDc <- NULL; this_learn_df <- NULL; this_test_df <- NULL 
      
      IDc <- sample(unique(learn_df$ID), 1)
      
      this_learn_df <- learn_df %>% filter(ID == IDc) %>% select(all_of(sim_vars))
      this_test_df <- test_df %>% filter(ID == IDc) %>% select(all_of(sim_vars))
      
      this_learn_df$type <- "learning"
      this_test_df$type <- "test"
      one_comb_df <- rbind(this_learn_df, this_test_df)
      
      sim_out <-
        RunSim(these_pars, one_comb_df, helpers, n_iters = 1)

  sim_out
  } # End generate simulations
  
  full_pr_sim <- list()
  
  # Store sim results in list
  for (s in 1:length(pr_sims_full)) {
    full_pr_sim[[s]] <- data.frame(pr_sims_full[[s]], "sim"=s)
  }
  
  full_pr_sim_df <- full_pr_sim %>% bind_rows()
  
  ### RECOVER - ie. optimize on these sims ### 
  helpers$sim_opt <- "opt"
  
  pr_model_res_full <-
    #foreach (i = 1:2) %do% {
    foreach (i = 1:n_pr_pars) %dopar% {
    #foreach (i = unique(full_pr_sim_df$sim)) %dopar% {
    # ^PUT BACK AFTER TESTING

      # Get empirical data for this pt
      this_sim_df <- NULL

      # Find the true parameters the generated the simulated data
      generative_pars <- full_pr_sim_df %>%
        filter(sim==i) %>% select((names(these_pars))) %>% unique()

      this_sim_df <- full_pr_sim_df %>%
        filter(sim==i) %>% select(-c(names(these_pars)))
      
      # Make sure that the pars stored in both are same
      assert(generative_pars == full_pr_sim_df %>%
               filter(sim==i) %>% select(c(names(these_pars))) %>% unique())
      pr_out <- NULL

      if (testing==1) { 
        helpers$print_nll <- 1
        helpers$mle_iters <- 2
        helpers$eb_iters <- 2
      } else { 
        # Run twice as many because during opt we run two batches
        #helpers$mle_iters <- 1
        helpers$mle_iters <- helpers$mle_iters*2 
      }

      # Run optimization, trying to catch errors if it breaks
      pr_out <-
        tryCatch(
        error=function(cnd) NA,
          RunOpt(this_sim_df, helpers)
          #RunOpt(this_sim_df, helpers, n_iters=40)
        ) #^ PUT BACK EVERYTHING UP TO TRYCATCH AFTER TESTING

      pr_res_df <- NULL
      
      # Save out the results if the optimization ran successfully
      #if (!is.na(any(pr_out))) {

          these_pars <- NULL
          these_pars <- generative_pars

          pr_res_df <-
            lapply(pr_out, function(x) {

              names(these_pars) <- paste0(names(these_pars), "_simmed")
              value_vec <- x$values
              # Following RSolnp CRAN documentation the final value is the optimized one
              nll <- value_vec[length(value_vec)]
              # 0 = converged
              convergence <- x$convergence
              pars <- x$pars
              names(pars) <- paste0(names(pars), "_recovered")

              mini_pr_df_out <- data.table(t(pars), these_pars, convergence, nll)

            mini_pr_df_out
            }) %>% bind_rows()

#      } # End is.na conditional; otherwise res_df is NULL so should still save without issue

      pr_res_df$sim <- i

    pr_res_df
    } %>% bind_rows() # End optimization loop
  
  # ## Save out results ##
  # Save out full first in case find best fit breaks
  #write.csv(pr_model_res_full, pr_all_path) #^PUT BACK
  pr_model_res <- pr_model_res_full %>% group_by(sim) %>% slice(which.min(nll))
  
  write.csv(pr_model_res, pr_best_path) #this_best_path)

  if (helpers$empirical_bayes == 1) {
    # Turn this on so that the optimization function will get routed to empirical bayes
    helpers$run_empirical_bayes <- 1 
    
    eb_pr_res_full <- 
      #foreach (i = unique(full_pr_sim_df$sim)) %dopar% { # PUT BACK  
      foreach (i = 1:n_pr_pars) %dopar% {
      #foreach (i = 1:n_pr_pars) %do% { # Needs to be \leq the number from the MLE PR loop
        
        # Get simulated data for this pt 
        this_sim_df <- NULL
        
        this_sim_df <- full_pr_sim_df %>%
          filter(sim==i) %>% select(-c(names(these_pars)))
        
        generative_pars <- full_pr_sim_df %>% 
          filter(sim==i) %>% select((names(these_pars))) %>% unique()
        
        if (testing==1) helpers$print_nll <- 1
        
        eb_pr_out <- NULL
        
        mle_recovered <- data.frame(pr_model_res %>% select(contains("recovered")))
        names(mle_recovered) <- 
          unlist(strsplit(names(mle_recovered), "_recovered"))
        
        # All working on the outer side for empirical bayes til this point  
        # Run all subjects again using the emp bayes penalty 
        eb_pr_out <-
          tryCatch( #PUT BACK
            error=function(cnd) NA,
            RunOpt(this_sim_df, helpers, mle_model_res=mle_recovered)
          )
        print(eb_pr_out)
        
        if (!is.na(eb_pr_out)) {
          eb_pr_res_df <- 
            lapply(eb_pr_out, function(x) {
              
              value_vec <- x$values
              # Following RSolnp CRAN documentation the final value is the optimized one
              nll <- value_vec[length(value_vec)]
              
              convergence <- x$convergence 
              
              pars <- x$pars 
              
              names(pars) <- helpers$par_names 
              
              names(pars) <- paste0(names(pars), "_EB_recovered")
              
              mini_eb_pr_df_out <- data.table(t(pars), "eb_convergence"=convergence, "eb_nll"=nll)
              print(mini_eb_pr_df_out)
              
              mini_eb_pr_df_out
            }) %>% bind_rows()
        } else {
          eb_pr_res_df <- NA
        }
        
        this_sim_full_recovs_out <- 
          data.frame(eb_pr_res_df, data.frame(pr_model_res %>% filter(sim==i)), "eb_ID"=unique(this_sim_df$ID))
        
        this_sim_full_recovs_out$sim <- i
        
        this_sim_full_recovs_out   
      } #%>% bind_rows()
    cat("\n Made it here")
    
    eb_pr_res_full <- eb_pr_res_full %>% bind_rows()
    eb_pr_model_res <- data.frame(eb_pr_res_full %>% group_by(sim) %>% slice(which.min(nll))) 
    
    write.csv(eb_pr_model_res, pr_best_path)   
    
  } else { # If not EB  
    write.csv(pr_model_res, this_best_path)    
  } 
#
} # End param recover conditional
# 
