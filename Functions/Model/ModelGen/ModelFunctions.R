RunSoftMax <- function(action_values, beta=100) {
  
  ### Runs softmax choice function and returns ###
  num_1 <- exp(beta*action_values[1])
  num_2 <- exp(beta*action_values[2])
  num_3 <- exp(beta*action_values[3])
  denom <- sum(exp(beta*action_values[1]), exp(beta*action_values[2]), exp(beta*action_values[3]))
  
  prob_val1 <- num_1/denom
  prob_val2 <- num_2/denom
  prob_val3 <- num_3/denom
  
list(prob_val1, prob_val2, prob_val3)  
}

RunLSESoftmax <- function(action_values, # vector to apply softmax to 
                          debug_info, 
                          verbose=0, # for printouts
                          beta=100, # 100 a la Master et al 20  
                          identifier='default' # optional ident for debugging on spec SM calls
) {
  
  ### Performs softmax using log sum exp trick. Returns softmax(vec) ###
  
  # Pull out the index of the max term, breaking ties by just taking first
  beta <- as.numeric(beta)
  mi <- which(action_values == max(action_values))[1]
  
  # Demonominator (constant)
  term2 <- beta * action_values[mi] + # max term 
    log(sum(exp(beta * action_values - beta * action_values[mi])))
  
  # Preallocate vector of softmax outputs
  sm_vec <- rep(NA, length(action_values))
  # Numerators
  term1_vec <- beta * action_values
  
  # Calc softmax for each elem 
  for (i in seq_along(action_values)) sm_vec[i] <- term1_vec[i] - term2
  
  # Break likelihood in case of over/underflow
  if (any(sm_vec > 0)) sm_vec <- rep(NA, 3)
  if (verbose) cat('\n Log liks', sm_vec)
  
exp(sm_vec) # Return as probabilities  
}

LearnByRLStandard <- function(reward, stim, a, Q_mat_rl, alpha, debug_info=NULL) {
  ### Calculate RPE and update Q value matrix ###
  
  rpe <- (reward - Q_mat_rl[stim, a])
  Q_mat_rl[stim, a] <- Q_mat_rl[stim, a] + alpha*rpe
  
list("Q_mat_tplus1"=Q_mat_rl, "rpe"=rpe)
}

LearnByCK <- function(stim, a, CK_mat, alpha_ck, debug_info=NULL) {
  ### Straight S-R learning, so always gets a +PE (or 0 if 1) for chosen action
  # Calculate CK RPE and update CK value matrix ###
  
  ck_rpe <- (1 - CK_mat[stim, a])
  assert(ck_rpe >= 0)
  CK_mat[stim, a] <- CK_mat[stim, a] + alpha_ck*ck_rpe
  
list("CK_mat_tplus1"=CK_mat, "ck_rpe"=ck_rpe)
}
# LearnByRLCooperative <- function(reward, stim, a, Q_mat_rl, alpha,  Q_wm_sa, W_wm, tib=0) {
#   ### Cooperative RLWM model where the expectation is jointly formed by the Q_RL(s,a) and Q_WM(s,a) ###
#   
#   joint_expectation <- (1-W_wm)*Q_mat_rl[stim, a] + W_wm*Q_wm_sa
#   
#   rpe <- (reward - joint_expectation)
#   #browser(expr=abs(rpe) > 1)
#   Q_mat_rl[stim, a] <- Q_mat_rl[stim, a] + alpha*rpe
#   
# list("Q_mat_tplus1"=Q_mat_rl, "rpe"=rpe)
# }


LearnByRLCooperativeSimple <- function(reward, stim, a, Q_mat_rl, alpha, tib, W_wm) {
  ### Cooperative RLWM model where the RPE is downweighted based on WM usage ###

  rpe_reg <- reward - Q_mat_rl[stim, a]
  # Take a chunk out of the RPE based on WM usage  
  rpe <- (1-W_wm)*rpe_reg 
  # cat("\nRPE before", rpe_reg)
  # cat("\nRPE after", rpe)
  
  Q_mat_rl[stim, a] <- Q_mat_rl[stim, a] + alpha*rpe

list("Q_mat_tplus1"=Q_mat_rl, "rpe"=rpe)
}

LearnByRLCooperative <- function(reward, stim, a, Q_mat_rl, alpha, tib, W_wm, eta, n_s) {
  ### Cooperative RLWM model where the RPE is downweighted based on WM usage ###
  
  rpe_reg <- reward - Q_mat_rl[stim, a]
  # Take a chunk out of the RPE based on WM usage  
  scaled_down_weight <- W_wm*eta
  rpe <- (1-scaled_down_weight)*rpe_reg 
  
  # cat("\nSet size", n_s)
  # cat("\nTrial", tib)
  #cat("\n\nWM usage", W_wm)
  # cat("\nEta", eta)
  # cat("\nAlpha", alpha)
  # cat("\nDownweighting (proportion of 1)", scaled_down_weight)
  # cat("\nRPE before", rpe_reg)
  # cat("\nRPE after", rpe)
  # cat("\nQ(s,a) before update\n\n"); print(Q_mat_rl[stim, a])
  
  Q_mat_rl[stim, a] <- Q_mat_rl[stim, a] + alpha*rpe
  
  #cat("\nQ(s,a) after update\n\n"); print(Q_mat_rl[stim, a])
  
list("Q_mat_tplus1"=Q_mat_rl, "rpe"=rpe)
}

UpdateUnchosenOptions <- function(state, 
                                  WM_Q_values, 
                                  s_outcome, 
                                  action, 
                                  eta_unc, 
                                  init_wm_value=1/3,
                                  trial=1e9, # For debugging at a specific point
                                  pun_value=-1, 
                                  rew_value=1,
                                  neutral_value=0
                                  ) {
### Leverage knowledge of the current outcome and what is already known about ###
# the other options to update the unchosen options in working memory when feasible, with extent of
# updating controlled by eta_unc — where eta_unc=1/-1/0 full inference of the unchosen option, 
# and eta_unc=0 is no update 
# Args: state is bandit, actions is chosen action,
# Q_values are Q(s,:), s_outcome is the signed outcome, 
# init_wm_value are WM initializations # 
  
  # If the chosen option was neutral.. 
  if (s_outcome == neutral_value) {
    
    # ... any remaining value lower than init value is punishment..
    candidate_pidx <- which(WM_Q_values < init_wm_value) # Potential punishment idx including action
    pidx <- setdiff(candidate_pidx, action) # Take out the action (bc that's neutral)
    
    #... so add an unchosen update to it 
    if (length(pidx > 0)) {
      
      WM_Q_values[pidx] <- 
        WM_Q_values[pidx] + eta_unc * (pun_value - WM_Q_values[pidx])
    
      # In this case, we know both neutral and punishment idx so we can also update the final item as reward  
      WM_Q_values[-c(action, pidx)] <- 
        WM_Q_values[-c(action, pidx)] + eta_unc * (rew_value - WM_Q_values[-c(action, pidx)])
      
    } else {
      
      # The same logic also applies to reward — we can infer that any item above the init WM val is reward 
      ridx <- which(WM_Q_values > init_wm_value) # Reward index
      assert(length(ridx) < 2)
      
      if (length(ridx) > 0) {
        
        WM_Q_values[ridx] <- 
          WM_Q_values[ridx] + eta_unc * (rew_value - WM_Q_values[ridx])  
        
      # And now we know neutral and reward so can update the final one as punishment  
        WM_Q_values[-c(action, ridx)] <- 
          WM_Q_values[-c(action, ridx)] + eta_unc * (pun_value - WM_Q_values[-c(action, ridx)])
      }
      
    }
   
  # If the chosen option was rewarded..
  } else if (s_outcome == rew_value) {
    
    # Due to decay, items below the initial value could be either punishment or neutral, 
    # however if they're below 0 then they're definitely punishment — so update these 
    pidx <- which(WM_Q_values < neutral_value)
    
    assert("\nPun indexes in case of reward are greater than 1, which is incorrect", length(pidx) < 2)

    if (length(pidx > 0)) {
      WM_Q_values[pidx] <- WM_Q_values[pidx] + eta_unc * (pun_value - WM_Q_values[pidx])
    }
   
  } 
  #Although inference from punishment is not possible,  may decr rfit?  # Finally if chosen option was punished..
  else if (s_outcome == pun_value) {

    # Any other item below the init value is neutral, so update it
    candidate_nidx <- which(WM_Q_values < init_wm_value) # Neutral index
    nidx <- setdiff(candidate_nidx, action) # Take out the action (bc that's punishment)

    if (length(nidx) > 0) {

      WM_Q_values[nidx] <-
        WM_Q_values[nidx] + eta_unc * (neutral_value - WM_Q_values[nidx])

      # Now know both neutral and punishment so we can also update the remaining as reward
      WM_Q_values[-c(action, nidx)] <-
        WM_Q_values[-c(action, nidx)] + eta_unc * (rew_value - WM_Q_values[-c(action, nidx)])

    } else {

      # Or if there's an item greater than the init, it's reward, so update that
      #candidate_ridx <- which(WM_Q_values  > init_wm_value) # Rew index
      ridx <- which(WM_Q_values  > init_wm_value) # Rew index #setdiff(candidate_ridx, action)

      if (length(ridx) > 0) {
      assert(length(ridx) < 2)

        WM_Q_values[ridx] <-
          WM_Q_values[ridx] + eta_unc * (rew_value - WM_Q_values[ridx])

        # Now know both rew and punishment so we can also update neutral
        WM_Q_values[-c(action, ridx)] <-
          WM_Q_values[-c(action, ridx)] + eta_unc * (neutral_value - WM_Q_values[-c(action, ridx)])

      }
    }
  # Break function if didn't go into one of the outcome conditionals
  } else {
    assert("Signed outcome coded wrong in unchosen option", 1==2)
  }

# Send out the updated Q values
WM_Q_values
}

UpdateUnchosenOptionsFixed <- function(state, 
                                  WM_Q_values, 
                                  s_outcome, 
                                  action, 
                                  eta_unc, 
                                  init_wm_value=1/3,
                                  trial=1e9, # For debugging at a specific point
                                  pun_value=-1, 
                                  rew_value=1,
                                  neutral_value=0
) {
  ### Leverage knowledge of the current outcome and what is already known about ###
  # the other options to update the unchosen options in working memory when feasible, with extent of
  # updating controlled by eta_unc — where eta_unc=1/-1/0 full inference of the unchosen option, 
  # and eta_unc=0 is no update 
  # Args: state is bandit, actions is chosen action,
  # Q_values are Q(s,:), s_outcome is the signed outcome, 
  # init_wm_value are WM initializations # 
  
  # If the chosen option was neutral.. 
  if (s_outcome == neutral_value) {
    
    # ... any remaining value lower than init value is punishment..
    candidate_pidx <- which(WM_Q_values < init_wm_value) # Potential punishment idx including action
    pidx <- setdiff(candidate_pidx, action) # Take out the action (bc that's neutral)
    
    #... so add an unchosen update to it 
    if (length(pidx > 0)) {
      
      WM_Q_values[pidx] <- -eta_unc
        #WM_Q_values[pidx] + eta_unc * (pun_value - WM_Q_values[pidx])
      
      # In this case, we know both neutral and punishment idx so we can also update the final item as reward  
      WM_Q_values[-c(action, pidx)] <- eta_unc
        #WM_Q_values[-c(action, pidx)] + eta_unc * (rew_value - WM_Q_values[-c(action, pidx)])
      
    } else {
      
      # The same logic also applies to reward — we can infer that any item above the init WM val is reward 
      #candidate_ridx <- which(WM_Q_values > init_wm_value) # Reward index
      #ridx <- setdiff(candidate_ridx, action) # Take out the action (bc that's neutral)
      
      ridx <- which(WM_Q_values > init_wm_value) # Reward index
      
      #browser(expr=length(ridx > 1))
      assert(length(ridx) < 2)
      
      if (length(ridx) > 0) {
        
        WM_Q_values[ridx] <- eta_unc
          #WM_Q_values[ridx] + eta_unc * (rew_value - WM_Q_values[ridx])  
        
        # And now we know neutral and reward so can update the final one as punishment  
        WM_Q_values[-c(action, ridx)] <- -eta_unc
          #WM_Q_values[-c(action, ridx)] + eta_unc * (pun_value - WM_Q_values[-c(action, ridx)])
      }
      
    }
    
    # If the chosen option was rewarded..
  } else if (s_outcome == rew_value) {
    
    # Due to decay, items below the initial value could be either punishment or neutral, 
    # however if they're below 0 then they're definitely punishment — so update these 
    pidx <- which(WM_Q_values < neutral_value)
    
    assert("\nPun indexes in case of reward are greater than 1, which is incorrect", length(pidx) < 2)
    
    if (length(pidx > 0)) {
      WM_Q_values[pidx] <- -eta_unc #WM_Q_values[pidx] + eta_unc * (pun_value - WM_Q_values[pidx])
    }
    
  } 
  #Although inference from punishment is not possible,  may decr rfit?  # Finally if chosen option was punished..
  # else if (s_outcome == pun_value) {
  # 
  #   # Any other item below the init value is neutral, so update it
  #   candidate_nidx <- which(WM_Q_values < init_wm_value) # Neutral index
  #   nidx <- setdiff(candidate_nidx, action) # Take out the action (bc that's punishment)
  # 
  #   if (length(nidx) > 0) {
  # 
  #     WM_Q_values[nidx] <-
  #       WM_Q_values[nidx] + eta_unc * (neutral_value - WM_Q_values[nidx])
  # 
  #     # Now know both neutral and punishment so we can also update the remaining as reward
  #     WM_Q_values[-c(action, nidx)] <-
  #       WM_Q_values[-c(action, nidx)] + eta_unc * (rew_value - WM_Q_values[-c(action, nidx)])
  # 
  #   } else {
  # 
  #     # Or if there's an item greater than the init, it's reward, so update that
  #     #candidate_ridx <- which(WM_Q_values  > init_wm_value) # Rew index
  #     ridx <- which(WM_Q_values  > init_wm_value) # Rew index #setdiff(candidate_ridx, action)
  # 
  #     if (length(ridx) > 0) {
  #     assert(length(ridx) < 2)
  # 
  #       WM_Q_values[ridx] <-
  #         WM_Q_values[ridx] + eta_unc * (rew_value - WM_Q_values[ridx])
  # 
  #       # Now know both rew and punishment so we can also update neutral
  #       WM_Q_values[-c(action, ridx)] <-
  #         WM_Q_values[-c(action, ridx)] + eta_unc * (neutral_value - WM_Q_values[-c(action, ridx)])
  # 
  #     }
  #   }
  # # Break function if didn't go into one of the outcome conditionals
  # } else {
  #   assert("Signed outcome coded wrong in unchosen option", 1==2)
  # }
  
# Send out the updated Q values
WM_Q_values
}
