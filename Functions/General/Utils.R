DefPlotPars <- function() {
  ### Set up some plot aspects we'll reuse across functions ####
  
  # Notes: Invoked only for side effect of returning general plot pars
  # to the global environment ###
  ga <<- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  # legend pars #
  lp <<- theme(legend.text = element_text(size = 20),
               legend.title = element_blank(),
               legend.key.size = unit(2.5, 'lines'))
  
  # turn off legend
  tol <<- theme(legend.position='none')
  
  # axis pars #
  ap <<- theme(axis.text = element_text(size=22),
               axis.title = element_text(size=25))
  # title pars #
  tp <<- theme(plot.title = element_text(size = 22, hjust = .5))
  
  stp <<- theme(plot.subtitle = element_text(size=18))
  
  # facet pars # 
  ft <<- theme(strip.text.x = element_text(size = 30))
  
  # color pars #
  cf_vals <<- c('orange', 'purple')
}

GenRandString <- function() round(runif(1, 1000, 9999))


ComparePars <- function(model1_par, model2_par, model_char="", xchar="", ychar="", transparency=1, use_identity_line=1) {
  ### Plots parameters from parameters comparable in some way, with # 
  # an identity line for the model 1 par to facilitate comparison (so 
  # if there's a baseline model in some way should be entered as model 1) ###
  df <- data.frame(model1_par, model2_par)
  
  ctest <- cor.test(model1_par, model2_par)
  r <- round(ctest$estimate, 2)
  p <- round(ctest$p.value, 2)
  if (p == 0) p = "< .01"
  str <- paste("r =", r, "p =", p)
  
  p1 <- ggplot(df, aes(x=model1_par, model2_par)) + 
    geom_point(size=3, alpha=transparency) + 
    ga + ap + tp + 
    stp +
    ggtitle(model_char, subtitle=str) +
    ylab(ychar) + xlab(xchar) 
  
  if (use_identity_line) p1 <- p1 + geom_line(aes(y=model1_par)) 
  
p1
}

CorrPlot <- function(model1_par, model2_par, type="M", model_char="", xchar="", ychar="", transparency=1, show_se=1) {
  ### Correlation plot ###
  df <- data.frame(model1_par, model2_par)
  
  ctest <- cor.test(model1_par, model2_par)
  r <- round(ctest$estimate, 2)
  p <- round(ctest$p.value, 2)
  if (p == 0) p = "< .01"
  str <- paste("r =", r, "p =", p)
  
  colors <- c("purple", "red", "blue")
  
  if (type == "M") col <- colors[1]
  if (type == "A") col <- colors[2]
  if (type == "D") col <- colors[3]
  
  p1 <- ggplot(df, aes(x=model1_par, model2_par)) + 
    geom_smooth(method='lm', formula= y~x, color=col, size=3) +
    geom_point(size=6, alpha=transparency, fill=col, pch=21) + 
    ga + ap + tp + 
    stp +
    ggtitle(model_char, subtitle=str) +
    ylab(ychar) + xlab(xchar) + 
    theme(plot.subtitle = element_text(size = 25, face = "bold"))
  
  
p1
}

# Bayesian helper fxs  

PropLess0 <- function(trace) length(which(trace < 0))/length(trace)
PropGreat0 <- function(trace) length(which(trace > 0))/length(trace)

CheckRhat <- function(path, criterion=1.1) all(read.csv(path)$rhat.this_fit < criterion)

ChainsToDf <- function(brms_model) {
  ### Hackey function to convert brms model to df ### 
  model_chains <- NULL; model_chains <- as_draws(brms_model)
  all_chains_df <- NULL
  all_chains_df <- foreach (i = 1:length(model_chains)) %do% {
    data.table(as_draws(brms_model)[[i]] %>% bind_rows())
  } %>% bind_rows()
  
all_chains_df
}

ExtractEstimates <- function(traces) {
  
  est_df <- foreach (r = 1:ncol(traces)) %do% {
    # Take a trace..
    this_trace <- traces[r]
    
    # Edited 4/25/25 to use bayesTestR instead of extracting via quantiles
    # .. apply quantiles on it .. 
    #this_trace_quantile <- quantile(unlist(this_trace), seq(.1, .9, .2))
    
    # Extract HDI  
    this_trace_ci_out <- bayestestR::hdi(this_trace, ci=.9)
    
    # Package up to send out 
    dt_out <- data.table("coef"=names(this_trace),
                         "lb_10"=this_trace_ci_out$CI_low, 
                         "ub_90"=this_trace_ci_out$CI_high,
                         "m"= mean(unlist((this_trace))))
    
    dt_out
  } %>% bind_rows()
  
est_df
}
 
  