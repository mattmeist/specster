#' This function creates up to two plots. 
#' @returns the data.table you provide as 'specs', with results
#' 
#' @param specs is the specifications as a data.table. 
#' * This is the result of setup_specster.R
#' @param data is the raw data. This has to be passed as a list.
#' @param model an atomic value denoting the model to be estimated. 
#' * This can be either:
#'  * lm (linear regression)
#'  * logistic (logistic regression)
#'  * felm (fixed-effect linear regression)
#'  * lme (mixed-effect linear regression)
#' @param cores is the number of parallel cores that will be used to run the regressions
#' * Defaults to 10, or the number of cores you have -1, whichever is smaller
#' 
#' # Indicate the packages required
pkgs <- c('foreach', 'doParallel', 'dplyr', 'dtplyr', 'lfe', 
          'data.table', 'lmerTest')

# Check for packages that are not installed
new.pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
# Install the ones we need
if(length(new.pkgs)) install.packages(new.pkgs)

#Load them all in
lapply(pkgs, library, character.only = TRUE)

rm(pkgs, new.pkgs)

# Note that the function defaults to 3 cores. You can ramp this up if needed.
run_specster <- function(data.list, 
                         specs, 
                         model, 
                         cores = min(parallel::detectCores() - 1, 10)){
  if(!is.list(data.list)){
    stop('data.list must be passed as a list (e.g., "list(d)"), and this list must be
         in the same order as you passed data into setup_specster.')
  } else if (model != "lm" & model != "logistic" & model != "felm" & model != "lme") {
    stop("Specify a model as either 'lm' (linear regression), 'logistic' (logistic regression), 
         'felm' (fixed-effect linear regression), or 'lme' (mixed-effect linear regression)")
  } else if (model == 'lm'){
    # If the effect is an interaction, we have to look for ":", not "*"
    specs$effect.inmod <- ifelse(stringr::str_detect(specs$effect, "\\*"),
                                 stringr::str_replace_all(specs$effect, "\\*", "\\:"),
                                 specs$effect)
    #Create empty columns to hold results
    specs$effect.coef <- rep(NA, times = nrow(specs))
    specs$effect.lb <- rep(NA, times = nrow(specs))
    specs$effect.ub <- rep(NA, times = nrow(specs))
    specs$effect.t <- rep(NA, times = nrow(specs))
    specs$effect.p <- rep(NA, times = nrow(specs))
    specs$R.Squared <- rep(NA, times = nrow(specs))
    specs$Model <- 1:nrow(specs)
    specs$data.used <- rep(NA, times = nrow(specs))
    
    # Run analysis
    #get the core processors ready to go
    registerDoParallel(cores=cores) 
    # Loop
    for(j in 1:length(data.list)){
      start <- nrow(specs)*(j-1)/length(data.list)+1
      end <- nrow(specs)*(j)/length(data.list)
      res <- foreach(i = start:end, .combine='rbind') %dopar% {
      mod <- summary(stats::lm(as.formula( #as.formula() allows us to paste things into a formula
        paste(specs$dv[i], "~",
              specs$effect[i], " +", 
              specs$controls[i])), # Then how we cluster SEs 
        data=data.list[[j]]))
      res <- c(mod$coefficients[specs$effect.inmod[i],1], #estimate
               mod$coefficients[specs$effect.inmod[i],1] - 1.96 * mod$coefficients[specs$effect.inmod[i],2], #Lower-bound
               mod$coefficients[specs$effect.inmod[i],1] + 1.96 * mod$coefficients[specs$effect.inmod[i],2], #upper-bound
               mod$coefficients[specs$effect.inmod[i],3], #t val
               mod$coefficients[specs$effect.inmod[i],4], # p val
               mod$r.squared, # R2
               deparse(mod$call[[3]]) # Data used in specification
      )
    }
    # Now set the results
    specs$effect.coef[start:end] <- res[,1]
    specs$effect.lb[start:end] <- res[,2]
    specs$effect.ub[start:end] <- res[,3]
    specs$effect.t[start:end] <- res[,4]
    specs$effect.p[start:end] <- res[,5]
    specs$R.Squared[start:end] <- res[,6]
    specs$data.used[start:end] <- res[,7]
    }
    specs <- specs[,-c('data.used', 'effect.inmod')]
    return(specs)
  } else if (model == 'logistic'){
    # If the effect is an interaction, we have to look for ":", not "*"
    specs$effect.inmod <- ifelse(stringr::str_detect(specs$effect, "\\*"),
                                 stringr::str_replace_all(specs$effect, "\\*", "\\:"),
                                 specs$effect)
    #Create empty columns to hold results
    specs$effect.coef <- rep(NA, times = nrow(specs))
    specs$effect.lb <- rep(NA, times = nrow(specs))
    specs$effect.ub <- rep(NA, times = nrow(specs))
    specs$effect.z <- rep(NA, times = nrow(specs))
    specs$effect.p <- rep(NA, times = nrow(specs))
    specs$Model <- 1:nrow(specs)
    specs$data.used <- rep(NA, times = nrow(specs))
    
    # Run analysis
    #get the core processors ready to go
    registerDoParallel(cores=cores) 
    # Loop
    for(j in 1:length(data.list)){
      start <- nrow(specs)*(j-1)/length(data.list)+1
      end <- nrow(specs)*(j)/length(data.list)
      res <- foreach(i = start:end, .combine='rbind') %dopar% {
        mod <- summary(stats::lm(as.formula( #as.formula() allows us to paste things into a formula
          paste(specs$dv[i], "~",
                specs$effect[i], " +", 
                specs$controls[i],", family = 'binomial")),
          data=data.list[[j]]))
        res <- c(mod$coefficients[specs$effect.inmod[i],1], #estimate
                 mod$coefficients[specs$effect.inmod[i],1] - 1.96 * mod$coefficients[specs$effect.inmod[i],2], #Lower-bound
                 mod$coefficients[specs$effect.inmod[i],1] + 1.96 * mod$coefficients[specs$effect.inmod[i],2], #upper-bound
                 mod$coefficients[specs$effect.inmod[i],3], #z val
                 mod$coefficients[specs$effect.inmod[i],4], # p val
                 deparse(mod$call[[3]]) # Data used in specification
        )
      }
      # Now set the results
      specs$effect.coef[start:end] <- res[,1]
      specs$effect.lb[start:end] <- res[,2]
      specs$effect.ub[start:end] <- res[,3]
      specs$effect.z[start:end] <- res[,4]
      specs$effect.p[start:end] <- res[,5]
      specs$data.used[start:end] <- res[,6]
    }
    specs <- specs[,-c('data.used', 'effect.inmod')]
  return(specs)
  } else if (model == 'felm'){
    # If the effect is an interaction, we have to look for ":", not "*"
    specs$effect.inmod <- ifelse(stringr::str_detect(specs$effect, "\\*"),
                                 stringr::str_replace_all(specs$effect, "\\*", "\\:"),
                                 specs$effect)
    #Create empty columns to hold results
    specs$effect.coef <- rep(NA, times = nrow(specs))
    specs$effect.lb <- rep(NA, times = nrow(specs))
    specs$effect.ub <- rep(NA, times = nrow(specs))
    specs$effect.t <- rep(NA, times = nrow(specs))
    specs$effect.p <- rep(NA, times = nrow(specs))
    specs$R.Squared <- rep(NA, times = nrow(specs))
    specs$Model <- 1:nrow(specs)
    specs$data.used <- rep(NA, times = nrow(specs))
    
    # Run analysis
    #get the core processors ready to go
    registerDoParallel(cores=cores) 
    # Loop
    for(j in 1:length(data.list)){
      start <- nrow(specs)*(j-1)/length(data.list)+1
      end <- nrow(specs)*(j)/length(data.list)
      res <- foreach(i = start:end, .combine='rbind') %dopar% {
        mod <- summary(lfe::felm(as.formula( #as.formula() allows us to paste things into a formula
          paste(specs$dv[i], "~",
                specs$effect[i], 
                specs$controls[i]," |", #Whatever controls are in row i, then the | to move to fixed effects
                specs$fixed.effects[i], "|",# Then the fixed effects
                "0|", # Then the instrumental variables 
                specs$cluster.se[i])), # Then how we cluster SEs 
          data=data.list[[j]]))
        res <- c(mod$coefficients[specs$effect.inmod[i],1], #estimate
                 mod$coefficients[specs$effect.inmod[i],1] - 1.96 * mod$coefficients[specs$effect.inmod[i],2], #Lower-bound
                 mod$coefficients[specs$effect.inmod[i],1] + 1.96 * mod$coefficients[specs$effect.inmod[i],2], #upper-bound
                 mod$coefficients[specs$effect.inmod[i],3], #t val
                 mod$coefficients[specs$effect.inmod[i],4], # p val
                 mod$r.squared,
                 deparse(mod$call[[3]])) # Data used in specification)
      }
      # Now set the results
      specs$effect.coef[start:end] <- res[,1]
      specs$effect.lb[start:end] <- res[,2]
      specs$effect.ub[start:end] <- res[,3]
      specs$effect.t[start:end] <- res[,4]
      specs$effect.p[start:end] <- res[,5]
      specs$R.Squared[start:end] <- res[,6]
      specs$data.used[start:end] <- res[,7]
    }
    specs <- specs[,-c('data.used', 'effect.inmod')]
    return(specs)
  }
  else if (model == 'lme'){
    # If the effect is an interaction, we have to look for ":", not "*"
    specs$effect.inmod <- ifelse(stringr::str_detect(specs$effect, "\\*"),
                                 stringr::str_replace_all(specs$effect, "\\*", "\\:"),
                                 specs$effect)
    #Create empty columns to hold results
    specs$effect.coef <- rep(NA, times = nrow(specs))
    specs$effect.lb <- rep(NA, times = nrow(specs))
    specs$effect.ub <- rep(NA, times = nrow(specs))
    specs$effect.t <- rep(NA, times = nrow(specs))
    specs$effect.p <- rep(NA, times = nrow(specs))
    specs$R.Squared <- rep(NA, times = nrow(specs))
    specs$Model <- 1:nrow(specs)
    specs$data.used <- rep(NA, times = nrow(specs))
    # Run analysis
    #get the core processors ready to go
    registerDoParallel(cores=cores) 
    # Loop
    for(j in 1:length(data.list)){
      start <- nrow(specs)*(j-1)/length(data.list)+1
      end <- nrow(specs)*(j)/length(data.list)
      res <- foreach(i = start:end, .combine='rbind') %dopar% {
        mod <- summary(lmerTest::lmer(as.formula( #as.formula() allows us to paste things into a formula
          paste(specs$dv[i], "~",
                specs$effect[i], 
                specs$controls[i],"+ (", #Whatever controls are in row i, then the | to move to fixed effects
                specs$random.slopes[i], "|",# Then the random slopes
                specs$random.intercepts[i])), # Then the random intercepts
          data=data.list[[j]]))
        res <- c(mod$coefficients[specs$effect.inmod[i],1], #estimate
                 mod$coefficients[specs$effect.inmod[i],1] - 1.96 * mod$coefficients[specs$effect.inmod[i],2], #Lower-bound
                 mod$coefficients[specs$effect.inmod[i],1] + 1.96 * mod$coefficients[specs$effect.inmod[i],2], #upper-bound
                 mod$coefficients[specs$effect.inmod[i],4], #t val
                 mod$coefficients[specs$effect.inmod[i],5], # p val
                 deparse(mod$call[[3]])) # Data used in specification
      }
    # Now set the results
    specs$effect.coef[start:end] <- as.numeric(res[,1])
    specs$effect.lb[start:end] <- as.numeric(res[,2])
    specs$effect.ub[start:end] <- as.numeric(res[,3])
    specs$effect.z[start:end] <- as.numeric(res[,4])
    specs$effect.p[start:end] <- as.numeric(res[,5])
    specs$data.used[start:end] <- as.numeric(res[,6])
    }
    specs <- specs[,-c('data.used', 'effect.inmod')]
    return(specs)
  }
  doParallel::stopImplicitCluster()
}

