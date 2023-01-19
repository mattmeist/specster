pkgs <- c('foreach', 'doParallel', 'dplyr', 'dtplyr', 'lfe', 
          'data.table', 'lmerTest')

# Check for packages that are not installed
new.pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
# Install the ones we need
if(length(new.pkgs)) install.packages(new.pkgs)

#Load them all in
lapply(pkgs, library, character.only = TRUE)

rm(pkgs, new.pkgs)

# Note that the standard function uses 3 cores. You can ramp this up if needed.
run_specster <- function(data, specs, model, cores = 3){
  if (model != "lm" & model != "logistic" & model != "felm" & model != "lme") {
    stop("Specify a model as either 'lm' (linear regression), 'logistic' (logistic regression), 
         'felm' (fixed-effect linear regression), or 'lme' (mixed-effect linear regression)")
  } else if (model == 'lm'){
    #Create empty columns to hold results
    specs$B1.coef <- rep(NA, times = nrow(specs))
    specs$B1.lb <- rep(NA, times = nrow(specs))
    specs$B1.ub <- rep(NA, times = nrow(specs))
    specs$B1.t <- rep(NA, times = nrow(specs))
    specs$B1.p <- rep(NA, times = nrow(specs))
    specs$R.Squared <- rep(NA, times = nrow(specs))
    specs$Model <- 1:nrow(specs)
    # Run analysis
    #get the core processors ready to go
    registerDoParallel(cores=cores) 
    # Loop
    res <- foreach(i = 1:nrow(specs), .combine='rbind') %dopar% {
      mod <- summary(stats::lm(as.formula( #as.formula() allows us to paste things into a formula
        paste(specs$dv[i], "~",
              specs$x[i], " +", 
              specs$controls[i])), # Then how we cluster SEs 
        data=data))
      res <- c(mod$coefficients[1,1], #estimate
               mod$coefficients[1,1] - 1.96 * mod$coefficients[1,2], #Lower-bound
               mod$coefficients[1,1] + 1.96 * mod$coefficients[1,2], #upper-bound
               mod$coefficients[1,3], #t val
               mod$coefficients[1,4], # p val
               mod$r.squared) # R2
    }
    # Now set the results
    specs$B1.coef <- res[,1]
    specs$B1.lb <- res[,2]
    specs$B1.ub <- res[,3]
    specs$B1.t <- res[,4]
    specs$B1.p <- res[,5]
    specs$R.Squared <- res[,6]
    specs
  } else if (model == 'logistic'){
    #Create empty columns to hold results
    specs$B1.coef <- rep(NA, times = nrow(specs))
    specs$B1.lb <- rep(NA, times = nrow(specs))
    specs$B1.ub <- rep(NA, times = nrow(specs))
    specs$B1.z <- rep(NA, times = nrow(specs))
    specs$B1.p <- rep(NA, times = nrow(specs))
    specs$Model <- 1:nrow(specs)
    # Run analysis
    #get the core processors ready to go
    registerDoParallel(cores=cores) 
    # Loop
    res <- foreach(i = 1:nrow(specs), .combine='rbind') %dopar% {
      mod <- summary(stats::lm(as.formula( #as.formula() allows us to paste things into a formula
        paste(specs$dv[i], "~",
              specs$x[i], " +", 
              specs$controls[i],", family = 'binomial")),
        data=data))
      res <- c(mod$coefficients[1,1], #estimate
               mod$coefficients[1,1] - 1.96 * mod$coefficients[1,2], #Lower-bound
               mod$coefficients[1,1] + 1.96 * mod$coefficients[1,2], #upper-bound
               mod$coefficients[1,3], #z val
               mod$coefficients[1,4] # p val
      )
    }
    # Now set the results
    specs$B1.coef <- res[,1]
    specs$B1.lb <- res[,2]
    specs$B1.ub <- res[,3]
    specs$B1.z <- res[,4]
    specs$B1.p <- res[,5]
    specs
  } else if (model == 'felm'){
    
    #Create empty columns to hold results
    specs$B1.coef <- rep(NA, times = nrow(specs))
    specs$B1.lb <- rep(NA, times = nrow(specs))
    specs$B1.ub <- rep(NA, times = nrow(specs))
    specs$B1.t <- rep(NA, times = nrow(specs))
    specs$B1.p <- rep(NA, times = nrow(specs))
    specs$R.Squared <- rep(NA, times = nrow(specs))
    specs$Model <- 1:nrow(specs)
    # Run analysis
    #get the core processors ready to go
    registerDoParallel(cores=cores) 
    # Loop
    res <- foreach(i = 1:nrow(specs), .combine='rbind') %dopar% {
      mod <- summary(lfe::felm(as.formula( #as.formula() allows us to paste things into a formula
        paste(specs$dv[i], "~",
              specs$x[i], 
              specs$controls[i]," |", #Whatever controls are in row i, then the | to move to fixed effects
              specs$fixed.effects[i], "|",# Then the fixed effects
              "0|", # Then the instrumental variables 
              specs$cluster.se[i])), # Then how we cluster SEs 
        data=data))
      res <- ifelse(specs$fixed.effects[i] != '0',
                    c(mod$coefficients[1,1], #estimate
                      mod$coefficients[1,1] - 1.96 * mod$coefficients[1,2], #Lower-bound
                      mod$coefficients[1,1] + 1.96 * mod$coefficients[1,2], #upper-bound
                      mod$coefficients[1,3], #t val
                      mod$coefficients[1,4], # p val
                      mod$r.squared),
                    c(mod$coefficients[2,1], #estimate
                      mod$coefficients[2,1] - 1.96 * mod$coefficients[2,2], #Lower-bound
                      mod$coefficients[2,1] + 1.96 * mod$coefficients[2,2], #upper-bound
                      mod$coefficients[2,3], #t val
                      mod$coefficients[2,4], # p val
                      mod$r.squared))
    }
    # Now set the results
    specs$B1.coef <- res[,1]
    specs$B1.lb <- res[,2]
    specs$B1.ub <- res[,3]
    specs$B1.t <- res[,4]
    specs$B1.p <- res[,5]
    specs$R.Squared <- res[,6]
    specs
  }
  else if (model == 'lme'){
    
    #Create empty columns to hold results
    specs$B1.coef <- rep(NA, times = nrow(specs))
    specs$B1.lb <- rep(NA, times = nrow(specs))
    specs$B1.ub <- rep(NA, times = nrow(specs))
    specs$B1.t <- rep(NA, times = nrow(specs))
    specs$B1.p <- rep(NA, times = nrow(specs))
    specs$R.Squared <- rep(NA, times = nrow(specs))
    specs$Model <- 1:nrow(specs)
    # Run analysis
    #get the core processors ready to go
    registerDoParallel(cores=cores) 
    # Loop
    res <- foreach(i = 1:nrow(specs), .combine='rbind') %dopar% {
      mod <- summary(lmerTest::lmer(as.formula( #as.formula() allows us to paste things into a formula
        paste(specs$dv[i], "~",
              specs$x[i], 
              specs$controls[i],"+ (", #Whatever controls are in row i, then the | to move to fixed effects
              specs$random.slopes[i], "|",# Then the random slopes
              specs$random.intercepts[i])), # Then the random intercepts
        data=data))
      res <- c(mod$coefficients[2,1], #estimate
               mod$coefficients[2,1] - 1.96 * mod$coefficients[2,2], #Lower-bound
               mod$coefficients[2,1] + 1.96 * mod$coefficients[2,2], #upper-bound
               mod$coefficients[2,4], #t val
               mod$coefficients[2,5]) # p val
    }
    # Now set the results
    specs$B1.coef <- res[,1]
    specs$B1.lb <- res[,2]
    specs$B1.ub <- res[,3]
    specs$B1.z <- res[,4]
    specs$B1.p <- res[,5]
    specs
  }
}

