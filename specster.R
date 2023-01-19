### specster ###
# This is a wrapper function, which is the highest level someone can call to run
# a specification curve with specster. It is one function, with __ arguments.

specster <- function(data = NULL, 
                     specs = NULL, 
                     cores = 3, 
                     model = NULL,
                     x, 
                     dv, 
                     controls = NULL, 
                     fixed.effects = NULL,
                     cluster.se = NULL, 
                     random.slopes = NULL, 
                     random.intercepts = NULL,
                     require.x = TRUE){
  if (model != "lm" & model != "logistic" & model != "felm" & model != "lme") {
    stop("Specify a model as either 'lm' (linear regression), 'logistic' (logistic regression), 
         'felm' (fixed-effect linear regression), or 'lme' (mixed-effect linear regression)")
  } else if (is.null(specs)){
    setup_specster(model = model,
                   x = x, 
                   dv = dv, 
                   controls = controls, 
                   fixed.effects = fixed.effects,
                   cluster.se = cluster.se, 
                   random.slopes = random.slopes, 
                   random.intercepts = random.intercepts, 
                   require.x = require.x)
    run_specster(model = model,
                 data = data, 
                 specs = specs, 
                 cores = cores)
  } else {
    run_specster(model = model,
                 data = data, 
                 specs = specs, 
                 cores = cores)
  }
}