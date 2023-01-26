#' This function creates a data.table of all specifications based on the following:
#'
#' @param model an atomic value denoting the model to be estimated. 
#' * This can be either:
#'  * lm (linear regression)
#'  * logistic (logistic regression)
#'  * felm (fixed-effect linear regression)
#'  * lme (mixed-effect linear regression)
#' @param x an atomic value denoting independent variable
#' * This cannot be empty. In a future version, I will allow it to be.
#' @param controls a vector of control variables. 
#' * Defaults to NULL.
#' @param fixed.effects a vector of fixed effects
#' * Defaults to NULL
#' * Can only be used with linear fixed-effect regression (lfe::felm)
#' @param cluster.se a vector of the different levels at which we can cluster fixed effects
#' * Defaults to NULL
#' * Can only be used with linear fixed-effect regression (lfe::felm)
#' @param random.intercepts a vector of random intercepts
#' * Defaults to NULL
#' * Can only be used with linear mixed-effect regression (lmerTest::lmer)
#' @param random.slopes a vector of random slopes
#' * Defaults to NULL
#' * Can only be used with linear mixed-effect regression (lmerTest::lmer)
#' @return a [data.table] of all combinations of these values
#' 
#' 
#' @note This will make the largest possible data.table, as it does not remove any
#' choices. You have to do that manually (e.g., specs[fixed.effects != ...])
#'
#' @seealso [run_specster()]. Put this output into that function
#'
#'
#' Now the fun!

setup_specster <- function(model,
                            x,
                            dv,
                            controls = NULL,
                            fixed.effects = NULL,
                            cluster.se = NULL,
                            random.intercepts = NULL,
                            random.slopes = NULL){
  if (model != "lm" & model != "logistic" & model != "felm" & model != "lme") {
    stop("Specify a model as either 'lm' (linear regression), 'logistic' (logistic regression), 
         'felm' (fixed-effect linear regression), or 'lme' (mixed-effect linear regression)")
  } else {
    # First, write a function to expand grids by data frame. Thanks to ytsaig on stackoverflow for this:
    # https://stackoverflow.com/questions/11693599/alternative-to-expand-grid-for-data-frames
    expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))
    
    # Create data frame for x
    x = as.data.frame(x)
    
    # Create controls
    if (!rlang::is_null(controls)) { #...if controls are not empty
      
      # Controls are like switches -- they can be on or off. Here, I set those levels.
      controls.df <- rbind(controls, rep(0, length(controls))) %>%
        list() %>%
        as.data.frame()
      #Now, I don't want to get messed up if we use the same variable for two purposes 
      #(e.g., control and fixed-effect). Therefore, we'll give everything a .c here
      # I do something similar for all the other inputs (e.g., .fe, .se...)
      colnames(controls.df) <- paste(controls,'.c', sep = ''); rownames(controls.df) <- NULL
      
      # Now, expand.grid will give us every combination of every variable, either
      # being on (variable name), or off (0)
      controls.df <- expand.grid(controls.df)
      
      # Now, we want to create a vector that we will pass on to the actual function in run_specster
      # We want to do this well, without extra spaces, zeros, things like that
      controls.df$controls <- apply(controls.df[,1:(length(controls.df))], 1 , paste , collapse = " + ") %>%
        stringr::str_remove_all(., '0') %>% # Remove all zeros
        stringr::str_remove_all(., '\\s+') %>% # Remove all spaces
        stringr::str_replace_all(., '\\+{2,1000}', '+') %>% # Replace multiple pluses with singles
        stringr::str_replace(., '^\\+', '') %>% # Remove all pluses from beginning
        stringr::str_remove_all(., '\\+$') %>% 
        #We don't want the entire value to be empty. If it is, we'll replace it with
        # '1', which will estimate the intercept
        ifelse(. == '', '1', .) 

    } else {
      # If there are no controls listed, they'll always be 1 (the mean)
      controls.df <- as.data.frame("1")
    }
    # Create Fixed Effects
    if(model != 'felm'){
      fixed.effects.df <- NULL
    } else if (!rlang::is_null(fixed.effects)) {
      
      # This bit follows the same procedure as for controls
      fixed.effects.df <- rbind(fixed.effects, rep(0, length(fixed.effects))) %>%
        list() %>%
        as.data.frame()
      colnames(fixed.effects.df) <- paste(fixed.effects,'.fe', sep = ''); rownames(fixed.effects.df) <- NULL
      fixed.effects.df <- expand.grid(fixed.effects.df)
      fixed.effects.df$fixed.effects <- apply(fixed.effects.df[,1:(length(fixed.effects.df))], 1 , paste , collapse = " + ") %>%
        stringr::str_remove_all(., '0') %>% # Remove all zeros
        stringr::str_remove_all(., '\\s+') %>% # Remove all spaces
        stringr::str_replace_all(., '\\+{2,100}', '+') %>% # Replace multiple pluses with singles
        stringr::str_replace(., '^\\+', '') %>% # Remove all pluses from beginning
        stringr::str_remove_all(., '\\+$')  %>% 
        ifelse(. == '', '0', .)

    } else {
      #But now, if we have no FEs, we want to set it to 0. This is because the
      # lfe::felm function takes no FEs as 0, not 1.
      fixed.effects.df <- as.data.frame("0")
    }
    # Create Cluster SE
    if(model != 'felm'){
      cluster.se.df <- NULL
    } else if (!rlang::is_null(cluster.se)) {
      cluster.se.df <- rbind(cluster.se, rep(0, length(cluster.se))) %>%
        list() %>%
        as.data.frame()
      colnames(cluster.se.df) <- paste(cluster.se, '.se', sep = ''); rownames(cluster.se.df) <- NULL
      cluster.se.df <- expand.grid(cluster.se.df)
      cluster.se.df$cluster.se <- apply(cluster.se.df[,1:(length(cluster.se.df))], 1 , paste , collapse = " + ") %>%
        stringr::str_remove_all(., '0') %>% # Remove all zeros
        stringr::str_remove_all(., '\\s+') %>% # Remove all spaces
        stringr::str_replace_all(., '\\+{2,100}', '+') %>% # Replace multiple pluses with singles
        stringr::str_replace(., '^\\+', '') %>% # Remove all pluses from beginning
        stringr::str_remove_all(., '\\+$')  %>% 
        ifelse(. == '', '0', .)

    } else {
      #Same as with fes
      cluster.se.df <- as.data.frame("0")
    }
    
    # Create Random Intercepts
    if(model != 'lme'){
      random.intercepts.df <- NULL
      }
    else if (!rlang::is_null(cluster.se)) {
      random.intercepts.df <- rbind(random.intercepts, rep(0, length(random.intercepts))) %>%
        list() %>%
        as.data.frame()
      colnames(random.intercepts.df) <- paste(random.intercepts,'.ri',sep = ''); rownames(random.intercepts.df) <- NULL
      random.intercepts.df <- expand.grid(random.intercepts.df)
      random.intercepts.df$random.intercepts <- apply(random.intercepts.df[,1:(length(random.intercepts.df))], 1 , paste , collapse = " + ") %>%
        stringr::str_remove_all(., '0') %>% # Remove all zeros
        stringr::str_remove_all(., '\\s+') %>% # Remove all spaces
        stringr::str_replace_all(., '\\+{2,100}', '+') %>% # Replace multiple pluses with singles
        stringr::str_replace(., '^\\+', '') %>% # Remove all pluses from beginning
        stringr::str_remove_all(., '\\+$')
      # With lme, we have to give at least 1 random intercept.
      # So we subset the data to always have that.
      random.intercepts.df <- subset(random.intercepts.df, random.intercepts != '')
      
    } else {
      stop("For 'lme' (mixed-effect linear regression), you must specify at least one
           random intercept.")
    }
    
    # Create Random Slopes
    if(model != 'lme'){
      random.slopes.df <- NULL
    } else if (!rlang::is_null(random.slopes)) {
      random.slopes.df <- rbind(random.slopes, rep(1, length(random.slopes))) %>%
        list() %>%
        as.data.frame()
      colnames(random.slopes.df) <- paste(random.slopes, '.rs',sep=''); rownames(random.slopes.df) <- NULL
      random.slopes.df <- expand.grid(random.slopes.df)
      random.slopes.df$random.slopes <- apply(random.slopes.df[,1:(length(random.slopes.df))], 1 , paste , collapse = " + ") %>%
        stringr::str_remove_all(., '0') %>% # Remove all zeros
        stringr::str_remove_all(., '\\s+') %>% # Remove all spaces
        stringr::str_replace_all(., '\\+{2,100}', '+') %>% # Replace multiple pluses with singles
        stringr::str_replace(., '^\\+', '') %>% # Remove all pluses from beginning
        stringr::str_remove_all(., '\\+$')   %>% 
        # lmer can take no random slopes however. We'll set it to 1.
        ifelse(. == '', '1', .)

    } else {
      random.slopes.df <- as.data.frame('1')
    }
    
    # Combining all inputs
    # This is where I'll use the expand.grid.df function
    if (model == 'lm' | model == 'logistic'){
      specs <- expand.grid.df(x = x,
                     dv = as.data.frame(dv),
                     controls = controls.df) %>%
        dplyr::mutate_all(as.character) %>%
        as.data.table()
      #Add pluses to beginning if there is no x
      # This is not relevant for this function as is, but is future proofing.
      specs$controls <- ifelse(specs$x != '', paste0('+', specs$controls, sep = ''), .) 
      }
    else if (model == 'lme'){
      specs <- expand.grid.df(x = x,
                     dv = as.data.frame(dv),
                     controls = controls.df,
                     random.intercepts = random.intercepts.df,
                     random.slopes = random.slopes.df) %>%
        dplyr::mutate_all(as.character) %>%
        as.data.table()
      specs$controls <- ifelse(specs$x != '', paste0('+', specs$controls, sep = ''), .) #Add pluses to beginning if there is no x
      
    }
    else if (model == 'felm'){
      specs <- expand.grid.df(x = x,
                  dv = as.data.frame(dv),
                  controls = controls.df,
                  fixed.effects = fixed.effects.df,
                  cluster.se = cluster.se.df) %>%
        dplyr::mutate_all(as.character) %>%
        as.data.table()
      specs$controls <- ifelse(specs$x != '', paste0('+', specs$controls, sep = ''), .) #Add pluses to beginning if there is no x
      
    }
  }
  specs
}