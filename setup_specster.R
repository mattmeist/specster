# First, load in a function to expand grids by data frame. Thanks to ytsaig on stackoverflow for this:
# https://stackoverflow.com/questions/11693599/alternative-to-expand-grid-for-data-frames
expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))

#Now, run the function
setup_specster <- function(model,
                            x,
                            dv,
                            controls = NULL,
                            fixed.effects = NULL,
                            random.intercepts = NULL,
                            random.slopes = NULL,
                            cluster.se = NULL,
                            require.x = TRUE,
                            empty.controls = TRUE){
  if (model != "lm" & model != "logistic" & model != "felm" & model != "lme") {
    stop("Specify a model as either 'lm' (linear regression), 'logistic' (logistic regression), 
         'felm' (fixed-effect linear regression), or 'lme' (mixed-effect linear regression)")
  } else {
    # Create X
    if (require.x = TRUE){
      x = as.data.frame(x)
    } else {
      x = as.data.frame(c('0', x))
    }
    # Create controls
    if (!rlang::is_null(controls)) {
      controls.df <- rbind(controls, rep(0, length(controls))) %>%
        list() %>%
        as.data.frame()
      colnames(controls.df) <- controls; rownames(controls.df) <- NULL
      controls.df <- expand.grid(controls.df)
      controls.df$controls <- apply(controls.df[,1:(length(controls.df))], 1 , paste , collapse = " + ") %>%
        stringr::str_remove_all(., '0') %>% # Remove all zeros
        stringr::str_remove_all(., '\\s+') %>% # Remove all spaces
        stringr::str_replace_all(., '\\+{2,100}', '+') %>% # Replace multiple pluses with singles
        stringr::str_replace(., '^\\+', '') %>% # Remove all pluses from beginning
        stringr::str_remove_all(., '\\+$') %>% 
        ifelse(. == '', '1', .)

    } else {
      controls.df <- as.data.frame("1")
    }
    # Create Fixed Effects
    if(model != 'felm'){
      fixed.effects.df <- NULL
    } else if (!rlang::is_null(fixed.effects)) {
      fixed.effects.df <- rbind(fixed.effects, rep(0, length(fixed.effects))) %>%
        list() %>%
        as.data.frame()
      colnames(fixed.effects.df) <- fixed.effects; rownames(fixed.effects.df) <- NULL
      fixed.effects.df <- expand.grid(fixed.effects.df)
      fixed.effects.df$fixed.effects <- apply(fixed.effects.df[,1:(length(fixed.effects.df))], 1 , paste , collapse = " + ") %>%
        stringr::str_remove_all(., '0') %>% # Remove all zeros
        stringr::str_remove_all(., '\\s+') %>% # Remove all spaces
        stringr::str_replace_all(., '\\+{2,100}', '+') %>% # Replace multiple pluses with singles
        stringr::str_replace(., '^\\+', '') %>% # Remove all pluses from beginning
        stringr::str_remove_all(., '\\+$')  %>% 
        ifelse(. == '', '0', .)

    } else {
      fixed.effects.df <- as.data.frame("0")
    }
    # Create Cluster SE
    if(model != 'felm'){
      cluster.se.df <- NULL
    } else if (!rlang::is_null(cluster.se)) {
      cluster.se.df <- rbind(cluster.se, rep(0, length(cluster.se))) %>%
        list() %>%
        as.data.frame()
      colnames(cluster.se.df) <- cluster.se; rownames(cluster.se.df) <- NULL
      cluster.se.df <- expand.grid(cluster.se.df)
      cluster.se.df$cluster.se <- apply(cluster.se.df[,1:(length(cluster.se.df))], 1 , paste , collapse = " + ") %>%
        stringr::str_remove_all(., '0') %>% # Remove all zeros
        stringr::str_remove_all(., '\\s+') %>% # Remove all spaces
        stringr::str_replace_all(., '\\+{2,100}', '+') %>% # Replace multiple pluses with singles
        stringr::str_replace(., '^\\+', '') %>% # Remove all pluses from beginning
        stringr::str_remove_all(., '\\+$')  %>% 
        ifelse(. == '', '0', .)
      cluster.se.df
      
    } else {
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
      colnames(random.intercepts.df) <- random.intercepts; rownames(random.intercepts.df) <- NULL
      random.intercepts.df <- expand.grid(random.intercepts.df)
      random.intercepts.df$random.intercepts <- apply(random.intercepts.df[,1:(length(random.intercepts.df))], 1 , paste , collapse = " + ") %>%
        stringr::str_remove_all(., '0') %>% # Remove all zeros
        stringr::str_remove_all(., '\\s+') %>% # Remove all spaces
        stringr::str_replace_all(., '\\+{2,100}', '+') %>% # Replace multiple pluses with singles
        stringr::str_replace(., '^\\+', '') %>% # Remove all pluses from beginning
        stringr::str_remove_all(., '\\+$')
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
      colnames(random.slopes.df) <- random.slopes; rownames(random.slopes.df) <- NULL
      random.slopes.df <- expand.grid(random.slopes.df)
      random.slopes.df$random.slopes <- apply(random.slopes.df[,1:(length(random.slopes.df))], 1 , paste , collapse = " + ") %>%
        stringr::str_remove_all(., '0') %>% # Remove all zeros
        stringr::str_remove_all(., '\\s+') %>% # Remove all spaces
        stringr::str_replace_all(., '\\+{2,100}', '+') %>% # Replace multiple pluses with singles
        stringr::str_replace(., '^\\+', '') %>% # Remove all pluses from beginning
        stringr::str_remove_all(., '\\+$')   %>% 
        ifelse(. == '', '1', .)
      random.slopes.df
      
    } else {
      random.slopes.df <- as.data.frame('1')
    }
    if (model == 'lm' | model == 'logistic'){
      specs <- expand.grid.df(x = x,
                     dv = as.data.frame(dv),
                     controls = controls.df) %>%
        dplyr::mutate_all(as.character) %>%
        as.data.table()
      }
    else if (model == 'lme'){
      specs <- expand.grid.df(x = x,
                     dv = as.data.frame(dv),
                     controls = controls.df,
                     random.intercepts = random.intercepts.df,
                     random.slopes = random.slopes.df) %>%
        dplyr::mutate_all(as.character) %>%
        as.data.table()
    }
    else if (model == 'felm'){
      specs <- expand.grid(x = x,
                  dv = as.data.frame(dv),
                  controls = controls.df,
                  fixed.effects = fixed.effects.df,
                  cluster.se = cluster.se.df) %>%
        dplyr::mutate_all(as.character) %>%
        as.data.table()
    }
  }
  specs
}
