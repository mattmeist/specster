#' This function creates up to two plots. 
#' @return curve is the specification curve itself, plotting estimates of B1
#' @return choices is the plot of choices we made, with tic marks indicating each 
#' @return can also be both
#' 
#' @param specs is the specification curve results as a data.table
#' @param desc is TRUE or FALSE, indicates direction in which B1 should be ordered 
#' * Defaults to FALSE
#' @param resolution is the proportion of all specifications that should be plotted
#' * Defaults to 1 (100%)
#' @param ci is TRUE or FALSE, indicates whether confidence interval for B1 is plotted
#' * Defaults to TRUE
#' @param h0 is the null hypothesis (which is the dotted line plotted for reference)
#' * Defaults to 0
#' @param choice.colors is the colors that will be used as tic marks for choices
#' @param sig.colors is the colors used to indicate significance in the curve
#' @param return indicates which plot is returned. Either:
#' * 'both' plots curve and choices, and is the default
#' * 'curve' plots just the curve
#' * 'choice' plots just the choices
#' Now the fun!# Plot specifications

plot_specster <- function(specs,
                          desc = FALSE,
                          resolution = 1,
                          ci = TRUE,
                          return = 'both',
                          h0 = 0,
                          choice.colors = c('grey20', 'grey45', 'grey70'),
                          sig.colors = c('red', 'dodgerblue')){
  specs <- as.data.table(specs)
  if (resolution < 0 | resolution > 1){
    stop("Resolution must be between 0 and 1")
  }
  ##################
  ### PLOT CURVE ###
  ##################
  else if (return != 'both' & return != 'curve' & return != 'choices'){
      stop("return must be equal to 'both', 'curve', or 'choices'.")
    } else
      {
    # Order specifications according to coefficient, either descending or not
    if(desc == TRUE){
      specs <- specs %>%
        dplyr::arrange(desc(B1.coef)) %>%
        dplyr::mutate(h.order = 1:n()) %>% # Sort on point estimates for horizontal ordering
        data.table::as.data.table()
    } else {
      specs <- specs %>%
        dplyr::arrange(B1.coef) %>%
        dplyr::mutate(h.order = 1:n()) %>% # Sort on point estimates for horizontal ordering
        data.table::as.data.table()
    }
    
    # Shrink according to resolution
    specs <- specs[sample(nrow(specs)*resolution)]
    
    # Draw the curve.
    curve <- ggplot(data = specs,
                    aes(x = h.order,
                        y = B1.coef,
                        ymin = B1.lb,
                        ymax = B1.ub, 
                        color = B1.p < .05)) +
      geom_point(size = 1) +
      scale_color_manual(values = sig.colors) +
      geom_hline(yintercept = h0, linetype = "dashed", color = "black") +
      labs(x = "", y = 'Estimate') + 
      theme_minimal(base_size = 11) +
      theme( legend.position = "none") + #This removes the legend
      theme(axis.title.x = element_text(face = "bold", size = 14)) + # Edit the typeface on the x axis title
      theme(axis.title.y = element_text(face = "bold", size = 14, vjust = .5)) + # Edit the typeface on the y axis title
      theme(axis.text.x = element_text(size = 10)) + # Edit the typeface on the x axis text 
      theme(axis.text.y = element_text(size = 12)) + # Edit the typeface on the y axis text
      theme(axis.line = element_line(colour = "black"))
      }
  # Add confidence interval if needed
  if (isTRUE(ci)) {
    curve <- curve +
      geom_pointrange(alpha = 0.5,
      size = .6,
      fatten = 1)
    } 
  
  ##################
  ## PLOT CHOICES ##
  ##################
  
  # We only want to plot the choices if they'll be returned. 
  # This is especially important because the choices take a decent amount of time/memory
  if (return != 'curve') {
    
    # Set the type of model -- this will guide how we group choices
    model <- ifelse('fixed.effects' %in% colnames(specs), 'felm',
                    ifelse('random.slopes' %in% colnames(specs), 'lme',
                           ifelse('B1.z' %in% colnames(specs), 'log', 'lm')))
    
    #fixed-effect linear
    if(model == 'felm'){
      # Set the dv, x, controls, fixed, se
      controls.vec <- stringr::str_extract_all(specs$controls, '\\w+') %>%
        unlist() %>%
        unique() %>%
        paste('.c', sep = '')
      fixed.effects.vec <- stringr::str_extract_all(specs$fixed.effects, '\\w+') %>%
        unlist() %>%
        unique() %>%
        paste('.fe', sep = '')
      cluster.se.vec <- stringr::str_extract_all(specs$cluster.se, '\\w+') %>%
        unlist() %>%
        unique() %>%
        paste('.se', sep = '')
      # Choices will be a transformation of specs
      choices <- specs %>%
        # Made long on everything that is not a result OR input
        pivot_longer(c(all_of(controls.vec), 
                              all_of(fixed.effects.vec), 
                                     all_of(cluster.se.vec)), 'Variable') %>% # Call variables variable
        mutate(Variable = as.factor(Variable), #Set Variable to factor
               value = ifelse(value != 0, "|", ""), #Make the value | if in the model -- this is what we plot
               Purpose = factor(ifelse(Variable %in% controls.vec, 'Controls',
                                       ifelse(Variable %in% fixed.effects.vec, 'Fixed Effects',
                                              'Clustering')),
                                levels = c('Fixed Effects', 'Controls', 'Clustering'))) %>% 
        mutate(Variable = as.factor(stringr::str_remove(Variable, '\\.\\w*$'))) %>%
        as.data.table() %>%
        ggplot(aes(h.order, Variable, color = Purpose)) +
        facet_grid(Purpose~., scales = 'free', space = 'free') +
        theme(strip.text.y = element_text(size = 14, angle = 0)) + 
        geom_text(aes(label = value)) +
        scale_color_manual(values = choice.colors) +
        labs(x = "\nSpecification number", y = "Included Variables\n") + 
        theme( legend.position = "none") + #This removes the legend
        theme(axis.title.x = element_text(face = "bold", size = 14)) + # Edit the typeface on the x axis title
        theme(axis.title.y = element_text(face = "bold", size = 14, vjust = .5)) + # Edit the typeface on the y axis title
        theme(axis.text.x = element_text(size = 10)) + # Edit the typeface on the x axis text 
        theme(axis.text.y = element_text(size = 12)) + # Edit the typeface on the y axis text
        theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
    else #linear mixed
      if(model == 'lme'){
        # Set the controls, random.slopes, random.intercepts
        controls.vec <- stringr::str_extract_all(specs$controls, '\\w+') %>%
          unlist() %>%
          unique() %>%
          paste('.c', sep = '')
        random.slopes.vec <- stringr::str_extract_all(specs$random.slopes, '\\w+') %>%
          unlist() %>%
          unique() %>%
          paste('.rs', sep = '')
        random.intercepts.vec <- stringr::str_extract_all(specs$random.intercepts, '\\w+') %>%
          unlist() %>%
          unique() %>%
          paste('.ri', sep = '')
        # Choices will be a transformation of specs
        choices <- specs %>%
          # Made long on everything that is not a result OR input
          pivot_longer(c(all_of(controls.vec), 
                         all_of(random.slopes.vec), 
                         all_of(random.intercepts.vec)), 'Variable') %>% # Call variables variable
          mutate(Variable = as.factor(Variable), #Set Variable to factor
                 value = ifelse(value != 0, "|", ""), #Make the value | if in the model -- this is what we plot
                 Purpose = factor(ifelse(Variable %in% controls.vec, 'Controls',
                                         ifelse(Variable %in% random.slopes.vec, 'Random Slopes',
                                                'Random Intercepts')),
                                  levels = c('Random Slopes', 'Random Intercepts', 'Controls'))) %>% 
          mutate(Variable = as.factor(stringr::str_remove(Variable, '\\.\\w*$'))) %>%
          as.data.table() %>%
          ggplot(aes(h.order, Variable, color = Purpose)) +
          facet_grid(Purpose~., scales = 'free', space = 'free') +
          theme(strip.text.y = element_text(size = 14, angle = 0)) + 
          geom_text(aes(label = value)) +
          scale_color_manual(values = choice.colors) +
          labs(x = "\nSpecification number", y = "Included Variables\n") + 
          theme( legend.position = "none") + #This removes the legend
          theme(axis.title.x = element_text(face = "bold", size = 14)) + # Edit the typeface on the x axis title
          theme(axis.title.y = element_text(face = "bold", size = 14, vjust = .5)) + # Edit the typeface on the y axis title
          theme(axis.text.x = element_text(size = 10)) + # Edit the typeface on the x axis text 
          theme(axis.text.y = element_text(size = 12)) + # Edit the typeface on the y axis text
          theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))
      } else #linear or logit
        {
          # Set the controls
          controls.vec <- stringr::str_extract_all(specs$controls, '\\w+') %>%
            unlist() %>%
            unique() %>%
            paste('.c', sep = '')
          
          # Choices will be a transformation of specs
          choices <- specs %>%
            # Made long on everything that is not a result OR input
            pivot_longer(c(all_of(controls.vec)), 'Control') %>% # Call variables Control
            mutate(Control = as.factor(Control), #Set Control to factor
                   value = ifelse(value != 0, "|", "")) %>% #Make the value | if in the model -- this is what we plot
            mutate(Control = as.factor(stringr::str_remove(Control, '\\.\\w*$'))) %>%
            as.data.table() %>%
            ggplot(aes(h.order, Control)) +
            theme(strip.text.y = element_text(size = 14, angle = 0)) + 
            geom_text(aes(label = value)) +
            scale_color_manual(values = choice.colors[1]) +
            labs(x = "\nSpecification number", y = "Controls\n") + 
            theme( legend.position = "none") + #This removes the legend
            theme(axis.title.x = element_text(face = "bold", size = 14)) + # Edit the typeface on the x axis title
            theme(axis.title.y = element_text(face = "bold", size = 14, vjust = .5)) + # Edit the typeface on the y axis title
            theme(axis.text.x = element_text(size = 10)) + # Edit the typeface on the x axis text 
            theme(axis.text.y = element_text(size = 12)) + # Edit the typeface on the y axis text
            theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))
        }
    }
  # Return either curve, choices, or both
  if (return == 'both') {
    return(cowplot::plot_grid(curve, choices, ncol = 1, align = "v", axis = 'l,r', labels = c('', '')))
  } else if (return == 'curve') {
    return(curve)
  } else if (return == 'choices') {
    return(choices)
  }
}