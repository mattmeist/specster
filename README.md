# specster
Flexible, multicore specification curve analysis for linear regression, logistic regression, fixed-effect regression, and linear mixed-effects regression.

### Note:
This is a work in progress. Please let me know of when you find errors.

## To install:
Run the following in your R console:
devtools::install_github("mattmeist/specster")  

# Scripts

## setup_specster.R
This function will set up your specifications. You must specify the type of model you would like to run, as this will alter what the specifications look like. For example, if you want to run mixed-effects regressions, you will be required to state at least one random intercept.

## run_specster.R
This function will take your specifications, and run the models on however many cores you'd like to use.

### Note:
If you want to run only a subset of the total number of specifications, please do so using the **resolution** argument in setup_specster(), *not* by subsetting the specifications as an argument to this function. If you do the latter, things go wrong, and quietly.

## plot_specster.R
This function will plot your specifications. 

### Note:
You might prefer having more control over your plots. If so, dig into the function here, *or* use this walkthrough: https://dcosme.github.io/2019/02/26/specification-curve-example/.
