# specster
Flexible, multicore specification curve analysis for linear regression, logistic regression, fixed-effect regression, and linear mixed-effects regression.

### Note:
This is a work in progress. Please let me know of any errors you find.

## To install:
Run the following in your R console:
devtools::install_github("mattmeist/specster")  

# Scripts
## specster.R
This is just a wrapper function that will first set up your specifications, and then run them. The result is a data.table with your specifications and results.

## setup_specster.R
This function will set up your specifications. You must specify the type of model you would like to run, as this will alter what the specifications look like. For example, if you want to run mixed-effects regressions, you will be required to state at least one random intercept.

## run_specster.R
This function will take your specifications, and run the models on however many cores you'd like to use.
