#Code from R-studio conference 2022 machine learning workshop. 
#notes can be found at https://workshops.tidymodels.org/

library(tidymodels)

#Loads frog data
data("tree_frogs", package = "stacks")
tree_frogs <- tree_frogs %>%
  mutate(t_o_d = factor(t_o_d),
         age = age / 86400) %>%
  filter(!is.na(latency)) %>%
  select(-c(clutch, hatched))

#Split the data as early as possible.
#This split object that says how to split the data. 
#Run training, testing, or total on it.
set.seed(123)
frog_split <- initial_split(tree_frogs, prop = .8)
frog_split
#> <Training/Testing/Total>
#> <429/143/572>

frog_train <- training(frog_split)
frog_test <- testing(frog_split)
# 
# #You could split by a different amount 
# set.seed(243)
# (frog_split.2 <- initial_split(tree_frogs, prop = .8))
# frog_train.2 <- training(frog_split.2)
# set.seed(123)
# (frog_split.3 <- initial_split(tree_frogs, prop = .8))
# frog_train.3 <- training(frog_split.3)

#exploring the data
hist(frog_train$age)
hist(frog_train$latency, bins = 20)
plot(frog_train$age, frog_train$latency)
frog_train %>% group_by(treatment, t_o_d) %>% summarize(mean_latency = mean(latency))
boxplot(latency ~ reflex, data = frog_train)
boxplot(latency ~ treatment, data = frog_train)

#We could use stratified sampling with the splitting.
set.seed(123)
frog_split <- initial_split(tree_frogs, prop = 0.8, strata = latency)
frog_split
#> <Training/Testing/Total>
#> <456/116/572>

##Setting modle, engine, and mode

linear_reg()
#> Linear Regression Model Specification (regression)
#> 
#> Computational engine: lm

linear_reg() %>%
  set_engine("glmnet")
#> Linear Regression Model Specification (regression)
#> 
#> Computational engine: glmnet


linear_reg() %>%
  set_engine("stan")
#> Linear Regression Model Specification (regression)
#> 
#> Computational engine: stan

#Notice that the default mode for decision tree is unknown.
decision_tree()
#> Decision Tree Model Specification (unknown)
#> 
#> Computational engine: rpart

#Here it is specified as a linear regression.
decision_tree() %>% 
  set_mode("regression")
#> Decision Tree Model Specification (regression)
#> 
#> Computational engine: rpart


##All available models are listed at https://www.tidymodels.org/find/parsnip/
  

#Do yourself: changing this model. 
#You can just change the "set_mode" parameter.
#You could also change the original function entirely.
#tree_spec <- decision_tree() %>% 
tree_spec <- decision_tree() %>%
  #set_mode("regression")
  set_mode("classification")
tree_spec

#Not using work flow (not recommended by them)
tree_spec <-
  decision_tree() %>% 
  set_mode("regression")

tree_spec %>% 
  fit(latency ~ ., data = frog_train) 
#> parsnip model object
#> 
#> n= 456 
#> 
#> node), split, n, deviance, yval
#>       * denotes terminal node
#> 
#>  1) root 456 2197966.00  92.90351  
#>    2) age>=4.947975 256  252347.40  60.89844  
#>      4) treatment=control 131   91424.06  48.42748 *
#>      5) treatment=gentamicin 125  119197.90  73.96800 *
#>    3) age< 4.947975 200 1347741.00 133.87000  
#>      6) treatment=control 140  986790.70 118.25710  
#>       12) reflex=mid,full 129  754363.70 111.56590 *
#>       13) reflex=low 11  158918.20 196.72730 *
#>      7) treatment=gentamicin 60  247194.60 170.30000  
#>       14) age< 4.664439 30  102190.20 147.83330  
#>         28) age>=4.566638 22   53953.86 129.77270 *
#>         29) age< 4.566638 8   21326.00 197.50000 *
#>       15) age>=4.664439 30  114719.40 192.76670 *

#You can easily change the type of model easily at the beginning. 
##Using workflow
tree_spec <-
  decision_tree() %>% 
#lm_spec <-
#linear_reg() %>%
    set_mode("regression")

tree_fit <- workflow() %>%
  add_formula(latency ~ ., ) %>%
  #add_model(lm_spec) %>%
  add_model(tree_spec) %>%
  fit(data = frog_train) 
#> ══ Workflow [trained] ════════════════════════════════════════════════
#> Preprocessor: Formula
#> Model: decision_tree()
#> 
#> ── Preprocessor ──────────────────────────────────────────────────────
#> latency ~ .
#> 
#> ── Model ─────────────────────────────────────────────────────────────
#> n= 456 
#> 
#> node), split, n, deviance, yval
#>       * denotes terminal node
#> 
#>  1) root 456 2197966.00  92.90351  
#>    2) age>=4.947975 256  252347.40  60.89844  
#>      4) treatment=control 131   91424.06  48.42748 *
#>      5) treatment=gentamicin 125  119197.90  73.96800 *
#>    3) age< 4.947975 200 1347741.00 133.87000  
#>      6) treatment=control 140  986790.70 118.25710  
#>       12) reflex=mid,full 129  754363.70 111.56590 *
#>       13) reflex=low 11  158918.20 196.72730 *
#>      7) treatment=gentamicin 60  247194.60 170.30000  
#>       14) age< 4.664439 30  102190.20 147.83330  
#>         28) age>=4.566638 22   53953.86 129.77270 *
#>         29) age< 4.566638 8   21326.00 197.50000 *
#>       15) age>=4.664439 30  114719.40 192.76670 *

#This provides predicted values in a single column
#Always returns a tibble and the rows are always the same.
#Never call these on things you extract from the fit, only the fit itself.
predict.vector <- predict(tree_fit, new_data = frog_test)
#This provides the predictions bound to the data frame
predict.df <- augment(tree_fit, new_data = frog_test)

#Extracting things from a model and understanding it.
#and then plotting it.
library(rpart.plot)
tree_fit %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = FALSE)

#Use functions from hardhat package, to get information about your model.

#Now extract some information from the linear model.

##Using workflow
  lm_spec <-
  linear_reg() %>%
  set_mode("regression")

lm_fit <- workflow() %>%
  add_formula(latency ~ ., ) %>%
  add_model(lm_spec) %>%
  #add_model(tree_spec) %>%
  fit(data = frog_train) 

#You can do anything with the linear model object that you can with lm
library(hardhat)
summary(extract_fit_engine(lm_fit))


#Deploying a trained model
library(vetiver)
v <- vetiver_model(lm_fit, "frog_hatching")
v
#> 
#> ── frog_hatching ─ <butchered_workflow> model for deployment 
#> A rpart regression modeling workflow using 4 features

#Deploy the API
library(plumber)
pr() %>%
  vetiver_api(v)
#> # Plumber router with 2 endpoints, 4 filters, and 1 sub-router.
#> # Use `pr_run()` on this object to start the API.
#> ├──[queryString]
#> ├──[body]
#> ├──[cookieParser]
#> ├──[sharedSecret]
#> ├──/logo
#> │  │ # Plumber static router serving from directory: /Users/max/Library/R/x86_64/4.2/library/vetiver
#> ├──/ping (GET)
#> └──/predict (POST)




library(tidymodels)

data("tree_frogs", package = "stacks")

tree_frogs <- tree_frogs %>%
  mutate(t_o_d = factor(t_o_d),
         age = age / 86400) %>% 
  filter(!is.na(latency)) %>%
  select(-c(clutch, hatched))

set.seed(123)

frog_split <- initial_split(tree_frogs, prop = 0.8, strata = latency)
frog_train <- training(frog_split)
frog_test <- testing(frog_split)

tree_spec <- decision_tree(cost_complexity = 0.001, mode = "regression")
tree_wflow <- workflow(latency ~ ., tree_spec)
tree_fit <- fit(tree_wflow, frog_train)




#Metrics in yardstick package
library(yardstick)
augment(tree_fit, new_data = frog_test) %>%
  metrics(latency, .pred)
#> # A tibble: 3 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 rmse    standard      59.2  
#> 2 rsq     standard       0.380
#> 3 mae     standard      40.2

augment(tree_fit, new_data = frog_test) %>%
  rmse(latency, .pred)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 rmse    standard        59.2

#Model performance
augment(tree_fit, new_data = frog_test) %>%
  group_by(reflex) %>%
  rmse(latency, .pred)
#> # A tibble: 3 × 4
#>   reflex .metric .estimator .estimate
#>   <fct>  <chr>   <chr>          <dbl>
#> 1 low    rmse    standard        94.3
#> 2 mid    rmse    standard       101. 
#> 3 full   rmse    standard        51.2

#Wrap up metrics
frog_metrics <- metric_set(rmse, msd)
augment(tree_fit, new_data = frog_test) %>%
  frog_metrics(latency, .pred)
#> # A tibble: 2 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 rmse    standard      59.2  
#> 2 msd     standard      -0.908


#Danger of overfitting - this just predicts using the training data
tree_fit %>%
  augment(frog_train)
#> # A tibble: 456 × 6
#>    treatment  reflex   age t_o_d     latency .pred
#>    <chr>      <fct>  <dbl> <fct>       <dbl> <dbl>
#>  1 control    full    5.42 morning        33  39.8
#>  2 control    full    5.38 morning        19  66.7
#>  3 control    full    5.38 morning         2  66.7
#>  4 control    full    5.44 morning        39  39.8
#>  5 control    full    5.41 morning        42  39.8
#>  6 control    full    4.75 afternoon      20  59.8
#>  7 control    full    4.95 night          31  83.1
#>  8 control    full    5.42 morning        21  39.8
#>  9 gentamicin full    5.39 morning        30  64.6
#> 10 control    full    4.55 afternoon      43 174. 
#> # … with 446 more rows

tree_fit %>%
  augment(frog_train) %>%
  rmse(latency, .pred)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 rmse    standard        49.4

tree_fit %>%
  augment(frog_test) %>%
  rmse(latency, .pred)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 rmse    standard        59.2


#Trying to use some metrics on entire set
#You can use metrics() to compute standard metrics
#or list them.
tree_fit %>%
  augment(tree_frogs) %>%
  #rsq(latency, .pred)
  metrics(latency, .pred)


#Trying to use some metrics on training set (bad)
tree_fit %>%
  augment(frog_train) %>%
  #rsq(latency, .pred)
  metrics(latency, .pred)


#Trying to use some metrics on testing set 
tree_fit %>%
  augment(frog_test) %>%
  #rsq(latency, .pred)
  metrics(latency, .pred)


##Using training data to decide between different models.
#We do this with resampling

#Cross Validation
vfold_cv(frog_train) # v = 10 is default
#> #  10-fold cross-validation 
#> # A tibble: 10 × 2
#>    splits           id    
#>    <list>           <chr> 
#>  1 <split [410/46]> Fold01
#>  2 <split [410/46]> Fold02
#>  3 <split [410/46]> Fold03
#>  4 <split [410/46]> Fold04
#>  5 <split [410/46]> Fold05
#>  6 <split [410/46]> Fold06
#>  7 <split [411/45]> Fold07
#>  8 <split [411/45]> Fold08
#>  9 <split [411/45]> Fold09
#> 10 <split [411/45]> Fold10

frog_folds <- vfold_cv(frog_train)
frog_folds$splits[1:3]
#> [[1]]
#> <Analysis/Assess/Total>
#> <410/46/456>
#> 
#> [[2]]
#> <Analysis/Assess/Total>
#> <410/46/456>
#> 
#> [[3]]
#> <Analysis/Assess/Total>
#> <410/46/456>

set.seed(123)
frog_folds <- vfold_cv(frog_train, v = 10, strata = latency)
frog_folds
#> #  10-fold cross-validation using stratification 
#> # A tibble: 10 × 2
#>    splits           id    
#>    <list>           <chr> 
#>  1 <split [408/48]> Fold01
#>  2 <split [408/48]> Fold02
#>  3 <split [408/48]> Fold03
#>  4 <split [409/47]> Fold04
#>  5 <split [411/45]> Fold05
#>  6 <split [412/44]> Fold06
#>  7 <split [412/44]> Fold07
#>  8 <split [412/44]> Fold08
#>  9 <split [412/44]> Fold09
#> 10 <split [412/44]> Fold10


tree_res <- fit_resamples(tree_wflow, frog_folds)
tree_res
#> # Resampling results
#> # 10-fold cross-validation using stratification 
#> # A tibble: 10 × 4
#>    splits           id     .metrics         .notes          
#>    <list>           <chr>  <list>           <list>          
#>  1 <split [408/48]> Fold01 <tibble [2 × 4]> <tibble [0 × 3]>
#>  2 <split [408/48]> Fold02 <tibble [2 × 4]> <tibble [0 × 3]>
#>  3 <split [408/48]> Fold03 <tibble [2 × 4]> <tibble [0 × 3]>
#>  4 <split [409/47]> Fold04 <tibble [2 × 4]> <tibble [0 × 3]>
#>  5 <split [411/45]> Fold05 <tibble [2 × 4]> <tibble [0 × 3]>
#>  6 <split [412/44]> Fold06 <tibble [2 × 4]> <tibble [0 × 3]>
#>  7 <split [412/44]> Fold07 <tibble [2 × 4]> <tibble [0 × 3]>
#>  8 <split [412/44]> Fold08 <tibble [2 × 4]> <tibble [0 × 3]>
#>  9 <split [412/44]> Fold09 <tibble [2 × 4]> <tibble [0 × 3]>
#> 10 <split [412/44]> Fold10 <tibble [2 × 4]> <tibble [0 × 3]>


#Tuning with the tune package

tree_res %>%
  collect_metrics()
#> # A tibble: 2 × 6
#>   .metric .estimator   mean     n std_err .config             
#>   <chr>   <chr>       <dbl> <int>   <dbl> <chr>               
#> 1 rmse    standard   59.6      10  2.31   Preprocessor1_Model1
#> 2 rsq     standard    0.305    10  0.0342 Preprocessor1_Model1


tree_res %>%
  collect_metrics() %>% 
  select(.metric, mean, n)
#> # A tibble: 2 × 3
#>   .metric   mean     n
#>   <chr>    <dbl> <int>
#> 1 rmse    59.6      10
#> 2 rsq      0.305    10


# Save the assessment set results
ctrl_frog <- control_resamples(save_pred = TRUE)
tree_res <- fit_resamples(tree_wflow, frog_folds, control = ctrl_frog)

tree_preds <- collect_predictions(tree_res)
tree_preds
#> # A tibble: 456 × 5
#>    id     .pred  .row latency .config             
#>    <chr>  <dbl> <int>   <dbl> <chr>               
#>  1 Fold01  39.6     1      33 Preprocessor1_Model1
#>  2 Fold01  72.1     3       2 Preprocessor1_Model1
#>  3 Fold01  63.8     9      30 Preprocessor1_Model1
#>  4 Fold01  72.1    13      46 Preprocessor1_Model1
#>  5 Fold01  43.3    28      11 Preprocessor1_Model1
#>  6 Fold01  61.7    35      41 Preprocessor1_Model1
#>  7 Fold01  39.6    51      43 Preprocessor1_Model1
#>  8 Fold01 134.     70      20 Preprocessor1_Model1
#>  9 Fold01  70.6    74      21 Preprocessor1_Model1
#> 10 Fold01  39.6   106      14 Preprocessor1_Model1
#> # … with 446 more rows


#We can plot the predictions to actual latencies.
tree_preds %>% 
  ggplot(aes(latency, .pred, color = id)) + 
  geom_abline(lty = 2, col = "gray", size = 1.5) +
  geom_point(alpha = 0.5) +
  coord_obs_pred()

#We don't retain the cross-validated models (they are trashed). 
#We only use them to validate our modeling method.

#Bootstrapping
set.seed(3214)
bootstraps(frog_train)
#> # Bootstrap sampling 
#> # A tibble: 25 × 2
#>    splits            id         
#>    <list>            <chr>      
#>  1 <split [456/163]> Bootstrap01
#>  2 <split [456/166]> Bootstrap02
#>  3 <split [456/173]> Bootstrap03
#>  4 <split [456/177]> Bootstrap04
#>  5 <split [456/166]> Bootstrap05
#>  6 <split [456/163]> Bootstrap06
#>  7 <split [456/164]> Bootstrap07
#>  8 <split [456/165]> Bootstrap08
#>  9 <split [456/170]> Bootstrap09
#> 10 <split [456/177]> Bootstrap10
#> # … with 15 more rows
#> 

#Make your own bootstrap and validation set
set.seed(2412)
bootstraps(frog_train, times = 30)
set.seed(9321)
validation_split(frog_train)

##Random Forest

rf_spec <- rand_forest(trees = 1000, mode = "regression")
rf_spec
#> Random Forest Model Specification (regression)
#> 
#> Main Arguments:
#>   trees = 1000
#> 
#> Computational engine: ranger

rf_wflow <- workflow(latency ~ ., rf_spec)
rf_wflow
#> ══ Workflow ══════════════════════════════════════════════════════════
#> Preprocessor: Formula
#> Model: rand_forest()
#> 
#> ── Preprocessor ──────────────────────────────────────────────────────
#> latency ~ .
#> 
#> ── Model ─────────────────────────────────────────────────────────────
#> Random Forest Model Specification (regression)
#> 
#> Main Arguments:
#>   trees = 1000
#> 
#> Computational engine: ranger

set.seed(2643)
#you can include a metrics parameter below to only include particular metrics
rf_res <- fit_resamples(rf_wflow, frog_folds, 
                        control = control_grid(save_pred = TRUE))
rf_res

rf_res %>%
  collect_metrics()

rf.predictions <- rf_res %>%
                collect_predictions()

=      

plot(rf.predictions$`.pred`, rf.predictions$latency)

#you can pass predictions to ggplot
collect_predictions(rf_res) %>% 
  ggplot(aes(latency, .pred, color = id)) + 
  geom_abline(lty = 2, col = "gray", size = 1.5) +
  geom_point(alpha = 0.5) +
  coord_obs_pred()

#comparing multiple workflows at once.
workflow_set(list(latency ~ .), list(tree_spec, rf_spec))
#> # A workflow set/tibble: 2 × 4
#>   wflow_id              info             option    result    
#>   <chr>                 <list>           <list>    <list>    
#> 1 formula_decision_tree <tibble [1 × 4]> <opts[0]> <list [0]>
#> 2 formula_rand_forest   <tibble [1 × 4]> <opts[0]> <list [0]>

workflow_set(list(latency ~ .), list(tree_spec, rf_spec)) %>%
  workflow_map("fit_resamples", resamples = frog_folds)
#> # A workflow set/tibble: 2 × 4
#>   wflow_id              info             option    result   
#>   <chr>                 <list>           <list>    <list>   
#> 1 formula_decision_tree <tibble [1 × 4]> <opts[1]> <rsmp[+]>
#> 2 formula_rand_forest   <tibble [1 × 4]> <opts[1]> <rsmp[+]>


#rank the different types of workflows.
workflow_set(list(latency ~ .), list(tree_spec, rf_spec)) %>%
  workflow_map("fit_resamples", resamples = frog_folds) %>%
  rank_results()
#> # A tibble: 4 × 9
#>   wflow_id         .config .metric   mean std_err     n preprocessor model  rank
#>   <chr>            <chr>   <chr>    <dbl>   <dbl> <int> <chr>        <chr> <int>
#> 1 formula_rand_fo… Prepro… rmse    55.8    1.71      10 formula      rand…     1
#> 2 formula_rand_fo… Prepro… rsq      0.371  0.0301    10 formula      rand…     1
#> 3 formula_decisio… Prepro… rmse    59.6    2.31      10 formula      deci…     2
#> 4 formula_decisio… Prepro… rsq      0.305  0.0342    10 formula      deci…     2



#Now you can fit to the test data with final fit.
# frog_split has train + test info
final_fit <- last_fit(rf_wflow, frog_split) 

final_fit
#> # Resampling results
#> # Manual resampling 
#> # A tibble: 1 × 6
#>   splits            id               .metrics .notes   .predictions .workflow 
#>   <list>            <chr>            <list>   <list>   <list>       <list>    
#> 1 <split [456/116]> train/test split <tibble> <tibble> <tibble>     <workflow>

collect_metrics(final_fit)
collect_predictions(final_fit)

#plot actual vs predicted in test set
collect_predictions(final_fit) %>%
  ggplot(aes(latency, .pred)) + 
  geom_abline(lty = 2, col = "deeppink4", size = 1.5) +
  geom_point(alpha = 0.5) +
  coord_obs_pred()

#you can extract the workflow for use in production environment
extract_workflow(final_fit)
#> ══ Workflow [trained] ════════════════════════════════════════════════
#> Preprocessor: Formula
#> Model: rand_forest()
#> 
#> ── Preprocessor ──────────────────────────────────────────────────────
#> latency ~ .
#> 
#> ── Model ─────────────────────────────────────────────────────────────
#> Ranger result
#> 
#> Call:
#>  ranger::ranger(x = maybe_data_frame(x), y = y, num.trees = ~1000,      num.threads = 1, verbose = FALSE, seed = sample.int(10^5,          1)) 
#> 
#> Type:                             Regression 
#> Number of trees:                  1000 
#> Sample size:                      456 
#> Number of independent variables:  4 
#> Mtry:                             2 
#> Target node size:                 5 
#> Variable importance mode:         none 
#> Splitrule:                        variance 
#> OOB prediction error (MSE):       3124.583 
#> R squared (OOB):                  0.3531813


##The stacks package allows you to use predictions that are a combination of two models.

library(stacks)
stack_ctrl <- control_resamples(save_pred = TRUE, save_workflow = TRUE)
#A linear model
lr_res <- 
  # define model spec
  linear_reg() %>%
  set_mode("regression") %>%
  # add to workflow
  workflow(preprocessor = latency ~ .) %>%
  # fit to resamples
  fit_resamples(frog_folds, control = stack_ctrl)

#A random forest model
rf_res <- 
  # define model spec
  rand_forest() %>%
  set_mode("regression") %>%
  # add to workflow
  workflow(preprocessor = latency ~ .) %>%
  # fit to resamples
  fit_resamples(frog_folds, control = stack_ctrl)

#initilizes the stack
frog_st <- stacks()

frog_st
#> # A data stack with 0 model definitions and 0 candidate members.

frog_st <- frog_st %>%
  add_candidates(lr_res) %>%
  add_candidates(rf_res)

frog_st
#> # A data stack with 2 model definitions and 2 candidate members:
#> #   lr_res: 1 model configuration
#> #   rf_res: 1 model configuration
#> # Outcome: latency (numeric)

frog_st_res <- frog_st %>%
  blend_predictions()

frog_st_res
#> # A tibble: 2 × 3
#>   member     type        weight
#>   <chr>      <chr>        <dbl>
#> 1 rf_res_1_1 rand_forest  0.635
#> 2 lr_res_1_1 linear_reg   0.344

frog_st_res <- frog_st_res %>%
  fit_members()

frog_test %>%
  select(latency) %>%
  bind_cols(
    predict(frog_st_res, frog_test)
  ) %>%
  ggplot(aes(latency, .pred)) + 
  geom_abline(lty = 2, 
              col = "deeppink4", 
              size = 1.5) +
  geom_point(alpha = 0.5) +
  coord_obs_pred()
