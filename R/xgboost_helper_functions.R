##########################################
#XGBoost custom performance functions
##########################################

fscore <- function(preds, dtrain){
  if(!require(ROCR))
    return('Library ROCR is not available. Please install it.')
  labels <- getinfo(dtrain, "label")
  pred = prediction(preds, labels)
  perf_f = performance(pred, "f")
  f_df <- data.frame(Fsc = perf_f@y.values[[1]], CUT = perf_f@x.values[[1]])
  fscoreMax <- max(f_df$Fsc, na.rm = TRUE)
  return(list(metric = "fscore", value =fscoreMax))
}

costError <- function(preds, dtrain){
  if(!require(ROCR))
    return('Library ROCR is not available. Please install it.')
  labels <- getinfo(dtrain, "label")
  fp <- sum(preds>=0.5 & labels==0)
  fn <- sum(preds<0.5 & labels==1)
  err = (0.75*fp + 0.25*fn)/length(labels)
  return(list(metric = "costerror", value = -err))
}

precision <- function(preds, dtrain){
  if(!require(ROCR))
    return('Library ROCR is not available. Please install it.')
  labels <- getinfo(dtrain, "label")
  pred = prediction(preds, labels)
  perf_f = performance(pred, "prec")
  f_df <- data.frame(Precision = perf_f@y.values[[1]], CUT = perf_f@x.values[[1]])
  precMax <- max(f_df$Precision, na.rm = TRUE)
  precMean <- mean(f_df$Precision[which(f_df$CUT>0.4)], na.rm = TRUE)
  return(list(metric = "precision", value =precMean))
}

lift <- function(preds, dtrain){
  if(!require(ROCR))
    return('Library ROCR is not available. Please install it.')
  labels <- getinfo(dtrain, "label")
  scores <- preds
  target_test <- as.factor(labels)
  levels(target_test) <- list(Good="0", Bad="1")
  results <- data.frame(score = scores, target = target_test)
  lift <- makeLiftChart(results, 10, TRUE)
  return(list(metric = 'lift', value = lift$lift[1]))
}

##########################################
#XGBoost Tuning - Grid Search
##########################################

#Tuning using a single cross validation
gridDefault <- function(searchGrid, dtrain, numtrees, eval_metric, metric, feval, objective){
  if(!require(xgboost))
    return('Library xgboost is not available. Please install it.')
  
  #THE XGBOOST CV MODEL WITH PARAMETERS
  boostCV<-xgb.cv(data=dtrain, nrounds=numtrees, nfold=5, showsd=FALSE, verbose=TRUE,
                  maximize=TRUE, metrics=metric, eval_metric=feval,objective=objective, 
                  max_depth=searchGrid[['max_depth']], min_child_weight = searchGrid[['min_child_weight']],
                  gamma = searchGrid[['gamma']], colsample_bytree = searchGrid[['colsample_bytree']], 
                  eta=searchGrid[['eta']], subsample = searchGrid[['subsample']], scale_pos_weight = searchGrid[['scale_pos_weight']])
  
  
  #XGB CV RESULTS
  xvalidationScores<-as.data.frame(boostCV)
  #SAVE metrics
  metric1 <- paste0('test.', eval_metric, '.mean')
  metric2 <- paste0('test.', metric, '.mean')
  
  score1<-xvalidationScores[numtrees,metric1]
  if(length(metric) > 0)
    score2<-xvalidationScores[numtrees,metric2]
  else
    score2 <- 0
  
  #NOTIFY OF ROUND 
  writeLines(paste0('Loop completed with ', eval_metric, ' of ', round(score1*100,3), '% and ', metric,' of ',round(score2*100,3),'% for 
                    \n max_depth = ',searchGrid[["max_depth"]], 
                    '\n min_child_weight = ',searchGrid[["min_child_weight"]],
                    '\n gamma = ', searchGrid[['gamma']],
                    '\n colsample_bytree = ', searchGrid[['colsample_bytree']],
                    '\n eta = ', searchGrid[['eta']],
                    '\n subsample = ', searchGrid[['subsample']],
                    '\n scale_pos_weight = ', searchGrid[['scale_pos_weight']]))
  
  #KEEP RESULTS
  res <- data.frame(max_depth = searchGrid[["max_depth"]], min_child_weight = searchGrid[["min_child_weight"]], gamma = searchGrid[['gamma']], 
                    colsample_by_tree = searchGrid[['colsample_bytree']], eta = searchGrid[['eta']], subsample = searchGrid[['subsample']], 
                    scale_pos_weight = searchGrid[['scale_pos_weight']])
  res[eval_metric] <- score1
  res[metric] <- score2
  
  return(res)
}

#Tuning using multiple boosts and looking at the mean of all as final result
gridCross <- function(searchGrid, dtrain, numtrees, eval_metric, metric, feval, objective, folds, reps, seed, nthread){
  if(!require(xgboost))
    return('Library xgboost is not available. Please install it.')
  
  #For each gain limit tune parameters using reps cross validations to eliminate noise
  scores1 <- rep(0,reps)
  scores2 <- rep(0,reps)
  
  metric1 <- paste0('test_', eval_metric, '_mean')
  metric2 <- paste0('test_', metric, '_mean')
  
  
  for (j in 1:reps) {
    #THE XGBOOST CV MODEL WITH PARAMETERS
    boostCV<-xgb.cv(data=dtrain, nrounds=numtrees, nfold=5, showsd=FALSE,
                    metrics=metric, verbose=TRUE, eval_metric= feval, folds = folds, seed = seed, nthread = nthread,
                    objective=objective, max_depth=searchGrid[["max_depth"]], eta=searchGrid[["eta"]],
                    maximize=TRUE, gamma = searchGrid[["gamma"]], colsample_bytree = searchGrid[["colsample_bytree"]],
                    min_child_weight = searchGrid[["min_child_weight"]], scale_pos_weight = searchGrid[["scale_pos_weight"]],
                    alpha = searchGrid[["alpha"]], lambda = searchGrid[["lambda"]])
    
    #XGB CV RESULTS
    perf <- data.frame(boostCV$evaluation_log)
    scores1[j] <- as.numeric(perf[nrow(boostCV$evaluation_log),metric1]) 
    if(is.na(feval))
      scores2[j]<-as.numeric(perf[nrow(boostCV$evaluation_log),metric2])
    else
      scores2[j] <- 0
  }
  
  
  #SAVE AUC
  score1<-mean(scores1)
  score2 <- mean(scores2)
  
  #NOTIFY OF ROUND 
  writeLines(paste0('Loop completed with ', eval_metric, ' of ', round(score1*100,3), '% and ', metric,' of ',round(score2*100,3),'% for 
                    \n max_depth = ',searchGrid[["max_depth"]], 
                    '\n min_child_weight = ',searchGrid[["min_child_weight"]],
                    '\n gamma = ', searchGrid[['gamma']],
                    '\n colsample_bytree = ', searchGrid[['colsample_bytree']],
                    '\n eta = ', searchGrid[['eta']],
                    '\n subsample = ', searchGrid[['subsample']],
                    '\n scale_pos_weight = ', searchGrid[['scale_pos_weight']],
                    '\n alpha = ', searchGrid[['alpha']],
                    '\n lambda = ', searchGrid[['lambda']]))
  
  #KEEP RESULTS
  res <- data.frame(max_depth = searchGrid[["max_depth"]], min_child_weight = searchGrid[["min_child_weight"]], gamma = searchGrid[['gamma']], 
                    colsample_by_tree = searchGrid[['colsample_bytree']], eta = searchGrid[['eta']], subsample = searchGrid[['subsample']], 
                    scale_pos_weight = searchGrid[['scale_pos_weight']], alpha = searchGrid[["alpha"]], lambda = searchGrid[['lambda']])
  
  res[eval_metric] <- score1
  res[metric] <- score2
  res[paste0('sd_',eval_metric)] <- sd(scores1)
  res[paste0('sd_', metric)] <- sd(scores2)
  
  return(res)
  
}

#Perform XGBoost tuning
tuneXGBoost <- function(data, gridFunction = "gridCross", numtrees=100, max_depth = c(5), min_child_weight= c(8), 
                        gamma=c(0), colsample_bytree=c(0.5), eta = c(0.1), subsample =c(1), scale_pos_weight = c(1),
                        alpha = c(0), lambda = c(1),
                        eval_metric = 'auc', metric = 'map', feval = NA, objective = 'binary:logistic', 
                        nthread = 1, folds = NULL, reps = 5, seed = 8675309){
  if(is.na(feval))
    feval <- eval_metric
  
  #Tune max_depth and min_child_weight
  searchGrid <- expand.grid(max_depth=max_depth, 
                            min_child_weight = min_child_weight,
                            gamma = gamma,
                            colsample_bytree = colsample_bytree,
                            eta = eta,
                            subsample = subsample,
                            scale_pos_weight = scale_pos_weight,
                            alpha = alpha,
                            lambda = lambda)
  
  searchGrid$row <- 1:nrow(searchGrid)
  
  if(gridFunction == "gridDefault")
    parameters<-ddply(searchGrid, "row", function(x) gridDefault(x, data, numtrees, eval_metric, metric, feval, objective, folds))
  
  else (gridFunction = "gridCross")
  parameters<-ddply(searchGrid, "row", function(x) gridCross(x, data, numtrees, eval_metric, metric, feval, objective, folds, reps, seed, nthread))
  
  return(parameters)
}

##########################################
#XGBoost Tuning - Bayesian Optimization
##########################################

#Helper function for tuning using Bayesian optimization
xgb_fit_bayes <- function(max_depth = 5, min_child_weight= 10, gamma=0, colsample_bytree=0.8, eta = 0.1,
                          subsample =0.8, scale_pos_weight = 1, alpha = 0, lambda = 1, nround = 150){
  score <- 0
  n <- 1
  
  for(i in 1:n){
    cv <- xgb.cv(booster = "gbtree", eta = eta,
                 max_depth = max_depth,
                 min_child_weight = min_child_weight,
                 subsample = subsample, colsample_bytree = colsample_bytree,
                 lambda = lambda, alpha = alpha,
                 objective = "binary:logistic",
                 eval_metric = 'auc',
                 data = dtrain, nround = nround,
                 folds = cv_folds, prediction = TRUE, showsd = TRUE,
                 early_stopping_rounds = 5, maximize = TRUE, verbose = TRUE, nthread = 30)
    
    res <- cv$evaluation_log
    score = score + res$test_auc_mean[nrow(res)]
  }
  
  list(Score = score/n, Pred = 0)
}

#Tune using Bayesian optimization
runBayesOptimization <- function(train, target){
  
  if(!require(xgboost))
    return('Library xgboost is not available. Please install it.')
  if(!require(rBayesianOptimization))
    return('Library rBayesianOptimization is not available. Please install it.')
  if(!require(dplyr))
    return('Library dplyr is not available. Please install it.')
  
  dtrain <- xgb.DMatrix(as.matrix(train %>% 
                        select(-one_of(target))),label = train[,target], missing = NA)
  
  cv_folds <- KFold(train$target, nfolds = 10, stratified = TRUE, seed = 8675309)
  
  start <- Sys.time()
  OPT_bayes <- BayesianOptimization(xgb_fit_bayes, bounds = list(
    min_child_weight = c(0L, 10L),
    gamma = c(0,5),
    colsample_bytree = c(0.3,1)),
    init_points = 20, n_iter = 30, acq = 'ucb', verbose = T
  )
  durationBayes <- Sys.time() - start
  message(durationBayes)
}