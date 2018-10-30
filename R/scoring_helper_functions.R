##########################################
#Scoring functions
##########################################

#' Helper for ROC plot
#' @param scr Model score
#' @param data Dataset containing the target variable
#' @param target Column name of the target variable
#' @return Plot of ROC curve and the corresponding dataset
getROC <- function(scr, data, target) {
  if(!require(ROCR))
    return('Library ROCR is not available. Please install it.')
  pred = prediction(scr, data[, target])
  ROC <- performance(pred, "tpr", "fpr")
  plot(ROC, col = rainbow(10))
  abline(a = 0, b = 1)
  grid()
  return(ROC)
}

#' Helper for AUC
#' @param scr Model score
#' @param data Dataset containing the target variable
#' @param target Column name of the target variable
#' @return AUC of the model score
getAuc<- function(scr, data, target){
  if(!require(ROCR))
    return('Library ROCR is not available. Please install it.')
  pred = prediction(scr, data[, target])
  return(performance(pred, "auc")@y.values[[1]])
}

#' Helper for FScore
#' @param scr Model score
#' @param data Dataset containing the target variable
#' @param target Column name of the target variable
#' @return FScore of the model score
getFScore <- function(scr, data, target){
  if(!require(ROCR))
    return('Library ROCR is not available. Please install it.')
  pred = prediction(scr, data[, target])
  perf_f = performance(pred, "f")
  f_df <- data.frame(Fsc = perf_f@y.values[[1]], CUT = perf_f@x.values[[1]])
  fscoreMax <- max(f_df$Fsc, na.rm = TRUE)
  fscoreMean <- mean(f_df$Fsc)
  return(fscoreMax)
}


#' Score data - caret models
#' @param model Caret model object
#' @param data Test set
#' @param target Column name of the target variable
#' @return Lift chart for the model
score <- function(model, test, target){
  if(!require(pROC))
    return('Library pROC is not available. Please install it.')
  
  scores <- predict(model, test %>% select(-one_of(target)),type = "prob")[,"Bad"]
  
  target_test <- test[,target]
  
  results <- data.frame(score = scores, target = target_test)
  lift <- makeLiftChart(results, 10, TRUE)
  
  ROC <- roc(predictor=scores,
             response=target_test,
             levels=rev(levels(target_test)))
  
  lift$auc <- ROC$auc
  print(ROC$auc)
  return(lift)
}

#' Score data - XGB models
#' @param model XGBoost model object
#' @param test Test set
#' @param target Column name of the target variable
#' @return Lift chart for the model
getLift <- function(model, test, target){
  if(!require(xgboost))
    return('Library xgboost is not available. Please install it.')
  
  dtest <- xgb.DMatrix(as.matrix(test %>% select(-one_of(target))), missing = NA)
  scores <- predict(model, dtest)
  target_test <- as.factor(test[,target])
  levels(target_test) <- list(Good="0", Bad="1")
  results <- data.frame(score = scores, target = target_test)
  lift <- makeLiftChart(results, 10, FALSE)
  ROC <- getROC(scores, test, target)
  auc<- getAuc(scores, test, target)
  lift$auc <- auc
  return(list(lift = lift, scores = scores))
}

##########################################
#Helper functions
##########################################

#' Run a Query using RODBC
#' @param query String with query
#' @param database Database name
#' @return Result of query as a data frame
performQuery <- function(query, database = "Hive"){
  if(!require(RODBC))
    return('Library RODBC is not available. Please install it.')
  odbc_Risk <- odbcConnect(database)
  #CLEAN UP ANY LINE BREAKS AND SPACES
  sqlQuery(odbc_Risk,'set hive.execution.engine=spark;', believeNRows=FALSE, stringsAsFactors=FALSE)
  query<-strwrap(gsub("[\r\n\t]"," ",query), width=100000000, simplify=TRUE)
  #PUSH THE QUERY TO THE SERVER USING THE ODBC CONNECTION AND PULL BACK ALL DATA
  data<-sqlQuery(odbc_Risk, query, believeNRows=FALSE, stringsAsFactors = FALSE, as.is = TRUE)
  odbcClose(odbc_Risk)
  return(data)
}

#Check whether an argument isn't NA ( just to save some time when writing code)
not.na <- function(x)
  return (!is.na(x))

#Binary operator opposite of %in%
`%nin%` <- function(a, b){
  return(! a %in% b)
}

#Binary operator similar to == , except that it accounts for NA => NA %==% NA is TRUE, while NA %==% to anything else is FALSE
`%==%` <- Vectorize(function(e1, e2) {
  if(is.na(e1) & is.na(e2))
    return(TRUE)
  else 
    return(isTRUE(e1 == e2))
})