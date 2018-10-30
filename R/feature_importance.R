##########################################
#Feature importance
##########################################

#' Runs the stability selection with lasso algorithm to select most important variables. See http://stat.ethz.ch/~nicolai/stability.pdf
#' @param data dataset with variables and target.
#' @param numvars Number of variables considered in each subsample.
#' @param cutoff Probability cutoff, only variables with probability higher than it are selected. Can range from 0.5 to 1, recommended is from 0.6 to 0.9.
#' @param return_df TRUE/FALSE, whether to return just the variable list(F) or the data frame with details as well (T)
#' @return Data frame consisting of selected variables and a dataframe with details if return_df == TRUE
stability_selection_lasso <- function(data, numvars=100, cutoff=0.6, return_df = F){
  if(!require(stabs))
    return('Library stabs is not available. Please install it.')
  
  data$target <- as.integer(as.character(data$target))
  if(cutoff !=0){
    stab <- stabsel(x= model.matrix(target~., data), y = data$target, fitfun = glmnet.lasso, cutoff=cutoff, q = numvars)
    probs <- as.data.frame(sort(stab$max, decreasing = TRUE))
    colnames(probs) <- c('P')
    selected_vars <- rownames(probs)[which(probs$P > cutoff)]
  }
  else
  {
    stab <- glmnet.lasso(model.matrix(target~., data), data$target, numvars)
    selected_vars <- names(stab$selected[which(stab$selected ==TRUE)])
    probs <- as.data.frame(stab$path)
  }
  
  if(return_df)
    return(list(st_sel_vars = selected_vars, st_df = probs))
  else
    return(selected_vars)
}


#' IV calculation
#' @param iv_data Dataset containing all the variables to calculate the IV for
#' @param iv_response Name of the response variable (it has to be binary)
#' @param iv_parallel Boolean, whether to use parallel processing or no. Default TRUE
#' @param cores Number of cores to use if the iv_parallel flag is set to TRUE
#' @return Dataset with iv values for each of the attributes in the initial dataset
calculateIV <- function(iv_data, iv_response, iv_parallel = FALSE, cores = 1){
  if(!require(Information))
    return('Library Information is not available. Please install it.')
  
  IV <- Information::create_infotables(data=iv_data, y=iv_response, parallel=iv_parallel, ncore = cores)
  IV <- as.data.frame(IV$Summary)
  return(IV)
}


#' Creates a dataset with summary of selected model features and their importance
#' 
#' @param test Scored hold-out set. Has to contain all the model variables. Currently assumed that any special values are taken care of prior to usage
#' @param score_col Column name of the score column
#' @param features Dataset containing model features ranked by importance with a corresponding importance value
#' @param feat_col Column name for the feature name
#' @param imp_col Column name for the feature importance
#' @param dict A dictionary
#' @param dict_feat_col Column name for the feature name in dictionary
#' @param bureau_col Column name for the bureau in the dictionary
#' @param desc_col Column name for the description in the dictionary
#' @param agg function to use as the aggregate
#' @return A dataset with details about each variable, including bureau, description, and values for top and bottom 10% of the population
modelFeatures <- function(test, score_col = 'score', features, feat_col = 'feature', imp_col = 'importance', 
                          dict, dict_feat_col = 'feature', bureau_col = 'bureau', desc_col = 'description', agg = mean){
  if(!require(fuzzyjoin))
    return('Library fuzzyjoin is not available. Please install it.')
  if(!require(dplyr))
    return('Library dplyr is not available. Please install it.')
  
  #Cast to data frame
  if(class(test) != 'data.frame')
    test <- as.data.frame(test, stringsAsFactors = F)
  
  if(class(features) != 'data.frame')
    features <- as.data.frame(features, stringsAsFactors = F)
  
  if(class(dict) != 'data.frame')
    dict <- as.data.frame(dict, stringsAsFactors = F)
  
  #Rename all the columns
  colnames(features)[which(colnames(features) == feat_col)] <- 'Feature'
  colnames(dict)[which(colnames(dict) == dict_feat_col)] <- 'Feature'
  colnames(features)[which(colnames(features) == imp_col)] <- 'Importance'
  colnames(dict)[which(colnames(dict)==bureau_col)] <- 'Bureau'
  colnames(dict)[which(colnames(dict) == desc_col)] <- 'Description' 
  
  #Keep only the feature name column and importance
  features <- features %>% select(one_of(c('Feature', 'Importance')))
  
  #Merge with the dictionary to get the bureaus and variable names
  feats1 <- regex_left_join(features, dict %>% select(one_of(c('Feature', 'Bureau', 'Description'))), by = 'Feature')
  
  #Remove redundant rows
  feats1 <- feats1 %>% 
    dplyr::group_by(Feature.x) %>%
    dplyr::arrange(desc(nchar(Feature.y))) %>%
    dplyr::slice(1) %>% 
    ungroup()
  
  #Merge back
  feats_out <- features %>% left_join(feats1 %>% select(one_of('Feature.x', 'Bureau', 'Description')), by = c('Feature' = 'Feature.x'))
  
  
  #Column name
  col <- ifelse(identical(agg,mean), 'Mean', "Median")
  #Calculate overall median  
  median<- test %>%
    select(one_of(as.character(features$Feature))) %>%
    summarise_all(funs(agg(., na.rm = TRUE)))
  
  feats_out[col] <- round(as.numeric(median),2)
  topQuant <- as.numeric(quantile(test[,score_col], 0.9))
  bottomQuant <- as.numeric(quantile(test[,score_col], 0.1))
  meansTop <- test %>%
    filter(.[[score_col]] >= topQuant) %>%
    select(one_of(as.character(features$Feature))) %>%
    summarise_all(funs(agg(., na.rm = TRUE)))
  feats_out[paste0(col, '_top10%')] <- round(as.numeric(meansTop),2)
  
  meansBottom <- test %>%
    filter(.[[score_col]] <= bottomQuant) %>%
    select(one_of(as.character(features$Feature))) %>%
    summarise_all(funs(agg(., na.rm = TRUE)))
  feats_out[paste0(col, '_bottom10%')] <- round(as.numeric(meansBottom),2)
  
  return(feats_out)
}