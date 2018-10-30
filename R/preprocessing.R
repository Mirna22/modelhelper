##########################################
#Preprocessing
##########################################

#' Get names of columns with zero variance
#' @param dat Data frame with variables that need to be checked for zero variance
#' @return Column names of variables that have zero variance
zeroVariance <- function(dat) {
  if(!require(purrr))
    return('Library purrr is not available. Please install it.')
  if(!require(dplyr))
    return('Library dplyr is not available. Please install it.')
  
  out <- dat %>% map(~length(unique(.x)))
  zero <- which(!out > 1)
  return(colnames(dat)[zero])
}


#' Preprocess the dataset
#' @param dat Data frame to preprocess
#' @param nearzero Boolean, whether to remove near zero variables or not. Default TRUE
#' @return Preprocessed dataset
preprocess <- function(dat, nearzero = T){
  
  if(!require(purrr))
    return('Library purrr is not available. Please install it.')
  if(!require(caret))
    return('Library caret is not available. Please install it.')
  
  
  #If there are any factors turn them into characters for the time being
  if(any(map_lgl(dat, is.factor))){
    dat <- dmap_if(dat, is.factor, as.character)
  }
  
  #Replace all the missing strings with NA
  chars <- dat %>% keep(is.character)
  if(ncol(chars) > 0){
    chars[chars == "" | chars == " " | chars == "NULL" | chars == 'NA'] <- NA
    dat[names(chars)] <- chars
  }
  curr <- ncol(dat)
  
  #Drop high missing percentage variables
  dat <- dat %>%
    purrr::discard(~100*sum(is.na(.x))/length(.x) > 80)
  message(paste0('high missing:', curr-ncol(dat)))
  curr <- ncol(dat)
  
  #Drop character variables
  dat <- dat %>%
    purrr::discard(~is.character(.x) & length(unique(.x)) > 32)
  message(paste0('character vars:', curr-ncol(dat)))
  curr = ncol(dat)
  
  #Remove zero variance
  nzv <- zeroVariance(dat)
  if(length(nzv) > 0){
    dat <- dat %>% select(-one_of(nzv))
    message(paste0('zero vars:', curr-ncol(dat)))
    curr <- ncol(dat)
  }
  
  #Remove near zero variance
  if(nearzero){
    nzvar_ind <- nearZeroVar(dat, allowParallel = T, foreach = TRUE)
    if(length(nzvar_ind)>0){
      dat <- dat[, -nzvar_ind]
      message(paste0('near zero vars:', curr-ncol(dat)))
      curr <- ncol(dat)
    }
  }
  
  #Impute factors
  impute_factors <- function(char_col) {
    char_col[is.na(char_col)] <- "Missing"
    return(char_col)
  }
  
  #Turn remaining characters into factors
  if (any(map_lgl(dat, is.character))) {
    dat <- dmap_if(dat, is.character, impute_factors)
    dat <- dmap_if(dat, is.character, as.factor)
  }
  
  return(dat)
}


#' Cap floor and save the cap/floor values
#' @param dat Data frame with variables to cap/floor
#' @target Name of the target variable
#' @return List where the first element is the capped/floored dataset, and second is the dataset with details on cap and floor values
capFloor <- function(dat, target){
  ret <- dat
  caps_dat <- data.frame(feature = character(), cap_limit = numeric(), floor_limit = numeric(), cap_with = numeric(), floor_with = numeric())
  names <- colnames(dat)[which(colnames(dat)!=target)]
  for(name in names){
    x <- dat[,name]
    orig_x <- x
    qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
    caps <- quantile(x, probs=c(.05, .95), na.rm = T)
    H <- 1.5 * IQR(x, na.rm = T)
    x[x < (qnt[1] - H)] <- caps[1]
    x[x > (qnt[2] + H)] <- caps[2]
    if(sd(x, na.rm = T) != 0){
      ret[,name] <- orig_x
      caps_dat <- rbind(caps_dat, data.frame(feature = name, cap_limit = qnt[2]+H, floor_limit = qnt[1]-H, cap_with = caps[2], floor_with = caps[1]))
    }else{
      ret[,name] <- x
      caps_dat <- rbind(caps_dat, data.frame(feature = name, cap_limit = NA, floor_limit = NA, cap_with = NA, floor_with = NA))
    }
  }
  
  return(list(data = ret, caps_dat = caps_dat))
}

##########################################
#Variable stability functions
##########################################
max_5_percentile <- function(x){
  if(class(x)!='numeric' & class(x)!='integer')
    return(NA)
  limit <- quantile(x, probs = 0.95, na.rm = TRUE, type = 7)
  return(round(mean(x[x >= limit], na.rm = TRUE),2))
}

min_5_percentile <- function(x){
  if(class(x)!='numeric' & class(x)!='integer')
    return(NA)
  limit <- quantile(x, probs = 0.05, na.rm = TRUE, type = 7)
  return(round(mean(x[x <= limit], na.rm = TRUE),2))
}

miss_percentage <- function(x){
  return(round(sum(is.na(x))/length(x)*100,2))
}

mean_na_rem <- function(x){
  if(class(x)!='numeric' & class(x)!='integer')
    return(mode_NA_rem(x))
  return(round(mean(x, na.rm = TRUE),2))
}

median_na_rem <- function(x){
  if(class(x)!='numeric' & class(x)!='integer')
    return(mode_NA_rem(x))
  return(round(median(x, na.rm = TRUE),2))
}

mode_NA_rem <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' Returns the missing percentage/mean/max by month
#' @param df Data frame with variables to check for stability
#' @param func Function to apply to each variable each month and compare across
#' @param vint_col Column that contains month
#' @return Data frame with results by month and standard deviation between months
byMonth <- function(df, func, vint_col){
  if(!require(purrr))
    return('Library purrr is not available. Please install it.')
  if(!require(data.table))
    return('Library data.table is not available. Please install it.')
  
  df[,vint_col] <- as.character(df[,vint_col])
  
  #Get vintages
  vintages <- sort(unique(df[,vint_col]))
  result <- data.frame(matrix(nrow = 0, ncol = ncol(df)))
  colnames(result) = colnames(df)
  df <- as.data.frame(df %>% map_if(is.factor, as.character), stringsAsFactors = F)
  
  #Loop through vintages and summarise
  for(i in 1:length(vintages)){
    vintage <- vintages[i]
    vintData <- df[df[vint_col] == vintage,]
    res <- map(vintData, func)
    result <- rbindlist(list(result, res))
  }
  
  rownames(result) <-vintages
  result <- data.frame(result, stringsAsFactors = F)
  
  #Add standard deviation
  result['STDEV',] <- rep(NA, ncol(result))
  for(i in 1:ncol(result)){
    if(class(result[,i]) == 'numeric' | class(result[,i]) == 'integer')
      result['STDEV', i] <- round(sd(result[1:nrow(result)-1,i]),2)
  }
  
  #Return dataframe
  result <- as.data.frame(result %>% map_if(is.character, as.factor))
  vars <- colnames(result)
  result <- as.data.frame(t(result))
  colnames(result) <- c(vintages, 'stdev')
  result$feature <- vars
  
  return(result[c('feature', vintages, 'stdev')])
}
