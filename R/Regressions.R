
#' Raw location regressions
#' 
#' @param location_data Location data
#' @param lagIV (optional) default is FALSE
#' 
#' @export
Uncorrected_location_regressions <- function(location_data, lagIV=FALSE) {
  
  flog.info("Provided methods include 'Uncorrected', so estimating IV at the location level.")
  
  results <- data.table(method = "Uncorrected")
  
  insert_covariates <- ""
  if(lagIV){
    insert_covariates <- paste0(insert_covariates, " + Z_i_lag ")
  }
  secondstage_formula <- sprintf("Y_i ~ X_i%s| Z_i%s", insert_covariates, insert_covariates)
  firststage_formula <- sprintf("X_i ~ Z_i%s", insert_covariates)
  reducedform_formula <- sprintf("Y_i ~ Z_i%s", insert_covariates)
  
  location_data <<- location_data # this is the moment I hate R the most, so stupid to have to do this
  mod <- ivreg(secondstage_formula, data = location_data, weights = location_data$W_i)
  res <- robust.se(mod)
  results$secondstage_coef <- res[2, 1]
  results$secondstage_se <- res[2, 2]
  results$secondstage_tstat <- res[2, 3]
  results$secondstage_pval <- res[2, 4]
  
  mod <- lm(firststage_formula, data = location_data, weights = location_data$W_i)
  res <- robust.se(mod)
  results$firststage_coef <- res[2, 1]
  results$firststage_se <- res[2, 2]
  results$firststage_tstat <- res[2, 3]
  results$firststage_pval <- res[2, 4]
  
  mod <- lm(reducedform_formula, data = location_data, weights = location_data$W_i)
  res <- robust.se(mod)
  results$reducedform_coef <- res[2, 1]
  results$reducedform_se <- res[2, 2]
  results$reducedform_tstat <- res[2, 3]
  results$reducedform_pval <- res[2, 4]
  
  return(results)
}

#' Apply the BHJ transformation to the industry level
#' 
#' @param industry_location_data Industry location data
#' @param lagIV (optional) default is FALSE
#' @param industryFE (optional) default is FALSE
#' @param timeFE (optional) default is FALSE
#' 
#' @export
BHJ_transform_to_industry <- function(industry_location_data, lagIV=FALSE, lagsIV=1, industryFE = FALSE, timeFE = FALSE) {
  
  byvars <- "industry"
  if(industryFE | timeFE){
    byvars <- c(byvars, "industry_raw", "year")
  }
  
  industry_data <- industry_location_data[, list(
    Xbar_j = sum(W_i * S_ij * X_i) / sum(W_i * S_ij),
    Ybar_j = sum(W_i * S_ij * Y_i) / sum(W_i * S_ij),
    Zbar_j = sum(W_i * S_ij * Z_i) / sum(W_i * S_ij),
    Sbar_j = sum(W_i * S_ij),
    G_j = G_j[1]
  ), by = byvars]
  
  if(lagIV){
    industry_data <- merge(industry_data, industry_location_data[,list(G_j_lag=G_j_lag[1]),industry], by='industry')
    if(lagsIV >= 2){
      industry_data <- merge(industry_data, industry_location_data[,list(G_j_lag2=G_j_lag2[1]),industry], by='industry')
    }
    if(lagsIV >= 3){
      industry_data <- merge(industry_data, industry_location_data[,list(G_j_lag3=G_j_lag3[1]),industry], by='industry')
    }
    if(lagsIV >= 4){
      industry_data <- merge(industry_data, industry_location_data[,list(G_j_lag4=G_j_lag4[1]),industry], by='industry')
    }
  }

  return(industry_data)
}

#' Check BHJ proposition 1
#' 
#' @param results Results
#' 
#' @export
BHJ_check_proposition1 <- function(results) {
  check_location_deviation <- 0.0
  check_location_deviation <- check_location_deviation + results[, diff(firststage_coef) / firststage_coef[1]]
  check_location_deviation <- check_location_deviation + results[, diff(secondstage_coef) / secondstage_coef[1]]
  check_location_deviation <- check_location_deviation + results[, diff(reducedform_coef) / reducedform_coef[1]]
  if (check_location_deviation / 3 > 1e-3) {
    flog.info("Warning: BHJ Proposition 1 appears to have failed. The point estimates are not identical at the location-level and at the industry-level, up to a tolerance level of 1e-3.")
  } else {
    flog.info("Check: BHJ Proposition 1 holds. The Uncorrected location regression and BHJ industry regression provide the same point estimate.")
  }
}

#' BHJ industry regressions
#' 
#' @param industry_data Industry data
#' @param lagIV (optional) default is FALSE
#' @param timeFE (optional) default is FALSE
#' @param industryFE (optional) default is FALSE
#' 
#' @export
BHJ_industry_regressions <- function(industry_data, lagIV=FALSE, lagsIV=1, timeFE=FALSE, industryFE=FALSE) {
  
  flog.info("Provided methods include 'BHJ', so computing standard errors of Borusyak, Hull, and Jaravel (2019).")
  
  insert_covariates <- ""
  if(lagIV){
    insert_covariates <- paste0(insert_covariates, " + G_j_lag ")
    if(lagsIV >= 2){
      insert_covariates <- paste0(insert_covariates, " + G_j_lag2 ")
    }
    if(lagsIV >= 3){
      insert_covariates <- paste0(insert_covariates, " + G_j_lag3 ")
    }
    if(lagsIV >= 4){
      insert_covariates <- paste0(insert_covariates, " + G_j_lag4 ")
    }
  }
  if(timeFE){
    insert_covariates <- paste0(insert_covariates, " + factor(year) ")
  }
  if(industryFE){
    insert_covariates <- paste0(insert_covariates, " + factor(industry_raw) ")
  }
  
  secondstage_formula <- sprintf("Ybar_j ~ Xbar_j%s| G_j%s", insert_covariates, insert_covariates)
  firststage_formula <- sprintf("Xbar_j ~ Zbar_j%s| G_j%s", insert_covariates, insert_covariates)
  reducedform_formula <- sprintf("Ybar_j ~ Zbar_j%s| G_j%s", insert_covariates, insert_covariates)
  
  results <- data.table(method = "BHJ")
  
  # stupidest behavior of R, ivreg would fail here if industry_data wasn't passed to parent scope
  industry_data <<- copy(industry_data)
  
  flog.info("main 2SLS formula is %s",secondstage_formula)

  mod <- ivreg(secondstage_formula, data = industry_data, weights = industry_data$Sbar_j)
  res <- robust.se(mod)
  results$secondstage_coef <- res[2, 1]
  results$secondstage_se <- res[2, 2]
  results$secondstage_tstat <- res[2, 3]
  results$secondstage_pval <- res[2, 4]

  mod <- ivreg(firststage_formula, data = industry_data, weights = industry_data$Sbar_j)
  res <- robust.se(mod)
  results$firststage_coef <- res[2, 1]
  results$firststage_se <- res[2, 2]
  results$firststage_tstat <- res[2, 3]
  results$firststage_pval <- res[2, 4]

  mod <- ivreg(reducedform_formula, data = industry_data, weights = industry_data$Sbar_j)
  res <- robust.se(mod)
  results$reducedform_coef <- res[2, 1]
  results$reducedform_se <- res[2, 2]
  results$reducedform_tstat <- res[2, 3]
  results$reducedform_pval <- res[2, 4]

  return(results)
}


#' Construct share matrix for AKM estimation
#' 
#' @param industry_location_data Industry location data
#' 
#' @export
AKM_construct_share_matrix <- function(industry_location_data) {
  # locations in rows, industries in columns
  S_ij_mat <- spread(industry_location_data[, .(location, industry, S_ij)], industry, S_ij)
  S_ij_mat[is.na(S_ij_mat)] <- 0
  S_ij_mat[, location := NULL]
  S_ij_mat <- as.matrix(S_ij_mat)
  return(S_ij_mat)
}


#' Location regressions
#' 
#' @param location_data Location data
#' @param AKM_share_matrix AKM share matrix
#' @param industry_clusters (optional) default is NULL
#' 
#' @export
AKM_location_regressions <- function(location_data, AKM_share_matrix, industry_clusters = NULL) {
  flog.info("Provided methods include 'AKM', so computing standard errors of Adao, Kolesar, and Morales (2019). Warning: this estimator is very slow or infeasible with large data.")
  results <- data.table(method = "AKM")
  
  # second stage
  mod <- ivreg_ss(Y_i ~ 1 | X_i, X = Z_i, data = location_data, W = AKM_share_matrix, weights = W_i, method = c("akm"), sector_cvar = industry_clusters)
  results$secondstage_coef <- mod$beta
  results$secondstage_se <- mod$se["AKM"]
  
  
  # first stage
  mod <- reg_ss(X_i ~ 1, X = Z_i, data = location_data, W = AKM_share_matrix, weights = W_i, method = c("akm"), sector_cvar = industry_clusters)
  results$firststage_coef <- mod$beta
  results$firststage_se <- mod$se["AKM"]
  
  # reduced form
  mod <- reg_ss(Y_i ~ 1, X = Z_i, data = location_data, W = AKM_share_matrix, weights = W_i, method = c("akm"), sector_cvar = industry_clusters)
  results$reducedform_coef <- mod$beta
  results$reducedform_se <- mod$se["AKM"]
  
  return(results)
}

