


#' Define the residualization formula.
#' 
#' @param linear_covariates (optional) default is empty
#' @param FE_covariates (optional) default is empty
#' 
#' @export
SSIV_residualization_formula <- function(linear_covariates = c(), FE_covariates = c()) {
  formula <- " ~ 1"
  if (length(linear_covariates) > 0) {
    formula <- paste(c(formula, linear_covariates), collapse = " + ")
  } 
  if (length(FE_covariates) > 0) {
    for (ii in 1:length(FE_covariates)) {
      FE_covariates[ii] <- sprintf("factor(%s)", FE_covariates[ii])
    }
    FE_covariates_sum <- paste(FE_covariates, collapse = " + ")
    formula <- paste(c(formula, FE_covariates_sum), collapse = " | ")
  } else {
    formula <- paste(c(formula, "0"), collapse = " | ")
  }
  return(formula)
}


#' This function has two tasks: residualize Y,X variables, then collapse to location
#' 
#' @param dd Data in which to replace variables with residualized versions.
#' @param Y_var  outcome variable (missings are allowed)
#' @param W_var name of weights
#' @param time_variable (optional) panel dimension
#' @param unit_identifier (optional) unique unit identifier, assumed to be 'location' if not specific
#' @param linear_covariates (optional) default is empty
#' @param FE_covariates (optional) default is empty
#' 
#' @export
SSIV_residualization_with_missings <- function(dd, Y_var, W_var, time_variable, unit_identifier, linear_covariates=c(), FE_covariates=c()) {
  formula <- SSIV_residualization_formula(linear_covariates = linear_covariates, FE_covariates = FE_covariates)
  flog.info("residualization formula for %s is %s", Y_var, formula)
  dd[, y_unit := copy(get(Y_var))]
  keep_vars <- c("location", "y_unit", W_var, linear_covariates, FE_covariates)
  merge_vars <- c("location")
  if (time_variable != "") {
    keep_vars <- c(keep_vars, time_variable)
    merge_vars <- c(merge_vars, time_variable)
  }
  if (unit_identifier != "") {
    keep_vars <- c(keep_vars, unit_identifier)
    merge_vars <- c(merge_vars, unit_identifier)
  }
  dd2 <- dd[, .SD, .SDcols = keep_vars]
  invisible(gc())
  dd2 <- na.omit(dd2)
  invisible(gc())
  dd2 <- dd2[!is.infinite(y_unit)]
  dd2$y_unit <- felm(as.formula(sprintf("y_unit %s", formula)), data = dd2, weights = dd2[, get(W_var)])$residuals
  flog.info("finished estimating residualization formula for %s", Y_var)
  dd2 <- dd2[, .SD, .SDcols = c(merge_vars, "y_unit")]
  dd[, y_unit := NULL]
  initial_size <- nrow(dd)
  flog.info("Left side of merge: %s. Right side of merge: %s.",initial_size, nrow(dd2))
  dd <- merge(dd, dd2, by = merge_vars, all.x = T)
  if(initial_size < nrow(dd)){
    stop(flog.info("BROKEN: somehow dd became larger when merging back in the residualized outcome %s.", Y_var))
  }
  setnames(dd, c(Y_var, "y_unit"), c(paste0(Y_var, "_raw"), Y_var))
  return(dd)
}


#' This function has two tasks: residualize Y,X variables, then collapse to location
#' 
#' @param location_outcomes Location outcomes
#' @param time_variable (optional) panel dimension
#' @param unit_identifier (optional) unique unit identifier, assumed to be 'location' if not specific
#' @param Y_vars vector of outcome variable names (missings are allowed)
#' @param X_vars vector of X variable names (missings are allowed but discouraged)
#' @param W_var name of weights
#' @param linear_covariates (optional) default is empty
#' @param FE_covariates (optional) default is empty
#' 
#' @export
SSIV_outcomes <- function(location_outcomes, # input data
                          time_variable = "", unit_identifier = "location", # deal with panels and micro-data
                          Y_vars, X_vars, W_var, # variable names
                          linear_covariates = c(), FE_covariates = c() # residualize
) {

  ## residualize X's and Y's
  if (length(linear_covariates) > 0 | length(FE_covariates) > 0) {
    flog.info("BEGIN residualizing")
    for (vv in c(Y_vars, X_vars)) {
      location_outcomes <- SSIV_residualization_with_missings(dd = location_outcomes, Y_var = vv, W_var = W_var, 
                                                            time_variable = time_variable, unit_identifier = unit_identifier, 
                                                            linear_covariates = linear_covariates, FE_covariates = FE_covariates)
    }
    flog.info("DONE all residualizing.")
  }

  flog.info("BEGIN collapse to the location level (by time if applicable)")
  output_identifiers <- c("location")
  if (time_variable != "") {
    output_identifiers <- c(output_identifiers, time_variable)
  }

  flog.info("collecting sum of weights")
  location_weights <- data.table(location = location_outcomes[,unique(location)])
  for(vv in Y_vars){
    weights_var <- location_outcomes[!is.na(get(vv))][, .SD, .SDcols = c('location', W_var)][, lapply(.SD, sum), by = 'location']
    setnames(weights_var,W_var,paste0('W_i_',vv))
    weights_var[, (paste0('W_i_',vv)) := get(paste0('W_i_',vv))/sum(get(paste0('W_i_',vv)))]
    location_weights <- merge(location_weights,weights_var,by='location')
  }
  
  flog.info("collecting weighted mean of Y_vars and X_vars")
  output_data <- location_outcomes[, .SD, .SDcols = c(output_identifiers, Y_vars, X_vars, W_var)][, lapply(.SD, weighted.mean, w = get(W_var), na.rm = T), by = output_identifiers]
  output_data[, (W_var) := NULL]
  output_data <- merge(output_data, location_weights, by = 'location', all.x=TRUE)

  return(output_data)
}


#' Replace industry with (industry,date) and location with (location,date)
#' 
#' @param location_outcomes Location outcomes
#' @param industry_shocks Industry shocks
#' @param industry_location_shares Industry location shares
#' @param time_variable Time identifier
#' 
#' @export
SSIV_panelID <- function(location_outcomes, industry_shocks, industry_location_shares, time_variable) {

  ## define inner panel identifier function
  SSIV_panelID_inner <- function(location_outcomes, industry_shocks, industry_location_shares, time_variable) {
    setnames(location_outcomes, c("location"), c("location_raw"))
    setnames(industry_shocks, c("industry"), c("industry_raw"))
    setnames(industry_location_shares, c("industry", "location"), c("industry_raw", "location_raw"))
    industry_location_shares[, location := .GRP, by = c("location_raw", time_variable)]
    industry_location_shares[, industry := .GRP, by = c("industry_raw", time_variable)]
    industry_location_shares <- industry_location_shares[, industry := factor(industry, levels = unique(industry_location_shares$industry))][order(industry, location)]
    map_industries <- unique(industry_location_shares[, .SD, .SDcols = c("industry", "industry_raw", time_variable)])
    industry_shocks <- merge(industry_shocks, map_industries, by = c("industry_raw", time_variable))
    map_locations <- unique(industry_location_shares[, .SD, .SDcols = c("location", "location_raw", time_variable)])
    location_outcomes <- merge(location_outcomes, map_locations, by = c("location_raw", time_variable))
    industry_location_shares[, c("year", "location_raw", "industry_raw") := NULL]
    return(list(industry_location_shares = industry_location_shares, industry_shocks = industry_shocks, location_outcomes = location_outcomes))
  }

  # panel identifiers
  flog.info("prepare panel identifiers")
  if (!(time_variable %in% names(location_outcomes))) {
    stop(flog.info("You provided time_variable=%s, which does not exist in the provided location_outcomes data.", time_variable))
  }
  if (!(time_variable %in% names(industry_shocks))) {
    stop(flog.info("You provided time_variable=%s, which does not exist in the provided industry_shocks data.", time_variable))
  }
  if (!(time_variable %in% names(industry_location_shares))) {
    stop(flog.info("You provided time_variable=%s, which does not exist in the provided industry_location_shares data.", time_variable))
  }
  
  flog.info("You provided time_variable=%s. Each (location, time_variable) pair will be treated like a separate location, and each (industry, time_variable) pair will be treated like a separate industry.  For further information, see Borusyak, Hull, and Jaravel (2019, Sec 3.3).", time_variable)
  output_data <- SSIV_panelID_inner(industry_location_shares = industry_location_shares, industry_shocks = industry_shocks, location_outcomes = location_outcomes, time_variable = time_variable)
  
  flog.info("finished panel identifiers")
  return(output_data)
}
