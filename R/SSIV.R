
#' Shift Share IV estimator
#' @param location_outcomes Location outcomes
#' @param industry_shocks Industry shocks
#' @param industry_location_shares Industry location shares
#' @param methods (optional) default is c("Uncorrected", "AKM", "BHJ")
#' @param equally_weighted (optional) default is FALSE
#' @param lagIV (optional) default is FALSE
#' @param industryFE (optional) default is FALSE
#' @param timeFE (optional) default is FALSE
#' @export
SSIV <- function(
                 location_outcomes, industry_shocks, industry_location_shares, # input data
                 methods = c("Uncorrected", "AKM", "BHJ"), equally_weighted = FALSE, # main functionality
                 lagIV = FALSE, lagsIV=1, industryFE = FALSE, timeFE = FALSE  # other functionality
) {
  methods <- methods[methods %in% c("Uncorrected", "AKM", "BHJ")]
  if (length(methods) == 0) {
    stop("No methods remain after removing invalid methods.")
  }

  # equally weighted
  if (equally_weighted) {
    flog.info("Since equally_weighted=TRUE, all locations will be equally weighted.")
    location_outcomes[, W_i := 1 / length(unique(location))]
  } else {
    if ("W_i" %in% names(location_outcomes)) {
      flog.info("Since equally_weighted=FALSE, locations will be weighted by the provided W_i variable.")
    } else {
      stop(flog.info("Since equally_weighted=FALSE, W_i variable must be provided in location_outcomes."))
    }
  }

  # run checks on provided data
  check_info <- SSIV_checks(location_outcomes, industry_shocks, industry_location_shares, lagIV=lagIV)

  # merge industry shocks to shares
  industry_location_shares <- merge(industry_location_shares, industry_shocks, by = "industry")

  # build shift-share IV
  industry_location_shares[, S_i := sum(S_ij), list(location)]
  industry_location_shares[, Z_i := sum(S_ij * G_j), list(location)]
  if(lagIV){
    industry_location_shares[, Z_i_lag := sum(S_ij * G_j_lag), list(location)]
    if(lagsIV >= 2){
      industry_location_shares[, Z_i_lag2 := sum(S_ij * G_j_lag2), list(location)]
    }
    if(lagsIV >= 3){
      industry_location_shares[, Z_i_lag3 := sum(S_ij * G_j_lag3), list(location)]
    }
    if(lagsIV >= 4){
      industry_location_shares[, Z_i_lag4 := sum(S_ij * G_j_lag4), list(location)]
    }
  }

  # collect results
  results <- data.table()
  
  # Uncorrected location-level regression
  if ("Uncorrected" %in% methods) {
    location_data <- merge(location_outcomes, unique(industry_location_shares[, .(location, Z_i, S_i)]), by = "location")
    if(lagIV){
      location_data <- merge(location_outcomes, unique(industry_location_shares[, .(location, Z_i_lag)]), by = "location")
    }
    Uncorrected_results <- Uncorrected_location_regressions(location_data)
    results <- rbindlist(list(results, Uncorrected_results), use.names = T, fill = T)
  }

  # BHJ industry-level transform and regression
  if ("BHJ" %in% methods) {
    industry_location_data <- merge(industry_location_shares, location_outcomes, by = c("location"))
    industry_data <- BHJ_transform_to_industry(industry_location_data = industry_location_data, lagIV=lagIV, lagsIV=lagsIV, industryFE=industryFE, timeFE=timeFE)
    BHJ_results <- BHJ_industry_regressions(industry_data = industry_data, lagIV=lagIV, lagsIV=lagsIV, industryFE=industryFE, timeFE=timeFE)
    results <- rbindlist(list(results, BHJ_results), use.names = T, fill = T)
  }
  if ("Uncorrected" %in% methods & "BHJ" %in% methods) {
    BHJ_check_proposition1(results[method %in% c("Uncorrected", "BHJ")])
  }

  flog.info("Finished SSIV.")
  return(results)
}

