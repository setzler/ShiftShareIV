
#' Check the industry_location_shares
#' 
#' @param industry_location_shares Industry location shares
#' 
#' @export
SSIV_check_shares <- function(industry_location_shares) {
  
  tolerance = 1e-3

  # check variables exist in industry_location_shares
  provided_names <- names(industry_location_shares)
  required_names <- c("location", "industry", "S_ij")
  missing_names <- c()
  for (the_name in required_names) {
    if (!(the_name %in% provided_names)) {
      missing_names <- c(missing_names, the_name)
    }
  }
  if (length(missing_names) > 0) {
    missing_names <- paste(missing_names, collapse = ",")
    stop(flog.info("Provided industry_location_shares is missing variable(s): %s", missing_names))
  }

  ## check for valid S_ij in industry_location_shares
  industry_location_maxN <- industry_location_shares[, .N, list(location, industry)][, max(N)]
  if (industry_location_maxN > 1) {
    stop(flog.info("At least one duplicate (location,industry) pair in industry_location_shares"))
  }
  if (industry_location_shares[, min(S_ij) < -tolerance]) {
    stop(flog.info("S_ij has values less than 0 in industry_location_shares"))
  }
  if (industry_location_shares[, max(S_ij) > 1 + tolerance]) {
    stop(flog.info("S_ij has values greater than 1 in industry_location_shares"))
  }
  S_i <- industry_location_shares[, list(S_i = sum(S_ij)), list(location)]
  if (S_i[, max(S_i - 1) > tolerance]) {
    stop(flog.info("The sum of S_ij is greater than 1.0 within at least one location"))
  }
  incomplete_shares <- S_i[, min(S_i - 1) < -tolerance]
  if (incomplete_shares) {
    flog.info("Incomplete shares detected: The sum of S_ij is less than 1 in at least one location.")
  }

  return(list(S_i = S_i, incomplete_shares = incomplete_shares))
}

#' Check that data provided meets the BHJ structure
#' 
#' @param location_outcomes Location outcomes
#' @param industry_shocks Industry shocks
#' @param industry_location_shares Industry location shares
#' 
#' @export
SSIV_checks <- function(location_outcomes, industry_shocks, industry_location_shares, 
                        lagIV = FALSE) {
  
  tolerance = 1e-3

  # check input types
  assertDataTable(location_outcomes)
  assertDataTable(industry_shocks)
  assertDataTable(industry_location_shares)

  ## check for duplicates
  if (location_outcomes[, list(N = .N), list(location)][, max(N)] > 1) {
    stop(flog.info("Duplicate location in location_outcomes"))
  }
  if (industry_location_shares[, list(N = .N), list(location, industry)][, max(N)] > 1) {
    stop(flog.info("Duplicate (industry,location) in industry_location_shares"))
  }
  if (industry_shocks[, list(N = .N), list(industry)][, max(N)] > 1) {
    stop(flog.info("Duplicate industry in industry_shocks"))
  }

  # check variables in location_outcomes
  provided_names <- names(location_outcomes)
  required_names <- c("location", "Y_i", "X_i", "W_i")
  missing_names <- c()
  for (the_name in required_names) {
    if (!(the_name %in% provided_names)) {
      missing_names <- c(missing_names, the_name)
    }
  }
  if (length(missing_names) > 0) {
    missing_names <- paste(missing_names, collapse = ",")
    stop(flog.info("Provided location_outcomes is missing variable(s): %s", missing_names))
  }

  # check variables in industry_shocks
  provided_names <- names(industry_shocks)
  required_names <- c("industry", "G_j")
  if(lagIV){
    required_names <- c(required_names, "G_j_lag")
  }
  missing_names <- c()
  for (the_name in required_names) {
    if (!(the_name %in% provided_names)) {
      missing_names <- c(missing_names, the_name)
    }
  }
  if (length(missing_names) > 0) {
    missing_names <- paste(missing_names, collapse = ",")
    stop(flog.info("Provided industry_shocks is missing variable(s): %s", missing_names))
  }

  ## check for valid weights W_i
  if (location_outcomes[, min(W_i) < -tolerance]) {
    stop(flog.info("W_i has values less than 0 in location_outcomes"))
  }
  if (location_outcomes[, min(W_i - 1) > -tolerance]) {
    stop(flog.info("W_i has values less than 1 in location_outcomes"))
  }
  if (max(abs(location_outcomes[, sum(W_i)] - 1)) > tolerance) {
    stop(flog.info("W_i does not sum to 1 across location_outcomes"))
  }

  ## check for valid S_ij in industry_location_shares
  share_checks <- SSIV_check_shares(industry_location_shares)
  S_i <- share_checks$S_i
  incomplete_shares <- share_checks$incomplete_shares

  ## check if all industries are in S_ij and G_j
  industries_with_G_j <- industry_shocks[, unique(industry)]
  industries_with_S_ij <- industry_location_shares[, unique(industry)]
  if (!all(industries_with_G_j %in% industries_with_S_ij)) {
    stop(flog.info("Some of the industries in industry_shocks are not in industry_location_shares"))
  }
  if (!all(industries_with_S_ij %in% industries_with_G_j)) {
    stop(flog.info("Some of the industries in industry_location_shares are not in industry_shocks"))
  }
}
