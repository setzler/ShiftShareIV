
rm(list=ls())
library(data.table)
library(ShiftShareIV)
library(lfe)
set.seed(101)

## build firm-specific log(wage) with industry, location, and firm FEs and shocks
CBP <- setDT(read.csv(file = "~/github/EconData/DataRepo/CensusCBP/CBP_cz_industry2.csv"))[year >= 2001 & year <= 2015]
CBP <- CBP[,list(industry=naics, location=cz, year, employment = employment_march, log_wage = log(payroll_quarter1/employment_march))][employment>0][, log_employment := log(employment)]
dd <- data.table(expand.grid(industry = unique(CBP$industry), location = unique(CBP$location), year = unique(CBP$year), firm_temp = 1:50))
CBP <- NULL
dd[, firm := .GRP, list(industry,location,firm_temp)]
dd[, firm_temp := NULL]
dd[, firm_TFP_bar := rnorm(1,sd=.1), firm]
dd[, industry_TFP_bar := rnorm(1,sd=.1), industry]
dd[, location_TFP_bar := rnorm(1,sd=.1), location]
dd[, firm_TFP_year := rnorm(1,sd=.1), list(firm,year)]
dd[, industry_TFP_year := rnorm(1,sd=.1), list(industry,year)]
dd[, location_TFP_year := rnorm(1,sd=.1), list(location,year)]
dd[, firm_TFP := firm_TFP_bar + firm_TFP_year]
dd[, industry_TFP := industry_TFP_bar + industry_TFP_year]
dd[, location_TFP := location_TFP_bar + location_TFP_year]
dd[, log_wage := firm_TFP + industry_TFP + location_TFP + 1 ]

## given log wage, construct the employment for a given labor supply elasticity
dd[, log_employment := 4.0*log_wage + 1 + location_TFP]
dd <- dd[,.(firm,industry,location,year,log_wage,log_employment)]
dd[, employment := exp(log_employment)]

## lagged values and changes
lag <- copy(dd)[, year := year+1]
setnames(lag,paste0(names(lag),'L'))
setnames(lag,c('firmL','locationL','industryL','yearL'),c('firm','location','industry','year'))
dd <- merge(dd,lag,by=c('firm','location','industry','year')) 
dd[, log_employmentD := log_employment-log_employmentL] # log change in employment
dd[, log_wageD := log_wage - log_wageL] # log change in wage

## main application
results <- SSIV(dd,  time_variable = 'year', equally_weighted = FALSE, build_data=TRUE, FE_covariates = 'firm',
            Y_var = "log_employmentD", X_var = "log_wageD", G_var = "log_employmentD", W_var = "employmentL", fixed_year = 2002)
print(results[])

## spillovers
spillovers <- SSIV(dd,  time_variable = 'year', equally_weighted = FALSE, build_data=TRUE, FE_covariates = 'firm',
                Y_var = "log_employmentD", X_var = "log_wageD", G_var = "log_employmentD", W_var = "employmentL", fixed_year = 2002,
                spillovers=T, outcome_industries = c(31,32,33))
print(spillovers[])


