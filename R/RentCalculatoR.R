#' @title Changes in rents calculator for different situations (according to Swiss law)
#'
#' @description With the package RentCalculatoR you can calculate the change in rent of residential real estate according to Swiss law. Changes in value-adding investments, depreciation, interests and reference interest rates can be taken into account. As well as changings due to changes in the market such as inflation, mortgage and cost increments.
#'
#' @param act_rent_CHF actual rent in CHF
#' @param investment_CHF investment in CHF
#' @param increasing_share increasing share
#' @param lifespan lifespan
#' @param maintenance_rate maintenance rate in %
#' @param inflation_rate_last_date last date of inflation adjustment
#' @param reference_last_date last date of mortgage adjustment
#' @param cost_incr_last_date last date of cost increase adjustment
#' @param flat_rate flat rate
#'
#' @return act_data(c(act_mortgage, act_date, act_infl_rate))
#' @return return_invest (c(value_incr_share_CHF, depreciation_CHF,interest_CHF, maintenance_CHF, total_add_rent_monthly_CHF, total_new_rent_monthly_CHF)
#' @return act_rent_CHF, total_add_rent_monthly_CHF, total_new_rent_monthly_CHF
#' @return act_rent_CHF, total_add_rent_monthly_CHF, total_new_rent_monthly_CHF, mortgage_change
#' @return act_rent_CHF, total_add_rent_monthly_CHF, total_new_rent_monthly_CHF, cost_diff_rate*flat_rate
#' @return act_rent_CHF, act_data(c(act_mortgage, act_date, act_infl_rate), total_add_rent_monthly_CHF, total_new_rent_monthly_CHF, mortgage_change, cost_diff_rate*flat_rate)
#'
#' @examples act_results <- RentInformations()
#' @examples RentInvestCalculatoR(1000,100000,50,12)
#' @examples RentInvestCalculatoR(0.000001,500000,100,50,8)
#' @examples inf_rate_change(1000, "2019-06-01")
#' @examples mortgage_rate_change(1000, "2013-09-03")
#' @examples cost_incr_change(1000, "2019-06-01")
#' @examples cost_incr_change(1000, "2019-06-01", 1)
#' @examples RentCalculatorNewCircum(1000, "2016-10-01", "2013-09-03", "2019-06-01", 1)
#' @examples building <- RentInvestCalculatoR(0.001, 250000, 100, 100, 10)
#' @examples windows <- RentInvestCalculatoR(0.001, 40000, 100, 20, 8)
#' @examples kitchen <- RentInvestCalculatoR(0.001, 30000, 70, 15, 8)
#' @examples bathroom <- RentInvestCalculatoR(0.001, 50000, 70, 30, 8)
#' @examples floors <- RentInvestCalculatoR(0.001, 25000, 50, 12, 0)
#'
# @export RentCalculatoR
# @export RentInformations
# @export RentInvestCalculatoR
# @export inf_rate_change
# @export mortgage_rate_change
# @export cost_incr_change
# @export RentCalculatorNewCircum
# @export RentCalculatorNewCircum
#'


##################################################################################################
# PROGRAMMING PROJECT, PROGRAMMING IN FINANCE II, SUMMERSEMESTER 2020,
# WITH PROF. PETER H. GRUBER AT UNIVERSITA DELLA SVIZZERA ITALIANA
# authors: Ruth Peterhans ruth.maria.peterhans@usi.ch
#          Giuliano Giovanardi, giuliano.giovanardi@usi.ch
##################################################################################################


# To write this package we used the mentioned sources in the Bibliography at the end of this file.

##################################################################################################
# CHANGES IN RENTS CALCULATOR (FOR SWITZERLAND)
# The package RentCalculatoR calculates the change in rent of residential real estate according to
# Swiss law. Changes in value-adding investments, depreciation, interest and reference interest
# rates can be taken into account.
##################################################################################################

##################################################################################################
# CONTENT OF THE RentCalculatoR
##################################################################################################
# This package uses different functions to calculate changes in rents separately:
# - PART A.1: mortgage_rates(), returns a clean table with the dates and rates of mortgage
# - PART A.2: inflation_rates(), returns a clean table with the dates and rates of inflation (based 2015)
# - PART A.3: RentInformations(), returns a summary (act_mortgage, act_date, act_infl_rate)

# It also provides a summary for changes due to investments:
# - PART B: RentInvestCalculatoR(act_rent_CHF, investment_CHF, increasing_share, lifespan, maintenance_rate)
#           returns a summary

# And finally it provides a summary for changes due to market circumstances:
# - PART C.1: inf_rate_change(act_rent_CHF, inflation_rate_last_date), returns a summary
# - PART C.2: mortgage_rate_change(act_rent_CHF, reference_last_date, next_termination_date), returns a summary
# - PART C.3: cost_incr_change(act_rent_CHF, cost_incr_last_date, flat_rate = 0.5), returns a summary
# - PART C.4: RentCalculatorNewCircum(act_rent_CHF, inflation_rate_last_date, reference_last_date,
#                                     cost_incr_last_date, flat_rate = 0.5), returns a summary

# We provide Examples for each function and also for a combination of the different functions together
# at the end of the PARTS code.


##################################################################################################
# REQUIRED INFORMATION - short for users
##################################################################################################
# For the calculation we use 3 main functions. They need the following information (variables):

# 1) The following information (variables) are taken from the system or from scraping (Code in PART A):
#    - current month and year for the calculation on inflation and general cost increases
#    - current mortgage rent (mortgage, in %); from https://www.bwo.admin.ch/bwo/de/home/
#      mietrecht/referenzzinssatz/entwicklung-referenzzinssatz-und-durchschnittszinssatz.html
#    - information about the inflation from xlsx-file on https://www.bfs.admin.ch/bfsstatic/
#     dam/assets/12827290/master/

# 2) From the user about the changings due to investments (Code in PART B):
#    - actual net rent (act_rent_CHF)
#    - total investment (investment_CHF)
#    - value-increasing share in % (increasing_share)
#    - lifespan in years (lifespan)
#    - maintenance allowance (maintenance), 10 % by default due to a Supreme court ruling

# 3) For changing due to change in the market circumstances (Code in PART C) we need to know of the user:
#    - actual net rent (act_rent)
#    - month and year of the last rent adjustment on reference rate (reference_last_date)
#    - month and year of the last inflation adjustment (inflation_rate_last_date)
#    - month and year of th last cost increase adjustment (cost_incr_last_date)
#    - flat rate for general cost increase (flat_rate_cost), from 0 to 1 %

##################################################################################################
# THIS ARE THE PACKAGES THAT ARE NEED: You have to install them in case you don't have them
if ("xml2" %in% rownames(installed.packages()) == FALSE) {
  install.packages("xml2")
}
library(xml2)

if ("rvest" %in% rownames(installed.packages()) == FALSE) {
  install.packages("rvest")
}
library(rvest)

if ("readr" %in% rownames(installed.packages()) == FALSE) {
  install.packages("readr")
}
library(readr)

if ("readxl" %in% rownames(installed.packages()) == FALSE) {
  install.packages("readxl")
}
library(readxl)

if ("tidyr" %in% rownames(installed.packages()) == FALSE) {
  install.packages("tidyr")
}
library(tidyr)

if ("zoo" %in% rownames(installed.packages()) == FALSE) {
  install.packages("zoo")
}
library(zoo)


##################################################################################################
#
#  PART A: Getting the relevant data: mortgage_rates(), inflation_rates(), RentInformations(),
#                           returns: act_data(c(act_mortgage, act_date, act_infl_rate))
#
##################################################################################################

#_Explanation behind _____________________________________________________________________________
# We use three functions that get the relevant information from the system or from scraping of the
# webpage of the Department of Statistics or the Department of Housing.
# They are coded in PART A and are accessible with the function RentInformations(). They will be
# available as Data on the name act_results.
#    - current relevant and official mortgage rent (in %); from https://www.bwo.admin.ch/bwo/de/home/
#      mietrecht/referenzzinssatz/entwicklung-referenzzinssatz-und-durchschnittszinssatz.html
#    - current date for the calculation on inflation and on general cost increases
#    - official information about the inflation rates from xlsx-file on https://www.bfs.admin.ch/bfsstatic/
#     dam/assets/12827290/master/
# We will use this information in Part B and C.

##################################################################################################


#
#  PART A.1: mortgage_rates(), returns a clean table with the dates and rates of mortgage
##################################################################################################

#############################
# MORTGAGE RATES TABLE - getting the mortgage data from the Federal Office for Housing
#############################
# Reference: https://www.bwo.admin.ch/bwo/de/home/mietrecht/referenzzinssatz/entwicklung-referenzzinssatz-und-durchschnittszinssatz.html

#' @export mortgage_rates

mortgage_rates <- function(){

  # get the mortgage rates from the website of the statistics of the government
  mortgage_url<- "https://www.bwo.admin.ch/bwo/de/home/mietrecht/referenzzinssatz/entwicklung-referenzzinssatz-und-durchschnittszinssatz.html"
  mortgage_urlHtml <- mortgage_url %>% read_html()
  mortgage_data <- mortgage_urlHtml %>% html_table(header=TRUE) %>% as.data.frame

  # clean up the data: set column names, define data types
  mortgage_data_clean <- data.frame(mortgage_data[,(1:2)])
  colnames(mortgage_data_clean) <- c("mortgage_rate", "valuable_from")
  mortgage_data_clean$mortgage_rate <- parse_number(mortgage_data_clean$mortgage_rate, locale = locale(decimal_mark=","))
  mortgage_data_clean$valuable_from <- strptime(mortgage_data_clean$valuable_from, format="%d.%m.%Y")
  mortgage_data_clean$valuable_from <- as.Date(mortgage_data_clean$valuable_from)

  # clean the line with additional Information, because the different way in measuring is not relevant to the calculations
  mortgage_data_clean <- mortgage_data_clean[complete.cases(mortgage_data_clean),]

  # return the data as dataframe
  return(mortgage_data_clean)
}

##################################################################################################


#
#  PART A.2: inflation_rates(), returns a clean table with the dates and rates of inflation (based 2015)
#
##################################################################################################

#############################
# INFLATION RATE TABLE - getting the inflation rate tables from the Department of Statistics
#############################
# Inflation index from as xlsx download of the department for statistics
# https://www.bfs.admin.ch/bfsstatic/dam/assets/12827290/master

#' @export inflation_rates

inflation_rates <- function(){

  # To ignore the warnings during usage
  options(warn=-1)
  options("getSymbols.warning4.0"=FALSE)

  URL_inflation <- "https://www.bfs.admin.ch/bfsstatic/dam/assets/13047088/master"
  download.file(URL_inflation, destfile = "inflation_rates.xlsx")
  inflation_base2015 <- read_excel("inflation_rates.xlsx", sheet = "2015",
                                   col_names =  c("year", (1:17)))

  # clean the spreadsheet of 2015 indices
  inflation_clean <- data.frame(inflation_base2015[,(1:13)])
  inflation_clean$year <- as.Date.character(inflation_clean$year, format = "%Y")
  inflation_clean$year <- as.numeric(format(inflation_clean$year, "%Y"))

  # take only cases where year has a value and sort from actual to past years
  years_only <- complete.cases(inflation_clean$year)
  inflation_clean <- cbind(years_only, inflation_clean)
  inflation_clean <- subset(inflation_clean, years_only == TRUE)
  inflation_clean <- inflation_clean [,(2:14)]
  inflation_clean <- inflation_clean[order(inflation_clean$year, decreasing = TRUE),]

  # return the data as dataframe
  return(inflation_clean)
}

##################################################################################################


#
#  PART A.3: RentInformations(), returns (act_mortgage, act_date, act_infl_rate)
#
##################################################################################################
# The RentInformations function gives you the actual data which is needed to calculate some changes
# in rents in PART B and C

#' @export RentInformations

RentInformations <- function(){

  # MORTGAGE - getting the actual mortgage (act_mortgage) from PART A.1
  #############################

  # filter out the actual mortgage rate (i.e. the last published)
  act_mortgage <- mortgage_rates()
  act_mortgage <- act_mortgage$mortgage_rate[1]

  # ACTUAL DATE - getting the actual date (act_date) out of the system
  #############################
  act_date <- Sys.Date()

  # INFLATION RATE - getting the actual inflation rate (act_inflation_rate) from PART A.2
  #############################

  # filter out the actual inflation rate (i.e. the last published)
  inflation_rates_data <- inflation_rates()
  act_inflation_rate <- inflation_rates_data[1,]
  act_inflation_rate <- gather(act_inflation_rate)
  act_inflation_rate <- subset(act_inflation_rate, !is.na(act_inflation_rate$value))
  act_inflation_rate <- as.character(act_inflation_rate$value)
  act_inflation_rate <- as.numeric(dplyr::last(act_inflation_rate))


  #############################
  # PRINT RESULTS
  #############################
  # overview of the results and report
  actual_data <- c(act_mortgage, as.character(act_date), act_inflation_rate)
  actual_answers <- c("The actual mortgage rate in % is:", "The actual date is:",
                      "And the actual inflation rate is (points on 2015 basis)")
  actual_print_result <- as.table(cbind(actual_answers, actual_data))
  print(actual_print_result)

  # return the actual data in a dataframe
  act_data <- data.frame(actual_data)
  act_data <- cbind(c("act_mortgage", "act_date", "act_inflation_rate"), act_data)
  colnames(act_data) <- c("name", "value")
  row1 <- as.vector(act_data$name)
  row2 <- as.vector(act_data$value)
  act_data <- rbind(row2)
  colnames(act_data) <- row1
  return(act_data)
}


# Example to PART A
# -------------------
# If you would like to know what the actual relevant data for rents is.
# This output will be used from the other functions in Part B and C as well.
act_results <- RentInformations()




###########################################################################################################################
#
#  PART B: RentInvestCalculatoR(act_rent_CHF, investment_CHF, increasing_share, lifespan, maintenance_rate)
#           returns return_invest (c(value_incr_share_CHF, depreciation_CHF,interest_CHF, maintenance_CHF = 10,
#                                    total_add_rent_monthly_CHF, total_new_rent_monthly_CHF)
#
##########################################################################################################################

#_Short for users________________________________________________________________________________
# To calculate the changings due to investments we need from the user (PART B of the package):
#    - actual net rent (act_rent_CHF)
#    - total investment (investment_CHF)
#    - value-increasing share in % (increasing_share)
#    - lifespan in years (lifespan)
#    - maintenance allowance in % (maintenance_rate), set to 10 % by default due to a Supreme Court ruling.

# And we use the actual mortgage (act_mortgage) from the RentInformations function (see in PART A.3)

#_Explanation behind _____________________________________________________________________________
# Rents are based on different market circumstances (see PART C). If you are investing into an
# apartment additional value-increasing components, you are allowed to increase the rent (art. 14 VMWG).
# Therefore you need to take account the investment in CHF, and how much it's value-adding. Most of the
# time this is a rate between 50 and 70 %. Only if you invest in something completely new (i.e. that wasn't
# there in a former version) or if it is something which supports ecology of the building you can take 100 %.
# Because lifespan of elements differ it has to be taken into account as well. On the webpage
# https://www.mietrecht.ch/index.php?id=32 you can look up different lifespans that are commonly accepted.
# The maintenance rate differs on the component. Some need no maintenance during the warranty and other never
# (for example a dishwasher or a carpet). Therefore you can choose a maintenance rate between 0 and 10 % by
# a Suppreme court ruling.

#' @export RentInvestCalculatoR

RentInvestCalculatoR <- function(act_rent_CHF, investment_CHF, increasing_share,
                                 lifespan, maintenance_rate=10){

  # calling the results of the RentInformations function (PART A) and take the actual mortgage
  act_data <- as.data.frame(RentInformations( ))
  act_mortgage <- as.numeric(act_data$act_mortgage)

  # the increasing share and the depreciation of the investment in CHF
  value_incr_share_CHF <- investment_CHF/100*increasing_share
  depreciation_CHF <- value_incr_share_CHF/lifespan

  # the allowed interest by law is 0.5 % above the actual mortgage rate (dived to 2 paries)
  allowed_interest <- (act_mortgage+.5)/2
  # this leads to a change due to interests
  interest_CHF <- value_incr_share_CHF*allowed_interest/100

  # the intermediate result for the change is the sum of the depreciation and the interests
  int_result <- depreciation_CHF+interest_CHF

  # the allowed maintenance increase is 10 % of the intermediate result
  maintenance_CHF <- int_result*maintenance_rate/100

  #############################
  # PRINT RESULTS
  #############################

  # total rent results and report
  total_add_rent_monthly_CHF <- (int_result + maintenance_CHF)/12
  total_new_rent_monthly_CHF <- (12*act_rent_CHF + int_result + maintenance_CHF)/12
  rent_answers <- c("Your actual rent per month in CHF is:", "The additional rent per month in CHF is:",
                    "And the new total rent per month in CHF is:")
  rent_summary <- c(act_rent_CHF, total_add_rent_monthly_CHF, total_new_rent_monthly_CHF)
  rent_summary <- round(rent_summary/5, digits = 2)*5   # round to 5 cts.
  rent_result <- as.table(cbind(rent_answers, rent_summary))
  print("*******************************************************************************")

  # return the results in a dataframe
  return_invest <- as.table(c(value_incr_share_CHF, depreciation_CHF,interest_CHF, maintenance_CHF,
                              total_add_rent_monthly_CHF, total_new_rent_monthly_CHF))
  return_invest <- round(return_invest/5, digits = 2)*5   # round to 5 cts.
  return_invest <- as.data.frame(return_invest)
  colnames (return_invest) <- c("name", "value")
  row3 <- as.vector(return_invest$name)
  CHF <- as.vector(return_invest$value)
  return_invest <- rbind(CHF)
  colnames(return_invest) <- c("value_incr_share_CHF", "depreciation_CHF","interest_CHF", "maintenance_CHF",
                               "total_add_rent_monthly_CHF", "total_new_rent_monthly_CHF")
  print(return_invest)
}



# Examples to PART B
# ----------------------------------------------------------------------------------------------------------

# EXAMPLE for standard use
# ----------------------------------------------------------------------------------------------------------
# You put in the needed data of your actual rent and of the investment into the calculator.
# If you don't give a value for the maintenance rate, the calculator uses 10 % by default

# To test it for a actual rent of 1000 CHF, an investment of 100000 CHF,
# a value increasing share of 50 % and a lifespan of 12 years:
RentInvestCalculatoR(1000,100000,50,12)


# EXAMPLE with a quasi zero value for actual_rent (e.g. for calculating initial rents of new buildings)
# ----------------------------------------------------------------------------------------------------------
# The starting values of the actual rent basically can't be zero. If you would like to calculate the initial
# rent for a new building, you have to set the actual rent to a very small value. Of course you can also use
# it, if you buy an apartment and you want to know how much rent you should get out of it. The calculator
# gives you here an approximation, because you have to set an average lifespan over all components of the
# building (see also EXAMPLE with different components for more precise calculations).

# To test it for a new building let's say we would like to calculate the rent for a unit (apartment).
# The unit is valid an investment of 500000 CHF,  the value increasing share is 100 % because there was no
# value before, the average lifespan will be 50 years and the maintenance is set to 8 % because in the
# in the first years you still have guarantee and would not spend as much on maintenance.
RentInvestCalculatoR(0.000001,500000,100,50,8)


# EXAMPLE with different components
# ----------------------------------------------------------------------------------------------------------
# If you would like to do the calculation for different components, you can use the calculator of course
# different times and add the values up. You can look up different lifespans of components on
# https://www.mietrecht.ch/index.php?id=32 (unfortunately only available in German language).

# Let's say you have after a total renovation (including energetic and ecological) for example
# following investments with different lifespans an maintenance efforts and you would like to know
# the sum of over all changes due to this investments:

# investment_name  |   act_rent_CHF | investment_CHF  | increasing_share | lifespan    |  maintenance_rate
#  structure of building      0.001        250000           100 %            100 years     10 % by default
#  windows                    0.001         40000           100 %             20 years      8 %
#  kitchen                    0.001         30000            70 %             15 years      8 %
#  bathroom                   0.001         50000            70 %             30 years      8 %
#  floors                     0.001         25000            50 %             12 years      8 %
#   Total                     0.001       sum(????)        sum(????)         average(????)  average (????)

building <- RentInvestCalculatoR(0.001, 250000, 100, 100, 10)
windows <- RentInvestCalculatoR(0.001, 40000, 100, 20, 8)
kitchen <- RentInvestCalculatoR(0.001, 30000, 70, 15, 8)
bathroom <- RentInvestCalculatoR(0.001, 50000, 70, 30, 8)
floors <- RentInvestCalculatoR(0.001, 25000, 50, 12, 0)

# And then to sum up all changes and give an answer
total_invest <- as.data.frame(rbind(building,windows, kitchen, bathroom, floors))
cat("The Total change in rent per month due to all investments is", sum(total_invest$total_add_rent_monthly_CHF))



##################################################################################################
#
#  PART C: CHANGING IN RENTS DUE TO NEW CIRCUMSTANCES IN THE MARKET
#
##################################################################################################
# Rents can change due to different new circumstances in the market. Therefore we are using a
# function for each: Change in inflation rate (C.1), change in mortgage (C.2) and cost increase (C.3).
# All different changings together are calculated with the RentCalculatorNewCircum like in (C.4).

##################################################################################################
#
#  PART C.1: CHANGE IN INFLATION RATE: inf_rate_change()
#             uses:     inf_rate_change(act_rent_CHF, inflation_rate_last_date)
#             returns: act_rent_CHF, total_add_rent_monthly_CHF, total_new_rent_monthly_CHF
#
##################################################################################################

#_Short for users________________________________________________________________________________
# To calculate the changings due to inflation we need from the user:
#    - actual net rent (act_rent_CHF)
#    - the date, when the inflation rate was the last time adjusted (inflation_rate_last_date)

#_Explanation behind _____________________________________________________________________________
# The law allows you to calculate 40 % of the inflation rate into the
# rent price (article 16 VMWG, Tenancy Law Regulation). The inflation rate
# includes a certain concept of the national consumer price index (LIK).
# It measures the price development of goods and services that are important
# for private households. It indicates the extent to which consumers have to
# increase or decrease spending when prices change in order to keep consumption
# volume constant (https://www.bfs.admin.ch/bfs/it/home/statistiche/prezzi/rilevazioni/lik/ipc.html).
# The Tenancy law tells to use this index, which also include rents, to
# calculate the changes in rents.
# Therefore we use the inflation index tables from the Department of Statistics
# (xlsx download, https://www.bfs.admin.ch/bfsstatic/dam/assets/12827290/master)
# like we already described in PART A. The RentInformations function
# of Part A already gives us the actual inflation rate.
# Therefore we need to know of the user the last data of the inflation
# rate adjustment (inflation_rate_last_date).

#' @export inf_rate_change

inf_rate_change <- function(act_rent_CHF, inflation_rate_last_date) {

  # calling the results of the RentInformations function (PART A.3) and take the actual inflation rate
  act_data <- as.data.frame(RentInformations( ))
  act_inflation <- as.numeric(act_data$act_inflation)

  # calling the results of the inflation_rates function (PART A.2) and take the last inflation rate
  infl_data <- as.data.frame(inflation_rates())
  reference_last_date <- as.Date(inflation_rate_last_date)
  reference_last_year <- as.numeric(format(reference_last_date, "%Y"))
  reference_last_month <- as.numeric(format(reference_last_date, "%m"))

  inflation_rate_last <- subset(infl_data, infl_data$year == reference_last_year)
  colnames(inflation_rate_last) <- c("year",c(1:12))
  last_inflation <- inflation_rate_last[,reference_last_month+1]

  # it's allowed to calculate 40 % of the difference into the rent
  inflation_difference <- act_inflation - last_inflation
  allowed_inflation_rate <- 0.4*inflation_difference/100


  # PRINT RESULTS
  #############################

  # total rent results and report
  total_add_rent_monthly_CHF <- act_rent_CHF*allowed_inflation_rate
  total_new_rent_monthly_CHF <- (12*act_rent_CHF + total_add_rent_monthly_CHF)/12
  rent_answers <- c("Your actual rent per month in CHF is:", "The additional rent per month in CHF is:",
                    "And the new total rent per month in CHF is:")
  rent_summary <- c(act_rent_CHF, total_add_rent_monthly_CHF, total_new_rent_monthly_CHF)
  rent_summary <- round(rent_summary/5, digits = 2)*5   # round to 5 cts.
  rent_result <- as.table(cbind(rent_answers, rent_summary))
  list(rent_result, c("due to 40 % of the change in inflation (in %):", allowed_inflation_rate))

}



# Examples to PART C.1 Inflation rates
# ----------------------------------------------------------------------------------------------------------
# If you like to know the change in your rent due to inflation. You need to add the actual
# rent in CHF (act_rent_CHF) and the date of the last adjustment due to inflation as date
# in the format "Years-month-day" (inflation_rate_last_date).

# The function returns you a summary of your actual rent in CHF (act_rent_CHF), the added
# monthly rent in CHF due to inflation (total_add_rent_monthly_CHF) and the total new
# monthly rent in CHF (total_new_rent_monthly_CHF). It gives you also the allowed inflation
# rate due to the 40 % allowed change (allowed_inflation_rate).

# To know what your actual rent of CHF 1000 changes, when last updated on 1.6.2019 you type:
inf_rate_change(1000, "2019-06-01")




##################################################################################################
#
#  PART C.2: CHANGE IN MORTGAGE  (C.2): mortgage_rate_change()
#             uses:     act_rent_CHF, reference_last_date
#             returns: act_rent_CHF, total_add_rent_monthly_CHF, total_new_rent_monthly_CHF, mortgage_change
#
##################################################################################################

#_Short for users________________________________________________________________________________
# To calculate the changings due to inflation rate we need from the user:
#    - actual net rent (act_rent_CHF)
#    - the date, when the mortgage rate was adjusted the last time (reference_last_date)


#_Explanation behind _____________________________________________________________________________
# The law allows you to calculate the amount of rent new, if there is a change in the mortgage
# rent (article 12a and 13 VMWG, Tenancy Law Regulation). Therefore you have to know the
# mortgage rates, which are calculated and published by the Department of Housing
# (https://www.bwo.admin.ch/bwo/de/home/mietrecht/referenzzinssatz/entwicklung-referenzzinssatz-und-durchschnittszinssatz.html).

# To calculate the change you have to know the actual mortgage, which we are elaborating in A.1.
# Then you need to know the last date, when the mortgage rate was updated on your rent (reference_last_date).

# Because the changing have different levels by art. 13 VMWG and also by changing to the positive
# or negative, the code is a bit more complex. We explain more of the details inside the commented code.


#' @export mortgage_rate_change

mortgage_rate_change <- function(act_rent_CHF, reference_last_date){

  # calling the results of the RentInformations function (PART A.3) and take the actual mortgage rate
  act_data <- as.data.frame(RentInformations( ))
  act_mortgage <- as.numeric(act_data$act_mortgage)

  # calling the results of the mortgage_rage function (PART A.1) and take the one of the last adjustment date
  mortgage_data <- as.data.frame(mortgage_rates())
  last_mortgage <- subset(mortgage_data, mortgage_data$valuable_from <= reference_last_date)
  last_mortgage <- last_mortgage[1,1]

  #_Explanation behind _____________________________________________________________________________
  ##################################################################################################
  # For the change in mortgage reference rate we have to differ between positive and negative changings
  ##################################################################################################

  # The calculation works like described in art. 13 VMWG. This means the changing per 1/4 % in mortgage rate
  # leads to a change in rent between 2 - 3 %.
  # - If the mortgage rate is above 6 %:           2 % per 1/4 % change
  # - If the mortgage rate is between 5 and 6 %: 2.5 % per 1/4 % change
  # - If the mortgage rate is below 5 %:            3 % per 1/4 % change

  # The calculation works like described in art. 13 VMWG, but vice versa for a negative change.
  # This means the changing per 1/4 % in mortgage rate leads to a change in rent between 1.96 - 2.91 %.
  # - If the mortgage rate is above 6 %:           1.96 % per 1/4 % change   (out of 2/102) = 1.96)
  # - If the mortgage rate is between 5 and 6 %:   2.44 % per 1/4 % change (out of 2.5/102.5 = 2.44)
  # - If the mortgage rate is below 5 %:            2.91 % per 1/4 % change (out of 3/103 = 2.91)


  # C.2a mortgage help factors
  ##################################################################################################

  # Difference in mortgage per 1/4 %
  mortgage_change_factor <- abs((act_mortgage - last_mortgage)/.25)

  if (act_mortgage < 5) {
    mortgage_rate_factor <- 3*mortgage_change_factor
  } else if (act_mortgage > 6) {
    mortgage_rate_factor <- 2*mortgage_change_factor
  } else {
    mortgage_rate_factor <- 2.5*mortgage_change_factor
  }

  # The mortgage_factor (change per 1/4 %) is positive if the actual mortgage is higher than the last

  if (act_mortgage > last_mortgage) {
    # Difference in mortgage (positive)
    mortgage_change_rate_factor <- (mortgage_rate_factor+100)/100
    mortgage_change_CHF_monthly <- mortgage_change_rate_factor * act_rent_CHF - act_rent_CHF

    # The mortgage_factor (change per 1/4 %) is negative if the actual mortgage is smaller than the last
  } else if (act_mortgage < last_mortgage) {
    # Difference in mortgage (negative)
    mortgage_change_rate_factor <- 1 - (mortgage_rate_factor / (100+mortgage_rate_factor))
    mortgage_change_CHF_monthly <- mortgage_change_rate_factor * act_rent_CHF - act_rent_CHF

    # If there is no mortgage_factor (no change per 1/4 %)
  } else if (act_mortgage == last_mortgage) {
    mortgage_change_CHF_monthly <- 0

    # And if the last_reference is not given
  } else {
    print("There is no valid change in mortgage due to a missing last reference date.
            Try for example with the last adjustment date of inflation.")
  }

  mortgage_change <- act_mortgage - last_mortgage

  # PRINT RESULTS
  #############################

  # total rent results and report
  total_add_rent_monthly_CHF <- mortgage_change_CHF_monthly
  total_new_rent_monthly_CHF <- act_rent_CHF + total_add_rent_monthly_CHF
  rent_answers <- c("Your actual rent per month in CHF is:", "The change in rent per month in CHF is:",
                    "And the new total rent per month in CHF is:")
  rent_summary <- c(act_rent_CHF, total_add_rent_monthly_CHF, total_new_rent_monthly_CHF)
  rent_summary <- round(rent_summary/5, digits = 2)*5   # round to 5 cts.
  rent_result <- as.table(cbind(rent_answers, rent_summary))
  list(rent_result, c("due to a procentual change in mortgage (in %):", mortgage_change))


}


# Examples to PART C.2 Mortgage change
# ----------------------------------------------------------------------------------------------------------
# If you like to know the change in your rent due to a change in mortgage. You need to add
# the actual rent in CHF (act_rent_CHF) and the date of the last adjustment due to mortgage
# as date in the format "Years-month-day" (inflation_rate_last_date).

# The function returns you a summary of your actual rent in CHF (act_rent_CHF), the added
# monthly rent in CHF due to mortgage change (total_add_rent_monthly_CHF) and the total new
# monthly rent in CHF (total_new_rent_monthly_CHF). It gives you also the percentage of the
# change in mortgage in % (mortgage_change).

# To know what your actual rent of CHF 1000 changes, when last updated on 3.9.2013 you type:
mortgage_rate_change(1000, "2013-09-03")



##################################################################################################
#
#  PART C.3: CHANGE DUE TO COST INCREASE (C.3): cost_incr_change()
#             uses:    act_rent_CHF, cost_incr_last_date, flat_rate = 0.5
#             returns: act_rent_CHF, total_add_rent_monthly_CHF, total_new_rent_monthly_CHF, cost_diff_rate*flat_rate
#
##################################################################################################

#_Short for users________________________________________________________________________________
# To calculate the changings due to cost increase the function needs from the user:
#    - actual net rent (act_rent_CHF)
#    - the date, when the cost increasing was adjusted the last time (cost_incr_last_date)
#    - the used flat rate between 0 and 1 % (flat_rate) by default it is 0.5 %

#_Explanation behind _____________________________________________________________________________
# The good practice allows you to add the changes due to cost increase up to 1 % per year.
# The calculator needs the actual rent in CHF (act_rent_CHF) and the date of the last adjustment
# of the rent due to cost increase (cost_incr_last_date). The function uses whole months to
# to calculate the difference. So if you don't know exactly the day of the last adjustment,
# it doesn't matter. Additionally you can also declare which rate between 0 and 1 % you like
# to use (flat_rate). By default its set to 0.5 %.

#' @export cost_incr_change

cost_incr_change <- function(act_rent_CHF, cost_incr_last_date, flat_rate = 0.5){

  # calling the results of the RentInformations function (PART A.3) and take the actual date
  act_data <- as.data.frame(RentInformations( ))
  act_date <- act_data$act_date

  # difference between the actual date and the cost_incr_last_date
  cost_diff_rate <- (as.yearmon(strptime(act_date, format = "%Y-%m-%d"))-
                       as.yearmon(strptime(cost_incr_last_date, format = "%Y-%m-%d")))

  # with the flat_rate this leads to a change in rent of
  cost_difference_CHF <- act_rent_CHF * cost_diff_rate /100 *flat_rate


  # PRINT RESULTS
  #############################

  # total rent results and report
  total_add_rent_monthly_CHF <- cost_difference_CHF
  total_new_rent_monthly_CHF <- act_rent_CHF + total_add_rent_monthly_CHF
  rent_answers <- c("Your actual rent per month in CHF is:", "The additional rent per month in CHF is:",
                    "And the new total rent per month in CHF is:")
  rent_summary <- c(act_rent_CHF, total_add_rent_monthly_CHF, total_new_rent_monthly_CHF)
  rent_summary <- round(rent_summary/5, digits = 2)*5   # round to 5 cts.
  rent_result <- as.table(cbind(rent_answers, rent_summary))
  list(rent_result, c("due to a procentual change in costs (in %):", cost_diff_rate*flat_rate))

}



# Examples to PART C.3 Cost increase
# ----------------------------------------------------------------------------------------------------------
# If the last adjustment of the rent was in june 2019, the actual rent in CHF is 1000 and you let the flat rate
# at the default level of 0.5 %, you use:
cost_incr_change(1000, "2019-06-01")
# If you like to have the same but with a flat rate of 1 % you use:
cost_incr_change(1000, "2019-06-01", 1)




##################################################################################################
#
#  PART C.4: CHANGE DUE TO NEW MARKET CIRCUMSTANCES: RentCalculatorNewCircum ()
#             uses:      act_rent_CHF, inflation_rate_last_date, reference_last_date,
#                        cost_incr_last_date, flat_rate = 0.5
#             returns:   act_rent_CHF, act_data(c(act_mortgage, act_date, act_infl_rate)
#                        total_add_rent_monthly_CHF, total_new_rent_monthly_CHF,
#                        mortgage_change, cost_diff_rate*flat_rate
##################################################################################################

#_Short for users________________________________________________________________________________
# To calculate the changings due to different changings on th market the function needs from the user:
#    - actual net rent (act_rent_CHF)
#    - the date, when the inflation rate was adjusted the last time (inflation_rate_last_date)
#    - the date, when the mortgage was adjusted the last time (reference_last_date)
#    - the date, when the cost increasing was adjusted the last time (cost_incr_last_date)
#    - the used flat rate between 0 and 1 % (flat_rate) by default it is 0.5 %

#_Explanation behind _____________________________________________________________________________
# This function combines all the elements that affect the change in rent due to changings in the
# market circumstances like inflation rates (C.1), mortgage rates (C.2) or cost increasing (C.3).
# Because they are ale relatively expressed to the acutal rent and to the market changes, they
# can be combined into one. This will lead to actualization in the rent regarding to all market
# circumstances.

#' @export RentCalculatorNewCircum

RentCalculatorNewCircum  <- function(act_rent_CHF, inflation_rate_last_date, reference_last_date,
                                     cost_incr_last_date, flat_rate = 0.5) {

  # call all functions and save them to separate variables
  inflation <- as.data.frame(inf_rate_change(act_rent_CHF, inflation_rate_last_date))
  mortgage <- as.data.frame(mortgage_rate_change(act_rent_CHF, reference_last_date))
  costs <- as.data.frame(cost_incr_change(act_rent_CHF, cost_incr_last_date, flat_rate))
  inflation_CHF <- as.numeric(inflation[5,3])
  mortgage_CHF <- as.numeric(mortgage[5,3])
  costs_CHF <- as.numeric(costs[5,3])
  total_new <- act_rent_CHF + inflation_CHF + mortgage_CHF + costs_CHF
  total_change <- inflation_CHF + mortgage_CHF + costs_CHF


  # PRINT RESULTS
  #############################

  #total rent results and report
  answer <- c("Your actual rent per month in CHF is:",
              "The change in rent per month due to inflation in CHF is:",
              "The change in rent per month due to mortgage in CHF is:",
              "The change in rent per month due to cost increase in CHF is:",
              "And the new total rent per month in CHF is:",
              "This is a change of total:")

  summary <- c(act_rent_CHF, inflation_CHF, mortgage_CHF, costs_CHF,
               total_new, total_change)

  rent_result <- as.data.frame(cbind(answer, summary))
  list(rent_result)

}


# Examples to PART C.4 Change in market circumstances
# ----------------------------------------------------------------------------------------------------------
# If you would like to calculate all changings due to changings in market circumstances
# together you can use the RentCalculatorNewCircum function. This combines all PART C.1
# - C.3 in one. It gives you an output and a dataset of the single changings, the total
# new rent and the total changings.

# For example if your actual rent is 1000 CHF and the last adjustments to inflation was on the
# 01.10.2016, the one to mortgage on the 3.9.2013 and to the costs on 1.6.2019 you use
# (supposing the flat rate for the cost increase is 1 %):
RentCalculatorNewCircum(1000, "2016-10-01", "2013-09-03", "2019-06-01", 1)



##################################################################################################
# EXAMPLES for use OF PART A to C together
# ----------------------------------------------------------------------------------------------------------
# If you have an apartment that is totally renovated and you have to calculate the new
# rent due to the investments and also due to changings in the market you can use
# the RentInvestCalculatoR together with the RentCalculatorNewCircum very easy.
# You take the two as data frames functions and add them to a new variable.

# Let's say you invested 100000 CHF with a value increasing share of 50 % and a lifespan of 12 years,
# while the rent before the investment was 1000 CHF.
invest <- as.data.frame(RentInvestCalculatoR(1000,100000,50,12))

# Let's say the last adjustments to inflation was on the 01.10.2016, the one to mortgage on the 3.9.2013
# and to the costs on 1.6.2019 you use (supposing the flat rate for the cost increase is 1 %):
market <- as.data.frame(RentCalculatorNewCircum(1000, "2016-10-01", "2013-09-03", "2019-06-01", 1))

# and then add the data with new added monthly value of the investment into the subset of market and
# add up the the sum of market changes (row 1 to 4) and the monthly added rent due to investment
# together to a new line which gives you the TOTAL result:
OverAllNewRent <- rbind(market[(1:4),],
                        c("The change in rent per month due to investment is:", invest$total_add_rent_monthly_CHF),
                        c("And the new TOTAL rent per month in CHF is:", sum(as.numeric(market[(1:4),2]))
                          +invest$total_add_rent_monthly_CHF)
)


##############################################################################################################
# BIBLIOGRAPHY
##############################################################################################################

# Topic related informations about rents in Switzerland
# https://www.mietrecht.ch

# General R
# https://stat.ethz.ch/R-manual/
# http://www.wolferonline.de/xmlR/XMLinR.pdf
# https://de.wikibooks.org/wiki/GNU_R
# https://stackoverflow.com/

# Creating a package
# https://www.analyticsvidhya.com/blog/2017/03/create-packages-r-cran-github/
# http://portal.stats.ox.ac.uk/userdata/ruth/APTS2012/Rcourse10.pdf
# https://cran.r-project.org/doc/contrib/Leisch-CreatingPackages.pdf
# https://support.rstudio.com/hc/en-us/articles/200486508-Building-Testing-and-Distributing-Packages
# https://support.rstudio.com/hc/en-us/articles/200486518

# Writing documentation
# https://support.rstudio.com/hc/en-us/articles/200532317-Writing-Package-Documentation

# Debugging
# https://stat.ethz.ch/pipermail/r-help/2014-May/374864.html
# https://stackoverflow.com/questions/26697727/what-does-error-in-namespaceexportns-exports-undefined-exports-mean

# Publishing on a GitHub-Account
# https://www.analyticsvidhya.com/blog/2017/03/create-packages-r-cran-github/




######################################################################
# AUTHOR'S DECLARATION
######################################################################

# We hereby certify that:
# - We have written the program ourselves except for clearly marked pieces of code
# - We have tested the program and it ran without crashing
# Giuliano Giovanardi and Ruth Peterhans

