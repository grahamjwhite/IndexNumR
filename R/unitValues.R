#' Aggregates prices to unit values and quantities to sums
#'
#' A function to aggregate price and quantity data to unit values
#'
#' @param x A dataframe containing price, quantity, a time period identifier
#' and a product identifier. It must have column names.
#' @param pvar A character string for the name of the price variable
#' @param qvar A character string for the name of the quantity variable
#' @param prodID A character string for the name of the product identifier
#' @param pervar character string for the name of the time variable. This variable
#' must contain integers starting at period 1 and increasing in increments of 1 period.
#' There may be observations on multiple products for each time period.
#' @return A dataframe containing columns for product identifier, time period,
#' quantities, and unit values.
#' @examples
#' # suppose the CES_sigma_2 dataset contains 12 monthly observations
#' # and suppose we want quarterly unit values.
#' df <- CES_sigma_2
#' # convert the monthly time variable into quarterly
#' df$time <- ceiling(CES_sigma_2$time/3)
#' # compute unit values using the quarterly time variable
#' unitValues(df,pvar="prices",qvar="quantities",pervar="time",prodID="prodID")
#' @export
unitValues <- function(x,pvar,qvar,pervar,prodID){

  # check valid column names are given
  colNameCheck <- checkNames(x, c(pvar, qvar, pervar, prodID))
  if(colNameCheck$result == FALSE){
    stop(colNameCheck$message)
  }

  # number of periods
  n <- max(x[[pervar]],na.rm = TRUE)

  # loop over all periods ...
  means_it <- lapply(1:n,function(i){
    # subset period i
    xt <- x[x[[pervar]]==i,]
    # loop over all products
    means_i <- lapply(unique(xt[[prodID]]),function(id){
      # subset the period 'i', product 'id' data
      xti <- xt[xt[[prodID]]==id,]
      # calculate expenditure for this item
      exp <- sum(xti[[pvar]]*xti[[qvar]])
      # calculate total quantity
      qit <- sum(xti[[qvar]])
      # calculate unit value
      unitValue <- exp/qit
      # create a vector containing the id, time, total quantity and unit value
      result <- cbind(id,i,qit,unitValue)
      colnames(result) <- c(prodID,"period",qvar,"unitValue")
      return(result)
    })
    # bind results for products in period 'i' into a matrix
    result_i <- do.call(rbind,means_i)
  })
  # bind results for all 'i' into a matrix
  result_it <- do.call(rbind,means_it)
  return(as.data.frame(result_it))
}

#' Generate an index of months
#'
#' A function to create a month index variable
#'
#' @param x A vector or column of dates
#' @param overlapWeeks Tells monthIndex how to deal with weeks that
#' cross over two adjacent months. Options are "naive", "majority", "wholeOnly" or "fourWeek".
#' "naive" simply takes the month number of the observation, ignoring where
#' the week of that observation falls. "majority" will allocate the observation
#' to the month that owns the majority of days in that week. "fourWeek"
#' first calculates a week index, then calculates the month index assuming
#' that there are four weeks in each month. "wholeOnly" will return NA for any dates
#' falling inside a week that overlaps two adjacent months; that is, only weeks that
#' are wholly within a month are given an index value. The default is "naive".
#' @examples
#' # given a vector of dates
#' df <- data.frame(date = as.Date(c("2017-01-01","2017-02-01","2017-03-01","2017-04-01"),
#' format = "%Y-%m-%d"))
#' # calculate the time period variable
#' df$period <- monthIndex(df$date, overlapWeeks = "naive")
#' df
#' @export
monthIndex <- function(x, overlapWeeks = "naive"){

  validMethods <- c("naive","majority","wholeOnly","fourWeek")
  if (!(overlapWeeks %in% validMethods)) {
    stop("Not a valid option for parameter overlapWeeks. See helpfile ?monthIndex for valid options.")
  }

  switch (overlapWeeks,
    naive = {
      firstDate <- min(x)
      firstYear <- as.numeric(format(firstDate,"%Y"))
      adj = as.numeric(format(firstDate,"%m"))-1

      month <- as.numeric(format(x,"%m"))+
        (as.numeric(format(x,"%Y"))-
           firstYear)*12-adj
    },
    majority = {
      month <- monthIndex(x, overlapWeeks = "naive")
      for (i in 1:length(month)) {
        d <- as.numeric(format(x[i],"%d")) # day of the month
        wd <- as.numeric(format(x[i],"%u")) # day of the week, Monday = 1
        # if the week overlapped slightly into next month, push it back by 1
        if (d < 4 & wd > 4 &
            as.numeric(format(as.Date(paste0(1,"-",format(x[i],"%m"),"-",format(x[i],"%Y")),
                                      format="%d-%m-%Y"),"%u")) > 4) {
          month[i] <- month[i] - 1
        }
        # if the week overlapped slightly into previous month, push it forward by 1
        else if (d > 25) {
          lastDay <- daysInMonth(x[i])
          if (lastDay - d < 3 & wd < 4 &
              as.numeric(format(as.Date(paste0(lastDay,"-",format(x[i],"%m"),"-",format(x[i],"%Y")),
                             format="%d-%m-%Y"),"%u")) < 4) {
            month[i] <- month[i] + 1
          }
        }
      }
    },
    fourWeek = {
      week <- weekIndex(x)
      month <- ceiling(week/4)
    },
    wholeOnly = {
      month <- monthIndex(x, "naive")
      for(i in 1:length(month)){
        wd <- as.numeric(format(x[i],"%u")) # day of the week, Monday = 1
        sowMonth <- as.numeric(format(x[i]-wd+1,"%m"))
        eowMonth <- as.numeric(format(x[i]-wd+7,"%m"))
        if(!(sowMonth == eowMonth)){
          month[i] = NA
        }
      }
    }
  )
  return(month)
}

#' Generate an index of quarters
#'
#' A function to create a quarter index variable
#'
#' @param x A vector or column of dates
#' @examples
#' # given a vector of dates
#' df <- data.frame(date = as.Date(c("2017-01-01","2017-04-01","2017-07-01","2017-08-01"),
#' format = "%Y-%m-%d"))
#' # calculate the time period variable
#' df$period <- quarterIndex(df$date)
#' df
#' @export
quarterIndex <- function(x){
  firstDate <- min(x)
  quarter <- ceiling(as.numeric(format(x,"%m"))/3)+
    (as.numeric(format(x,"%Y"))-
       as.numeric(format(firstDate,"%Y")))*4
  quarter <- quarter - (quarter[which.min(x)]-1)
  return(quarter)
}

#' Generate an index of years
#'
#' Function to create a year index variable
#'
#' @param x A vector or column of dates
#' @examples
#' # given a vector of dates
#' df <- data.frame(date = as.Date(c("2017-01-01","2018-04-01","2019-07-01","2019-08-01"),
#' format = "%Y-%m-%d"))
#' # calculate the time period variable
#' df$period <- yearIndex(df$date)
#' df
#' @export
yearIndex <- function(x){
  firstDate <- min(x)
  year <- as.numeric(format(x,"%Y"))-as.numeric(format(firstDate,"%Y"))+1
  return(year)
}

#' Generate an index of weeks
#'
#' Function to create a week index variable with weeks
#' determined as defined in ISO 8601.
#' If the week (starting on Monday) containing 1 January has four
#' or more days in the new year, then it is considered week 1.
#' Otherwise, it is the 53rd week of the previous year, and the
#' next week is week 1.
#'
#' @param x A vector of dates
#' @examples
#' # given a vector of dates
#' df <- data.frame(date = as.Date(c("2016-12-20","2016-12-27","2017-01-01","2017-01-07"),
#' format = "%Y-%m-%d"))
#' # calculate the time period variable
#' df$period <- weekIndex(df$date)
#' df
#' @export
weekIndex <- function(x){

  # we first need a measure of how many weeks are in each year in our sample
  years <- sort(as.numeric(unique(format(x,"%Y"))))
  # this gets the week number of December 31 for each of the years in
  # the year vector
  weeksInYears <- sapply(years,
          function(y){as.numeric(format(as.Date(paste0(y,"-12-31")),"%V"))})
  # If the week number is 1, then there must be 52 weeks in the year because
  # it's saying that the end of the calendar year falls into week 1 of the
  # following year.
  weeksInYears[weeksInYears==1] <- 52
  cumWeeks <- cumsum(weeksInYears)

  # get the week number within the year of each date
  weeks <- as.numeric(format(x,"%V"))
  # get the year of each date in 'week-year' format. see ?strptime
  weekYears <- as.numeric(format(x,"%G"))

  # initialise a matrix for our final week index
  week <- matrix(0, nrow=length(x), ncol=1)

  # get the week of the first date in our sample, we'll use this to
  # normalise our weekindex to start at 1
  firstWeek <- as.numeric(format(min(x),"%V"))

  # compute the week index as the week's number in the current year, plus the
  # number of elapsed weeks in prior years, normalised to start at week 1.
  for(i in seq_along(x)){
    week[i,1] <- weeks[i] + cumWeeks[years==weekYears[i]] - cumWeeks[1] - (firstWeek-1)
  }

  return(as.vector(week))
}
