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

#' Generate an index of months
#'
#' A function to create a month index variable
#'
#' @param x A vector or column of dates
#' @param overlapWeeks Tells monthIndex how to deal with weeks that
#' cross over two adjacent months. Options are "naive", "majority", "wholeOnly" or "fourWeek".
#' "naive" simply takes the month number of the observation, ignoring where
#' the week of that observation falls. "majority" will allocate the observation
#' to the month that owns the majority of days in that week, assuming
#' that Monday is day one of the week. "fourWeek" first calculates a week index,
#' then calculates the month index assuming that there are four weeks in each month.
#' "wholeOnly" will return NA for any dates falling inside a week that overlaps two
#' adjacent months; that is, only weeks that are wholly within a month
#' are given an index value. The default is "naive".
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

            d <- as.numeric(format(x, "%d"))
            wd <- as.numeric(format(x, "%u"))
            m <- as.numeric(format(x, "%m"))
            y <- as.numeric(format(x, "%Y"))

            for (i in 1:length(month)) {
              # if the week overlapped slightly into next month, push it back by 1
              if (d[i] < 4 & wd[i] > 4 &
                  as.numeric(format(as.Date(paste0(1, "-", m[i], "-", y[i]),
                                            format="%d-%m-%Y"),"%u")) > 4) {
                month[i] <- month[i] - 1
              }
              # if the week overlapped slightly into previous month, push it forward by 1
              else if (d[i] > 25) {
                lastDay <- daysInMonth(x[i])
                if (lastDay - d[i] < 3 & wd[i] < 4 &
                    as.numeric(format(as.Date(paste0(lastDay, "-", m[i], "-", y[i]),
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
            wd <- as.numeric(format(x, "%u")) # day of the week, Monday = 1
            sowMonth <- as.numeric(format(x - wd + 1, "%m"))
            eowMonth <- as.numeric(format(x - wd + 7, "%m"))
            month[!(sowMonth == eowMonth)] <- NA
          }
  )
  return(month)
}
