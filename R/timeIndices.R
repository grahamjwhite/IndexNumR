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

  # get the week number within the year of each date
  weeks <- as.numeric(format(x,"%V"))
  # get the year of each date in 'week-year' format. see ?strptime
  weekYears <- as.numeric(format(x,"%G"))
  # we first need a measure of how many weeks are in each year in our sample
  years <- sort(unique(weekYears))

  # this gets the week number of December 31 for each of the years in
  # the year vector
  weeksInYears <- sapply(years,
                         function(y){as.numeric(format(as.Date(paste0(y,"-12-31")),"%V"))})
  # If the week number is 1, then there must be 52 weeks in the year because
  # it's saying that the end of the calendar year falls into week 1 of the
  # following year.
  weeksInYears[weeksInYears==1] <- 52
  cumWeeks <- cumsum(weeksInYears)

  # get the week of the first date in our sample, we'll use this to
  # normalise our weekindex to start at 1
  firstWeek <- as.numeric(format(min(x),"%V"))

  # compute the week index as the week's number in the current year, plus the
  # number of elapsed weeks in prior years, normalised to start at week 1.
  yearPos <- match(weekYears, years)
  week <- weeks + cumWeeks[yearPos] - weeksInYears[yearPos] - (firstWeek-1)

  return(week)
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
            # POSIXlt gives the date parts directly, which is much faster than format()
            lt <- as.POSIXlt(x)
            firstLt <- as.POSIXlt(min(x))

            month <- (lt$mon + 1) + (lt$year - firstLt$year)*12 - firstLt$mon
          },
          majority = {
            month <- monthIndex(x, overlapWeeks = "naive")

            lt <- as.POSIXlt(x)
            d <- lt$mday
            wd <- ifelse(lt$wday == 0, 7, lt$wday) # day of the week, Monday = 1
            y <- lt$year + 1900
            m <- lt$mon + 1

            # number of days in the month of each date
            daysInMonth <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[m]
            leapYear <- (y %% 4 == 0 & y %% 100 != 0) | y %% 400 == 0
            lastDay <- daysInMonth + (m == 2 & leapYear)

            # day of the week of the first and last day of each date's month
            firstWd <- (wd - d) %% 7 + 1
            lastWd <- (firstWd + lastDay - 2) %% 7 + 1

            # if the week overlapped slightly into next month, push it back by 1
            back <- d < 4 & wd > 4 & firstWd > 4

            # if the week overlapped slightly into previous month, push it forward by 1
            forward <- !back & d > 25 & lastDay - d < 3 & wd < 4 & lastWd < 4

            month[back] <- month[back] - 1
            month[forward] <- month[forward] + 1

            # moving the earliest dates into the previous or next month can
            # shift the start of the index, so re-normalise it to start at 1
            month <- month - min(month) + 1
          },
          fourWeek = {
            week <- weekIndex(x)
            month <- ceiling(week/4)
          },
          wholeOnly = {
            month <- monthIndex(x, "naive")
            wday <- as.POSIXlt(x)$wday
            wd <- ifelse(wday == 0, 7, wday) # day of the week, Monday = 1
            sowMonth <- as.POSIXlt(x - wd + 1)$mon
            eowMonth <- as.POSIXlt(x - wd + 7)$mon
            month[!(sowMonth == eowMonth)] <- NA
          }
  )
  return(month)
}
