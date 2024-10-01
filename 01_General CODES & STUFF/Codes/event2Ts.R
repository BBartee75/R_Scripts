
#' Function to convert event-like data to a continuous time series.
#' @param events: Events to convert to a time series.
#' @param startCol: Name of column that contains the event start.
#' @param stopCol: Name of column that contains the event stop.
#' @param interval: Interval size in seconds. Default is 600 seconds.
#'
#' @return Returns a non-aggregated continuous time series. Event starts and event stops are adjusted to match the time series character.
#'
#' @export
#'
#'
event2Ts <- function(events, startCol, stopCol, interval = 600) {

  if (is.data.table(events)) {

    x <- copy(events)

  } else {

    x <- as.data.table(events)

  }

  setnames(x, old = c(startCol, stopCol), new = c("StartUnixDT", "StopUnixDT"))
  converted <- FALSE
  if (is.POSIXct(x$StartUnixDT)) {

    x[, StartUnixDT := as.numeric(StartUnixDT)]
    converted <- TRUE

  }

  if (is.POSIXct(x$StopUnixDT)) {

    x[, StopUnixDT := as.numeric(StopUnixDT)]
    converted <- TRUE

  }


  x[, rowId := .I]
  by.ids <- c("rowId", "StartUnixDT", "StopUnixDT")
  x_ts <- x[, .(UnixDT = seq(floor(StartUnixDT / interval) * interval, StopUnixDT, by = interval)), by = mget(by.ids)]
  x_ts[x, on = c(by.ids), (colnames(x)) := mget(colnames(x))]
  x_ts[, rowId := NULL]

  x_ts[, StopUnixDT  := UnixDT + pmin(interval, StopUnixDT  - UnixDT)]
  x_ts[, StartUnixDT := UnixDT + pmax(0,        StartUnixDT - UnixDT)]
  x_ts[, duration    := StopUnixDT - StartUnixDT]

  if (converted) {

    x_ts[, ':=' (StartUnixDT = as.POSIXct(StartUnixDT, tz = "UTC", origin = "1970-01-01"),
                 StopUnixDT  = as.POSIXct(StopUnixDT, tz = "UTC", origin = "1970-01-01"))]

  }

  setnames(x_ts, old = c("StartUnixDT", "StopUnixDT"), new = c(startCol, stopCol))

  return(x_ts)

}
