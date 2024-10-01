
#' Function to aggregate continuous time series data.
#' @param eventTs: An event time series obtained from event2Ts.
#' @param byGroups: Columns to be used for grouping in aggregation step.
#'
#' @return Returns an aggregated, continuous time series including total duration and occurrence.
#'
#' @export
#'
#'
aggregateEventTs <- function(eventTs, byGroups) {

  if (is.data.table(eventTs)) {

    x <- copy(eventTs)

  } else {

    x <- as.data.table(eventTs)

  }

  x <- x[, .(firstEventStart = first(eventStart), lastEventEnd = last(eventEnd), totalDuration = sum(duration), totalOccurence = .N), by = mget(byGroups)]

  return(x)

}
