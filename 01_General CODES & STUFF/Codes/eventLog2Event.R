
#' Function to transform event log data into an event-like shape.
#' @param logAlarms: Event log data obtained from getStLog.
#' @param trustFirstOn: Flag (TRUE/FALSE) if first on or last on should be used. FALSE by default.
#' @param trustFirstOff: Flag (TRUE/FALSE) if first off or last off should be used. TRUE by default.
#' @param keepLast: Flag (TRUE/FALSE) if current, active events should be kept and extended to lastDay. FALSE by default.
#' @param keepFirst: Flag (TRUE/FALSE) if first event should be kept and extended to firstDay. FALSE by default.
#' @param keepEventOff: Flag (TRUE/FALSE) if on or off events should be returned. Defaults to on events.
#' @param lastDay: Datetime that is used in keepLast.
#' @param firstDay: Datetime that is used in keepFirst.
#' @return Returns transformed event log with a timestamp column for both - event start and event stop.
#'
#' @export
#'
eventLog2Event <- function(eventLog, trustFirstOn = FALSE, trustFirstOff = TRUE, keepLast = FALSE, keepFirst = FALSE, keepEventOff = FALSE,
                           lastDay = Sys.Date(), firstDay) {

  x <- copy(eventLog)

  # Order rows for further processing
  setkey(x, WEA, TimeUTC, MS)

  if (keepFirst) {

    # Create artifical first state
    firstState <- x[x[, .(idx = .I[1L]), by=.(WEA, ALMLOGID)]$idx]
    firstState[, ':=' (TimeUTC = as.POSIXct(paste(firstDay, "00:00:00"), tz = "UTC"), MS = 0, STATE = ifelse(STATE == 1, 0, 1), OPERATOR = 0)]

    # Bind to input
    x <- rbindlist(list(firstState, x))

    # Order
    setkey(x, WEA, TimeUTC, MS)

  }

  # Normalize rising Edge to 0 = off and 1 = on
  x[, RLEID := rleid(STATE), by = .(WEA, ALMLOGID)]


  # Add keep column to for trust.first.on and trust.first.off flag
  x[, keep  := 0]

  if (trustFirstOn) {

    x[x[STATE == 1, .I[1],  by = .(WEA, ALMLOGID, RLEID)]$V1, keep := 1]

  } else {

    x[STATE == 1, keep := 1]

  }

  if (trustFirstOff) {

    x[x[STATE == 0, .I[1],  by = .(WEA, ALMLOGID, RLEID)]$V1, keep := 1]

  } else {

    x[x[STATE == 0, .I[.N],  by = .(WEA, ALMLOGID, RLEID)]$V1, keep := 1]

  }

  # Filter double values
  x <- x[keep == 1]

  # Add event end
  x[, ':=' (eventStart   = TimeUTC,
            eventStartMS = MS,
            eventEnd     = shift(TimeUTC,   type = "lead"),
            eventEndMS   = shift(MS,        type = "lead"),
            flaggedOff   = 1 - shift(STATE, type = "lead", fill = 1)),
    by = .(WEA, ALMLOGID)]


  # Keep currently active event
  if (keepLast) {

    x[is.na(eventEnd), ':=' (eventEnd   = as.numeric(as.POSIXct(paste(lastDay, "00:00:00"), tz = "UTC") %m-% seconds(1)),
                             eventEndMS = 0)]

  } else {

    x <- x[!is.na(eventEnd)]

  }

  # Add duration
  x[, eventDuration := as.integer(eventEnd - eventStart)]


  # Return only results
  keepState <- 1
  if (keepEventOff) keepState <- 0

  return(x[STATE == keepState,
           .(WEA, ALMLOGID,
             eventStart, eventStartMS,
             eventEnd, eventEndMS,
             eventDuration, flaggedOff)])

}
