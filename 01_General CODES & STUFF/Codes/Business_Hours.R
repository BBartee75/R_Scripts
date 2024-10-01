library(lubridate)




biz_hrs(startdate, enddate)



biz_hrs <- Vectorize(function(start, end, starting_time = '00:00', ending_time = '23:59', holidays = NULL){
  
  
  if(end < start){
    
    return(NA)
    
  } else {
    
    start_datetime <- as.POSIXct(paste0(substr(start,1,11), starting_time, ':00'))
    end_datetime <- as.POSIXct(paste0(substr(end,1,11), ending_time, ':00'))
    
    if(as.Date(start) == as.Date(end) & !as.Date(start) %in% holidays & !format(as.Date(start), "%u") %in% c(6,7)){ #if starting time stamp is on same day as ending time stamp and if day is not a holiday or weekend
      
      if(start > start_datetime & end < end_datetime){ #if starting time stamp is later than start business hour and ending time stamp is earlier then ending business hour.
        return(as.numeric(difftime(end, start), units = 'hours'))
      } else if(start > start_datetime & end > end_datetime & start < end_datetime){ #if starting time stamp is later than end business hour and ending time stamp is earlier then ending business hour.
        return(as.numeric(difftime(as.POSIXct(paste0(substr(start,1,11), ending_time, ':00')), start), units = 'hours'))
      } else if(start < start_datetime & end < end_datetime & end > start_datetime){ #if starting time stamp is earlier than end business hour and ending time stamp is later than starting business hour.
        return(as.numeric(difftime(end, start_datetime), units = 'hours'))
      } else if(start > end_datetime & end > end_datetime){ #if starting time stamp is later than end business hour and ending time stamp is later than ending business hour.
        return(0)
      } else if(start < start_datetime & end < start_datetime){ #if starting time stamp is earlier than start business hour and ending time stamp is earlier than starting business hour.
        return(0)
      } else {
        return(as.numeric(difftime(end_datetime, start_datetime), units = 'hours'))
      }
      
    } else { #if starting time stamp and ending time stamp occured on a different day.
      
      business_hrs <- as.numeric(difftime(as.POSIXct(paste0('2017-01-01', ending_time, ':00')),
                                          as.POSIXct(paste0('2017-01-01', starting_time, ':00')) #calculate business hours range by specified parameters
      ), units = 'hours')
      
      start_day_hrs <- ifelse(start < as.POSIXct(paste0(substr(start,1,11), ending_time, ':00')) & !as.Date(start) %in% holidays & !format(as.Date(start), "%u") %in% c(6,7), #if start time stamp is earlier than specified ending time
                              as.numeric(difftime(as.POSIXct(paste0(substr(start,1,11), ending_time, ':00')), start), units = 'hours'), #calculate time between time stamp and specified ending time
                              0 #else set zero
      ) #calculate amount of time on starting day
      start_day_hrs <- pmin(start_day_hrs, business_hrs) #cap the maximum amount of hours dertermined by the specified business hours
      start_day_hrs
      end_day_hrs <- ifelse(end > as.POSIXct(paste0(substr(end,1,11), starting_time, ':00')) & !as.Date(end) %in% holidays & !format(as.Date(end), "%u") %in% c(6,7), #if end time stamp is later than specified starting time
                            as.numeric(difftime(end, as.POSIXct(paste0(substr(end,1,11), starting_time, ':00'))), units = 'hours'), #calculate time between time stamp and specified starting time
                            0) #calculate amount of time on ending day
      end_day_hrs <- pmin(end_day_hrs, business_hrs) #cap the maximum amount of hours dertermined by the specified business hours
      days_between <- seq(as.Date(start), as.Date(end), by = 1) #create a vector of dates (from and up to including) the starting time stamp and ending time stamp
      business_days <- days_between[!days_between %in% c(as.Date(start), as.Date(end)) & !days_between %in% holidays & !format(as.Date(days_between), "%u") %in% c(6,7)] #remove weekends and holidays from vector of dates
      
      return(as.numeric(((length(business_days) * business_hrs) + start_day_hrs + end_day_hrs))) #multiply the remaining number of days in the vector (business days) by the amount of business hours and add hours from the starting and end day. Return the result
      
    }
    
  }
  
  
})