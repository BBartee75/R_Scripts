NACheck1 <- (is.na(WTGdata)==TRUE)
NACheck2 <- any(is.na(WTGdata)) # any N/A's
NACheck3 <- which(is.na(WTGdata) == TRUE, arr.ind=TRUE) # where are the N/A's