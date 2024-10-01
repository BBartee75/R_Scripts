#Number of days in a month

#You can write simple function to do that:


numberOfDays <- function(date) {
    m <- format(date, format="%m")

    while (format(date, format="%m") == m) {
        date <- date + 1
    }

    return(as.integer(format(date - 1, format="%d")))
}


#Invoke as:
#date = as.Date("2011-02-23", "%Y-%m-%d")
# run this function numberOfDays(date)
# output is 28
# date # date is unchanged
# date output "2011-02-23"