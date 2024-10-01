#Search for Any NA's
any(is.na(dataframe/table))

#count NA's
sum(is.na((dataframe/table))

#Where are NAs
summary(dataframe/table)

#remove rows with NAs
na.omit(dataframe/table)

# replace na

DT[, names(DT) := lapply(.SD, function(x) {x[is.na(x)] <- "0" ; x})]

dt[is.na(dt)] <- 0

f_rep <- function(dt) {
dt[is.na(dt)] <- 0
return(dt)
}

remove_na <- function(x){
  dm <- data.matrix(x)
  dm[is.na(dm)] <- 0
  data.table(dm)
}

delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}
