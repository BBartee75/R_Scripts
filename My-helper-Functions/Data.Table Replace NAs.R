library(data.table)

DT = data.table(a=c(1,"A",NA),b=c(4,NA,"B"))

DT
    a  b
1:  1  4
2:  A NA
3: NA  B

DT[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})] #you can change na to nan if needed
   a b
1: 1 4
2: A 0
3: 0 B