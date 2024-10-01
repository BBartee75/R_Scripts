
gsub('([[:punct:]])|\\s+',' ', X ) #fill in X for your data


gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", X, perl=TRUE) #fill in X for your data
 
 
gsub("[^0-9A-Za-z///' ]","" , X,ignore.case = TRUE)  #fill in X for your data