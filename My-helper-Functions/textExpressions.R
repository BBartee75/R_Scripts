#split names crate function
splitNames = strsplit(names(df), "\\.") # by period

#subsitute ore repalce characheters in a string/name
sub("_", "", names(df),) # will remove the underscore "_" first only underscore
gsub("_", "", names(df),) # will remove all from above

# str_trim() removes whitespace from start and end of string; str_squish() also reduces repeated whitespace inside a string.
#library(stringr)
str_trim(string, side = c("both", "left", "right"))

# Metacharacters Searching
#start of line
^i think #will search all line that begin with i think
morning$ #will search allines that end with morning
^[0-9][a-zA-Z] # will match any number 0-9, a-z lowercase or A-Z uppercase
9.11 # will serach for any 9 or 11. the period mean search for any character
flood|fire # will search for flood or fire the | is or, use for many x|x|x|x|..........

# Formating Dates ******************************
%d # day as number
%a # abbrevatied weekday
%A # Weekday full
%m # month number
%b # abbrevatied month
%B # full month name
%y # 2 digit year
%Y # 4 digit year 

