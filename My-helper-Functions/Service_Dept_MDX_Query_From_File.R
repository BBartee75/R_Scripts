## Script to retreive data from OLAP Analysis Server (AS) using a OLAP connection.
## Define first a MDX query in a file. read it, and then do the select to return results
##

#install.packages("olapR")
#install.packages("dplyr")

## *** Load Libraries

library(stringr)
library(dplyr)
library(olapR)

## *** Define some parameters that will be used in the query. Note: Do not include extra spaces before or after e.g. "@FarmName:Achiras" 
QueryParameters <- c(  
                         "@FarmName:Lagoa do Barro"
                       , "@TimeZone:LocalDate"
                       , "@StartDate:2019-01-01"
                       , "@EndDate:2019-07-01"
                   )

{
#mdxQuery = '
    #SELECT
	#NON EMPTY{
		#order([Measures].[LocalStartTime],desc)
		#,[Measures].[LocalEndTime]
		#,[Measures].[AlarmDuration]
	#} ON 0,
	#NON EMPTY{
		#Crossjoin(
			#Crossjoin(
				#CrossJoin(
					#CrossJoin(
						#[Geography].[Country].children
						#,[WTG].[WTG].children
					#)
					#,[@TimeZone].[Date].Children
				#)
				#,[Alarm].[Code].Children
				#,[Alarm].[Description].[Description]
			#)
			#,[Alarms].[MaintenanceFlag].Children
			#,[Alarms].[IsClosedByOtherAlarm].Children
		#)
	#} ON ROWS  
#FROM [SHARP] 
#WHERE (
	#[WTG].[FarmName].&[@FarmName]
	#,[@TimeZone].[Day Date].&[@StartDateT00:00:00]:[@TimeZone].[Day Date].&[@EndDateT00:00:00]
#)
    #'
}

## *** Define what file contains the MDX query (exported from MS management Studio). *** 
{
    QueryFileName = 'c:/sjt/repos/no_scontrol/dataimporttests_r/rproject1/queries/service dept/alarms001_v1.mdx'
    #QueryFileName = 'c:/sjt/repos/no_scontrol/dataimporttests_r/rproject1/queries/service dept/statistics_001v1.mdx'
}



## ***  Open,read and close the file. Note: strip the firtst 3 characters due to the encoding of the file. TODO: improve the encoding problem *** 
{
    connection = file(QueryFileName, open = "r")
    Filesize = file.size(QueryFileName)
    mdxQuery = readChar(connection, nchars = 3) ## Strip encoding header
    mdxQuery = readChar(connection, nchars = (file.size(QueryFileName) - 3))
    close.connection(connection)
}

# *** Replace keywords. TODO: Use a for each and a dictionary to optimize code *** 
{
    for (val in QueryParameters) { mdxQuery = str_replace_all(mdxQuery, str_split(val, ":", simplify = TRUE)[1], str_split(val, ":", simplify = TRUE)[2]) }
}

## ***  Clean unwanted characters from text (leave new lines "\n") *** 
{
    mdxQuery = str_replace_all(str_replace_all(mdxQuery, "\n", " "), "'", "''")
}

# *** Connect to cube, send Query and get results *** 
{
    ConectionString <- ("Provider = MSOLAP;Integrated Security = SSPI;Persist Security Info = True;Initial Catalog = AccionaSHARPTabular;Data Source = AMSSQL12A\\SQL2016;Safety Options = 2;Update Isolation Level = 2")
    olapCnn <- OlapConnection(ConectionString)
    result1 <- execute2D(olapCnn, mdxQuery)
}


