# load packages
library(ggplot2)

# Get data for corn belt yields and make a map for 3 decades: 1981-1990, 1991-2000, 2001-2010
# Corn belt is Indiana, Illinois, Ohio, Wisconsin, Minnesota, and Iowa

#corn <- read.table("C:\\Users\\nethe\\Documents\\Research\\corn_data.txt", header = TRUE, sep = "\t", fill = TRUE)
#field_corn <- subset(corn, COMMODITY_DESC == 'CORN')
cornbelt_corn <- field_corn[ field_corn$STATE_NAME == 'INDIANA' | field_corn$STATE_NAME == 'ILLINOIS' | field_corn$STATE_NAME == 'OHIO'
                            | field_corn$STATE_NAME == 'WISCONSIN' | field_corn$STATE_NAME == 'MINNESOTA' | field_corn$STATE_NAME == 'IOWA'
                             && field_corn$AGG_LEVEL_DESC == 'COUNTY', ] 
cornbelt_corn$COUNTY_NAME <- factor(cornbelt_corn$COUNTY_NAME) 
yields <- cornbelt_corn[ cornbelt_corn$SHORT_DESC == 'CORN, GRAIN - YIELD, MEASURED IN BU / ACRE' & cornbelt_corn$SOURCE_DESC == 'SURVEY',]
yields_80s <- yields[ yields$YEAR >= '1981' & yields$YEAR <= '1990',] 
yields_90s <- yields[ yields$YEAR >= '1991' & yields$YEAR <= '2000',] 
yields_00s <- yields[ yields$YEAR >= '2001' & yields$YEAR <= '2010',] 
