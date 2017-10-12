install.packages("ggplot2")
library(ggplot2)
INcornmakeswhiskey00_09 <- subset(IN_field_corn, IN_field_corn$YEAR >= 2000 & YEAR <= 2009)
INcornmakeswhiskey00_09$YEAR <- factor(INcornmakeswhiskey00_09$YEAR) 
cornyield <- subset(INcornmakeswhiskey00_09, SHORT_DESC == "CORN, GRAIN - YIELD, MEASURED IN BU / ACRE")
cornyieldbycounty <- subset(cornyield, AGG_LEVEL_DESC == "COUNTY")
cornyieldbycounty$COUNTY_NAME <- factor(cornyieldbycounty$COUNTY_NAME)
cornyieldbycounty$YEAR <- factor(cornyieldbycounty$YEAR)
tapply(as.numeric(cornyieldbycounty$VALUE),cornyieldbycounty$COUNTY_NAME,mean,na.rm=T)
mean_corn_yield <- tapply(as.numeric(cornyieldbycounty$VALUE),cornyieldbycounty$YEAR,mean,na.rm=T)
mean_corn_yield <- data.frame(names(mean_corn_yield),mean_corn_yield)
qplot(mean_corn_yield$names.mean_corn_yield.,mean_corn_yield$mean_corn_yield,mean_corn_yield,xlab="Year",ylab="Corn Yield (BU / ACRE)")
head(mean_corn_yield)
head(cornyieldbycounty$VALUE)
#Median
median_corn_yield <- tapply(as.numeric(cornyieldbycounty$VALUE),cornyieldbycounty$YEAR,median,na.rm=T)
median_corn_yield <- data.frame(names(median_corn_yield),median_corn_yield)
qplot(median_corn_yield$names.median_corn_yield.,median_corn_yield$median_corn_yield,median_corn_yield,xlab="Year",ylab="Corn Yield (BU / ACRE)")

#Median by County for 2009
cornyieldbycounty09 <- cornyieldbycounty[ cornyieldbycounty$YEAR == 2009,]
mediancounties <- tapply(as.numeric(cornyieldbycounty09$VALUE),cornyieldbycounty09$COUNTY_NAME, median,na.rm=T)
mediancounties
mediancountyyield09 <- data.frame(names(mediancounties),mediancounties)
qplot(mediancountyyield09$names.mediancounties.,mediancountyyield09$mediancounties,data=mediancountyyield09)



## Production for 1981 through 2010 for Indiana
INcorn81_10 <- IN_field_corn[IN_field_corn$YEAR >= 1981 & IN_field_corn$YEAR <= 2010, ]
cornproduction81_10 <- INcorn81_10[INcorn81_10$SHORT_DESC == "CORN, GRAIN - PRODUCTION, MEASURED IN BU", ]
cornproduction81_10bycounty <- cornproduction81_10[cornproduction81_10$AGG_LEVEL_DESC == "COUNTY", ]
cornproduction81_10bycounty$COUNTY_NAME <- factor(cornproduction81_10bycounty$COUNTY_NAME)
meanproductionyield81_10bycounty <- tapply(as.numeric(cornproduction81_10bycounty$VALUE),cornproduction81_10bycounty$COUNTY_NAME,mean, na.rm=T)
meanproductionyield81_10bycounty
## Acres Harvested for 1981 through 2010 for Indiana
cornharvested81_10 <- INcorn81_10[INcorn81_10$SHORT_DESC == "CORN, GRAIN - ACRES HARVESTED", ]
cornharvested81_10bycounty <- cornharvested81_10[cornharvested81_10$AGG_LEVEL_DESC == "COUNTY", ]
cornharvested81_10bycounty$COUNTY_NAME <- factor(cornharvested81_10bycounty$COUNTY_NAME)
meanharvested81_10bycounty <- tapply(as.numeric(cornharvested81_10bycounty$VALUE),cornharvested81_10bycounty$COUNTY_NAME,mean, na.rm=T)
meanharvested81_10bycounty

meanproductionyield81_10bycounty / meanharvested81_10bycounty
