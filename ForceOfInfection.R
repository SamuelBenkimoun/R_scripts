# Importing the files
library(readxl)
library(dplyr)
mob_US <- read_excel("For article USA 09_09.xls")
cases_US <- read_excel("For article USA 09_09.xls", sheet = "New cases")
# Renaming column names in date format (sometimes read as numbers when imported from Excel)
i = 0
for (i in 6:length(mob_US)) {
  colnames(mob_US)[i] <- as.character(as.Date(as.numeric(colnames(mob_US)[i]), origin = "1899-12-30"))
}
# Joining the mobility and the cases from the departure regions
join_US <- left_join(mob_US, cases_US, by= c("from County"="countyFIPS"))
# Multiplying the FB population flow by the number of cases in the departure region
foi_mob_US <- data.frame(
    Map(function(x,y) if(all(is.numeric(x),is.numeric(y))) x * y else x, join_US[6:31], join_US[36:61])
)
foi_mob_US <- cbind(join_US[1:5], foi_mob_US)
# Summing the FOI by Region of destination
county_FOI <- aggregate(foi_mob_US[6:31], by= list(County=foi_mob_US$`To County`), FUN = sum)
county_FOI <- left_join(county_FOI, unique(foi_mob_US[c(3,5)]), by= c("County"="To County"))
county_FOI <- cbind(county_FOI[28], county_FOI[1:27])
# Applying a log value on the dataframe
county_FOI <- cbind(county_FOI[1:2],log10(county_FOI[3:28]+1))
# Cleaning the column names (removing the X from the left join operation)
colnames(county_FOI) <- gsub('X', "", colnames(county_FOI))
write.csv(county_FOI, "./county_FOI.csv")
