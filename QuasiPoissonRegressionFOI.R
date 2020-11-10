library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
options(scipen=999)
#IMPORTING THE CASES DATA
cases_US <- read_excel("covid_confirmed_usafacts2.xlsx")
##labeling the columns with the proper date format
i = 5
k = 43899
for (i in 5:length(cases_US)) {
  colnames(cases_US)[i] <- k
  colnames(cases_US)[i] <- as.character(as.Date(as.numeric(colnames(cases_US)[i]), origin = "1899-12-30"))
  k = k+7
}
cases_Col <- read_csv("Col_cases_week.csv", col_types = cols(X1 = col_skip()))
##labeling the columns with the proper date format
i = 2
k = 43903
for (i in 2:length(cases_Col)) {
  colnames(cases_Col)[i] <- k
  colnames(cases_Col)[i] <- as.character(as.Date(as.numeric(colnames(cases_Col)[i]), origin = "1899-12-30"))
  k = k+7
}
cases_Sw <- read_csv("Sw_cases.csv", col_types = cols(X1 = col_skip()))
cases_Fr <- read_csv("France_cases_week.csv", col_types = cols(X1 = col_skip()))

#IMPORTING MOBILITY DATA
mob_US <- read_excel("For article USA 09_09.xls")
##labeling the columns with the proper date format
i = 0
for (i in 6:length(mob_US)) {
  colnames(mob_US)[i] <- as.character(as.Date(as.numeric(colnames(mob_US)[i]), origin = "1899-12-30"))
}
mob_Col <- read_excel("Colombia Admin per day and week.xlsx", sheet = "Week wo intra")
##labeling the columns with the proper date format
colnames(mob_Col) <- gsub("\\..*","",colnames(mob_Col))
i = 0
for (i in 10:length(mob_Col)) {
  colnames(mob_Col)[i] <- as.character(as.Date(as.numeric(colnames(mob_Col)[i]), origin = "1899-12-30"))
}
mob_Sw <- read_excel("Sweden for Article.xlsx")
i= 0
for (i in 2:length(mob_Sw)) {
  colnames(mob_Sw)[i] <- as.character(as.Date(as.numeric(colnames(mob_Sw)[i]), origin = "1899-12-30"))
}
mob_Fr <- read_delim("France mobility.csv", ";", escape_double = FALSE, locale = locale(), trim_ws = TRUE)

# IMPORTING THE FORCE OF INFECTION COMPUTED DATA
FOI_US <- read_csv("./FOI/USA_county_FOI.csv")
FOI_Col <- read_csv("./FOI/Colombia_municipios_FOI.csv")
FOI_Sw <- read_csv("./FOI/Sweden_county_FOI.csv")
FOI_Fr <- read_csv("./FOI/Fra_dept_FOI.csv")

# IMPORTING POPULATION DATA
pop_US <- read_csv("US_co-est2019-alldata.csv", col_types = cols(STATE = col_number()))
pop_US <- mutate(pop_US, countyFIPS= paste(pop_US$STATE, pop_US$COUNTY, sep = ""))
pop_Col <- read_excel("Colombia-densidad-municipios.xls")
colnames(pop_Col)[1] <- c("MCPIOS")
pop_Sw <- read_excel("Sweden_be0101_pop.xlsx")
colnames(pop_Sw)[2:3] <- c("Location", "Pop_Tot")
pop_Fr <- read_excel("France_estim-pop-dep-sexe-gca-1975-2020.xls", sheet = "2020")
colnames(pop_Fr)[1] <- c("dep")

# USA Constituting the results tables  
resulttable_US <- read.csv(text=("Date,P-value_FOI,P-value_Cases_week-1,P-value_Interaction, Dev_FOI, Dev_Cases_week-1, Dev_Interaction, Intercept, Coef_FOI, Coef_Cases-1, Coef_Inter, Pseudo-R2"))
i <- 5
j <- 1


for (i in 5:length(cases_US)) {
  poissondata <- left_join(cases_US[c(1,2,i, i+1)], FOI_US[c(3,i-1)], by= c("countyFIPS"="County"))%>%
    na.omit() %>%
    merge(select(pop_US, c("POPESTIMATE2019", "countyFIPS")))
  resulttable_US[j,1] <- colnames(poissondata)[4]
  colnames(poissondata) <- c("county_id", "county_name", "cases_week_0", "cases_week_1", "logFOI_week_0", "population")
  model <- glm(formula=cases_week_1~logFOI_week_0+cases_week_0+cases_week_0:logFOI_week_0,
               family="quasipoisson", 
               data=poissondata, 
               offset = log(population))
  resulttable_US[j,2] <- coef(summary(model))[2,4] #FOI
  resulttable_US[j,3] <- coef(summary(model))[3,4] #Cases
  resulttable_US[j,4] <- coef(summary(model))[4,4] #Interaction
  resulttable_US[j,5] <- anova(model)$Deviance[2]/anova(model)$'Resid. Dev'[1]*100
  resulttable_US[j,6] <- anova(model)$Deviance[3]/anova(model)$'Resid. Dev'[1]*100
  resulttable_US[j,7] <- anova(model)$Deviance[4]/anova(model)$'Resid. Dev'[1]*100
  resulttable_US[j,8] <- coef(model)["(Intercept)"]
  resulttable_US[j,9] <- coef(model)["logFOI_week_0"]
  resulttable_US[j,10] <- coef(model)["cases_week_0"]
  resulttable_US[j,11] <- coef(model)["logFOI_week_0:cases_week_0"]
  resulttable_US[j,12] <- with(summary(model), 1 - deviance/null.deviance)
  j=j+1
}

# COLOMBIA Constituting the results tables
resulttable_Col <- read.csv(text=("Date,P-value_FOI,P-value_Cases_week-1,P-value_Interaction, Dev_FOI, Dev_Cases_week-1, Dev_Interaction, Intercept, Coef_FOI, Coef_Cases-1, Coef_Inter, Pseudo-R2"))
j <- 1
for (i in 2:length(cases_Col)) {
  poissondata <- left_join(cases_Col[c(1,i, i+1)], FOI_Col[c(2,i+2)], by= c("MCPIOS"="End MCPIOS"))%>%
    na.omit() %>%
    merge(select(pop_Col, c("MCPIOS", "POB2010")))
  resulttable_Col[j,1] <- colnames(poissondata)[3]
  colnames(poissondata) <- c("county_id", "cases_week_0", "cases_week_1", "logFOI_week_0", "population")
  model <- glm(formula=cases_week_1~logFOI_week_0+cases_week_0+cases_week_0:logFOI_week_0,
               family="quasipoisson", 
               data=poissondata, 
               offset = log(population))
  resulttable_Col[j,2] <- coef(summary(model))[2,4] #FOI
  resulttable_Col[j,3] <- coef(summary(model))[3,4] #Cases
  resulttable_Col[j,4] <- coef(summary(model))[4,4] #Interaction
  resulttable_Col[j,5] <- anova(model)$Deviance[2]/anova(model)$'Resid. Dev'[1]*100
  resulttable_Col[j,6] <- anova(model)$Deviance[3]/anova(model)$'Resid. Dev'[1]*100
  resulttable_Col[j,7] <- anova(model)$Deviance[4]/anova(model)$'Resid. Dev'[1]*100
  resulttable_Col[j,8] <- coef(model)["(Intercept)"]
  resulttable_Col[j,9] <- coef(model)["logFOI_week_0"]
  resulttable_Col[j,10] <- coef(model)["cases_week_0"]
  resulttable_Col[j,11] <- coef(model)["logFOI_week_0:cases_week_0"]
  resulttable_Col[j,12] <- with(summary(model), 1 - deviance/null.deviance)
  
  j=j+1
}

# SWEDEN Constituting the results tables
resulttable_Sw <- read.csv(text=("Date,P-value_FOI,P-value_Cases_week-1,P-value_Interaction, Dev_FOI, Dev_Cases_week-1, Dev_Interaction, Intercept, Coef_FOI, Coef_Cases-1, Coef_Inter, Pseudo-R2"))
i <- 2
j <- 1
for (i in 2:length(cases_Sw)) {
  poissondata <- left_join(cases_Sw[c(1,i, i+1)], FOI_Sw[c(2,i+1)], by= c("Location"="Location"))%>%
    na.omit() %>%
    merge(select(pop_Sw, c("Location", "Pop_Tot")))
  resulttable_Sw[j,1] <- colnames(poissondata)[3]
  colnames(poissondata) <- c("county_id", "cases_week_0", "cases_week_1", "logFOI_week_0", "population")
  model <- glm(formula=cases_week_1~logFOI_week_0+cases_week_0+cases_week_0:logFOI_week_0,
               family="quasipoisson", 
               data=poissondata, 
               offset = log(population))
  resulttable_Sw[j,2] <- coef(summary(model))[2,4] #FOI
  resulttable_Sw[j,3] <- coef(summary(model))[3,4] #Cases
  resulttable_Sw[j,4] <- coef(summary(model))[4,4] #Interaction
  resulttable_Sw[j,5] <- anova(model)$Deviance[2]/anova(model)$'Resid. Dev'[1]*100
  resulttable_Sw[j,6] <- anova(model)$Deviance[3]/anova(model)$'Resid. Dev'[1]*100
  resulttable_Sw[j,7] <- anova(model)$Deviance[4]/anova(model)$'Resid. Dev'[1]*100
  resulttable_Sw[j,8] <- coef(model)["(Intercept)"]
  resulttable_Sw[j,9] <- coef(model)["logFOI_week_0"]
  resulttable_Sw[j,10] <- coef(model)["cases_week_0"]
  resulttable_Sw[j,11] <- coef(model)["logFOI_week_0:cases_week_0"]
  resulttable_Sw[j,12] <- with(summary(model), 1 - deviance/null.deviance)
  j=j+1
}

# FRANCE Constituting the results tables
resulttable_Fr <- read.csv(text=("Date,P-value_FOI,P-value_Cases_week-1,P-value_Interaction, Dev_FOI, Dev_Cases_week-1, Dev_Interaction, Intercept, Coef_FOI, Coef_Cases-1, Coef_Inter, Pseudo-R2"))
i <- 2
j <- 1
for (i in 2:length(cases_Fr)) {
  poissondata <- left_join(cases_Fr[c(1,i, i+1)], FOI_Fr[c(2,i+1)], by= c("dep"="Location"))%>%
    na.omit() %>%
    merge(select(pop_Fr, c("dep", "Total")))
  resulttable_Fr[j,1] <- colnames(poissondata)[3]
  colnames(poissondata) <- c("county_id", "cases_week_0", "cases_week_1", "logFOI_week_0", "population")
  model <- glm(formula=cases_week_1~logFOI_week_0+cases_week_0+cases_week_0:logFOI_week_0,
               family="quasipoisson", 
               data=poissondata, 
               offset = log(population))
  resulttable_Fr[j,2] <- coef(summary(model))[2,4] #FOI
  resulttable_Fr[j,3] <- coef(summary(model))[3,4] #Cases
  resulttable_Fr[j,4] <- coef(summary(model))[4,4] #Interaction
  resulttable_Fr[j,5] <- anova(model)$Deviance[2]/anova(model)$'Resid. Dev'[1]*100
  resulttable_Fr[j,6] <- anova(model)$Deviance[3]/anova(model)$'Resid. Dev'[1]*100
  resulttable_Fr[j,7] <- anova(model)$Deviance[4]/anova(model)$'Resid. Dev'[1]*100
  resulttable_Fr[j,8] <- coef(model)["(Intercept)"]
  resulttable_Fr[j,9] <- coef(model)["logFOI_week_0"]
  resulttable_Fr[j,10] <- coef(model)["cases_week_0"]
  resulttable_Fr[j,11] <- coef(model)["logFOI_week_0:cases_week_0"]
  resulttable_Fr[j,12] <- with(summary(model), 1 - deviance/null.deviance)
  j=j+1
}

write.csv(resulttable_US, "./Quasipoisson/USA_model.csv")
write.csv(resulttable_Col, "./Quasipoisson/Colombia_model.csv")
write.csv(resulttable_Sw, "./Quasipoisson/Sweden_model.csv")
write.csv(resulttable_Fr, "./Quasipoisson/France_model.csv")

resulttable_US$country='Westcoast - USA'
resulttable_Col$country='Colombia'
resulttable_Sw$country='Sweden'
resulttable_Fr$country='France'

resulttable <- do.call("rbind", list(resulttable_US, resulttable_Col, resulttable_Sw, resulttable_Fr))

# SHARE OF DEVIANCE GRAPH
long <- melt(resulttable[c(1,3,4,5,10)], id.vars = c("Date", "country"))
long <- long[order(long$Date),]
long$variable <- factor(long$variable, levels = c("Var_Interaction", "Dev_Cases_week.1", "Dev_FOI"))
ggplot(long, aes(x=Date, y=value, group=variable)) + 
  geom_area(aes(fill = variable))+
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size=18, face="bold", hjust = 0.5))+
  ggtitle("Share (%) of deviance by variable")+
  facet_wrap(~ country)

# CASES PLUS P VALUE FOI
cases_US <- subset(cases_US, cases_US$countyFIPS %in% FOI_US$County)
rt_US <- cbind(resulttable_US, melt(colSums(cases_US[5:30]))[2:26,])%>%
  cbind(melt(colSums(mob_US[6:31]))[2:26,])
colnames(rt_US)[12] <- c("Total_cases")
colnames(rt_US)[13] <- c("Total_mobilities")
rt_US <- mutate(rt_US, Total_mobilities=Total_mobilities/max(rt_US$Total_mobilities))

rt_Col <- cbind(resulttable_Col, melt(colSums(cases_Col[3:25])))%>%
  cbind(melt(colSums(mob_Col[11:33])))
colnames(rt_Col)[12] <- c("Total_cases")
colnames(rt_Col)[13] <- c("Total_mobilities")
rt_Col <- mutate(rt_Col, Total_mobilities=Total_mobilities/max(rt_Col$Total_mobilities))

rt_Sw <- cbind(resulttable_Sw, melt(colSums(cases_Sw[3:21])))%>%
  cbind(melt(colSums(mob_Sw[3:21])))
colnames(rt_Sw)[12] <- c("Total_cases")
colnames(rt_Sw)[13] <- c("Total_mobilities")
rt_Sw <- mutate(rt_Sw, Total_mobilities=Total_mobilities/max(rt_Sw$Total_mobilities))

rt_Fr <- cbind(resulttable_Fr, melt(colSums(cases_Fr[3:25])))%>%
  cbind(melt(colSums(mob_Fr[13:35])))
colnames(rt_Fr)[12] <- c("Total_cases")
colnames(rt_Fr)[13] <- c("Total_mobilities")
rt_Fr <- mutate(rt_Fr, Total_mobilities=Total_mobilities/max(rt_Fr$Total_mobilities))

rt2 <- do.call("rbind", list(rt_US, rt_Col, rt_Sw, rt_Fr))
coeff <- max(rt2$Total_cases)
index_mob <- max(rt2$Total_mobilities)
ggplot(data=rt2, aes(Date, P.value_FOI*coeff, group=1))+
  scale_y_continuous("Cases", sec.axis = sec_axis(~./coeff,name = "P.value / Mobility index")) +
  geom_col(aes(y=Total_cases, group=1))+
  geom_line(aes(y=Total_mobilities*coeff), size=0.8, color = "blue")+
  geom_smooth(size= 0)+
  geom_line(size=0.8, color= "#26C680")+
  geom_point(color= "#0B3B26")+
  geom_hline(aes(yintercept=0.05*coeff), linetype="dashed", color = "red")+
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size=18, face="bold", hjust = 0.5))+
  ggtitle("P.value of the FOI week-1")+
  facet_wrap(~ country)

## COEFF
ggplot(data=rt2, aes(Date, group=1))+
  #geom_line(aes(y=Intercept), size=0.8, color = "green")+
  #scale_y_continuous("Cases", sec.axis = sec_axis(~./coeff,name = "P.value / Mobility index")) +
  #geom_col(aes(y=Total_cases, group=1))+
  #geom_line(aes(y=Total_mobilities*coeff), size=0.8, color = "blue")+
  geom_line(aes(y=Coef_FOI, color = "blue"), size=0.8)+
  geom_line(aes(y=Coef_Cases.1, color = "red"), size=0.8)+
  geom_line(aes(y=Coef_Inter, color = "purple"), size=0.8)+
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size=18, face="bold", hjust = 0.5), legend.position="right")+
  ggtitle("Quasi-Poisson Coefficients")+
  facet_wrap(~ country)

## QUASI R2
rt3 <- melt(rt2, id.var = c("Date", "country")) %>%
  #subset(variable==c("Coef_FOI", "Coef_Cases.1", "Coef_Inter", "Pseudo.R2"))%>%
  subset(variable==c("Pseudo.R2"))

ggplot(data=rt3, aes(x=Date, y=value, colour=country, group = country))+
  geom_hline(aes(yintercept=0.5), linetype="dashed", color = "red")+
  geom_line()+
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(size=18, face="bold", hjust = 0.5), legend.position="right")+
  ggtitle("Quasi-Poisson Pseudo R2")
