#Importing the required libraries
library(readxl)
library(dplyr)
library(dotwhisker)
#Importing the data file with the survey results
survey <- read_excel("CHALLINEQ/Challineq_Geo_21apr2022.xlsx")
#Keeping only a certain number of variables of interest
survey <- survey[c("ID", "garden","shared_green","shared_green_price","income","house_independant",
           "house_status", "common_entry", "odour","incident","children_number","SC","ST","OBC","General",
           "Other","religion","colony", "euc_distance_park")]
#Reformat certain variables into binary values
survey$garden <- ifelse(survey$garden=="Yes",1,0) 
survey$shared_green <- ifelse(survey$shared_green=="Yes",1,0)
survey$house_independant <- ifelse(survey$house_independant=="Yes",1,0) 
survey$odour <- ifelse(survey$odour=="Yes",1,0) 
survey$incident <- ifelse(survey$incident=="Yes",1,0)  
survey$common_entry <- ifelse(survey$common_entry=="Common entry",1,0)
#Reformat certain variables into numeric values
survey$shared_green_price <- as.numeric(survey$shared_green_price)
survey$children_number <- as.numeric(survey$children_number)
survey[11:15] <- sapply(survey[11:15],as.numeric)
#Reformat certain variables as factors
survey$income <- as.factor(survey$income)
survey$income <- factor(survey$income, levels = c("Less than 2,000 Rs/month", 
                                 "2,000 to 6,000", 
                                 "6,000 to 10,000",
                                 "10,000 to 20,000",
                                 "20,000 to 30,000",
                                 "30,000 to 40,000",
                                 "40,000 to 50,000",
                                 "50,000 to 60,000",
                                 "60,000 to 70,000",
                                 "70,000 to 110,000",
                                 "110,000 and above"))
survey$colony <- as.factor(survey$colony)
survey$religion <- as.factor(survey$religion)
#Subset dataset removing some non-attributed values and outlier values
survey <- subset(survey, house_status != "NA")
survey <- subset(survey, survey$religion %in% c("Hindu", "Muslim", "Christian", "Sikh", "Jain"))
survey <- subset(survey, survey$ST == 0)
survey <- subset(survey, children_number < 12)
survey <- subset(survey, euc_distance_park < 5000)
#Restructuring the housing types and subseting to keep only the types with a sufficient size in the sample
survey[survey$house_status=="Delhi Government Quarters",]$house_status <- "Government Quarters"
survey[survey$house_status=="Central Government Quarters",]$house_status <- "Government Quarters"
survey[survey$house_status=="Railway Colony Quarters",]$house_status <- "Government Quarters"
survey[survey$house_status=="private housing",]$house_status <- "Private housing"
survey[survey$house_status=="private housing",]$house_status <- "Private housing"
survey[survey$house_status=="Department of Delhi Fire Services",]$house_status <- "Government Quarters"
survey$house_status <- as.factor(survey$house_status)
#survey$house_status <- droplevels(survey$house_status)
survey <- subset(survey, survey$house_status %in% c("Government Quarters", 
                                   "DDA 1 : group housing", 
                                   "DDA 2 : simple housing", 
                                   "Private housing", 
                                   "Private Resettlement", 
                                   "Slum 1 : unauthorized colony", 
                                   "Slum 2 : resettlement colony", 
                                   "Urban Village"))
#Building the caste attribute from several binary fields
survey$caste <- "General"
survey$caste <- ifelse(survey$SC==1,"SC",ifelse(survey$OBC==1,"OBC", "General"))
survey$caste <- as.factor(survey$caste)
#Building distance to greenspace threshold variables
survey$park250 <- ifelse(survey$euc_distance_park < 250,"1", "0")
survey$park500 <- ifelse(survey$euc_distance_park < 500,"1", "0")
survey$nopark500 <- ifelse(survey$euc_distance_park > 500,"1", "0")


#Plotting the share of declared access to green space or not by type of housing
survey %>%
  mutate(shared_green = as.character(shared_green)) %>%
  count(house_status, shared_green) %>%
  group_by(house_status) %>%
  mutate(lab = paste0(round(prop.table(n) * 100, 2), '%')) %>%
  ggplot(aes(house_status,n, fill=shared_green)) + 
  geom_col() +
  scale_fill_manual(name="Declared access to \na shared green space:",
                    labels = c("No access", "Access"),
                    values = c("LightGray", "MediumSeaGreen"))+
  scale_x_discrete(labels= c("DDA: group housing", "DDA: simple housing","Government quarters","Private housing", "Private Resettlement", "Slum: unauthorized", "Slum: resettlement", "Urban village"))+
  geom_text(aes(label=lab),position=position_stack(vjust = 0.5), size = 3) +
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"))

#Establishing the baselines values
survey <— survey %>%
  mutate(religion=relevel(religion,ref="Hindu")) %>%
  mutate(house_status=relevel(house_status,ref="Government Quarters")) %>%
  mutate(caste=relevel(caste, ref="General"))

#Fitting a regression model on the declared access to green space
fit_green <- glm(shared_green ~ house_status + children_number + odour + incident + caste + religion + nopark500, 
                 #+ nopark500:house_status, 
    data = na.omit(survey[c("shared_green","house_status","children_number","odour","incident","caste","religion","euc_distance_park", "park250", "park500", "nopark500")]), 
    family = binomial(link = "logit"))

#Computing the ROC Curve and AUC
prob = predict(fit_green, newdata = survey, type = "response")
auc = roc(survey$shared_green ~ prob, plot = TRUE, print.auc = TRUE)

#Plotting the regression results
dwplot(fit_green, show_intercept = FALSE,
       model_order = c("Logistic regression"),
       vline = geom_vline(
         xintercept = 0,
         colour = "grey60",
         linetype = 2
       )) +     
  ggtitle("Determinants of the declared access to a shared green space")+
  theme_minimal() +
  xlab("Coefficient Estimate (dot) with 95% Confidence Interval (segment)") + ylab("") +
  xlim(-3, 3) +
  geom_segment(aes(x=conf.low,y=term,xend=conf.high,
                   yend=term,colour=p.value<0.10))+
  geom_point(aes(x=estimate,y=term,colour=p.value<0.10,size=.5)) +
  geom_text(aes(x=estimate,y=term,
                label = ifelse((exp(estimate)>1),
                               paste("+",((round((exp(estimate)),2))-1)*100,"%", sep = ""),
                               paste(((round((exp(estimate)),2))-1)*100,"%", sep = ""))), 
            vjust=(-1), size=3.5)+
  scale_color_manual(values = c("gray","gray","3d9f88"))+
  theme(legend.position="right", 
        legend.background = element_rect(colour = "grey80"),
        plot.title = element_text(hjust = 0.5, face = "bold"))
