# Importing the data
library(readxl)
Colombia_Admin_2 <- read_excel("Colombia Admin 2.xlsx", sheet = "Movement")

# Extracting non-numeric data
Colombia_Admin_day <- Colombia_Admin_2[1:7]

# Loop to sum rows every 3 column (3 timesteps per day)
j = 8
i = 8
for (i in seq(from = 8, to = length(Colombia_Admin_2), by=3)){
  Colombia_Admin_day[j] <- rowSums(Colombia_Admin_2[c(i:(i+2))])
  # Refactoring the column names with the dates, and "/" separator
  colnames(Colombia_Admin_day)[j] <- substr(colnames(Colombia_Admin_2[i]), 2, nchar(colnames(Colombia_Admin_2[i])) - 5)
  colnames(Colombia_Admin_day)[j] <- gsub('\\.', "/", colnames(Colombia_Admin_day[j]))
  j = j + 1
}
# writing the ouput file in CSV
write.csv(Colombia_Admin_day, "./Colombia_Admin_day.csv")

Colombia_Admin_week <- Colombia_Admin_2[1:7]

j = 8
i = 8
for (i in seq(from = 8, to = length(Colombia_Admin_2), by=21)){
  Colombia_Admin_week[j] <- rowSums(Colombia_Admin_2[c(i:(i+20))])
  colnames(Colombia_Admin_week)[j] <- substr(colnames(Colombia_Admin_2[i]), 2, nchar(colnames(Colombia_Admin_2[i])) - 5)
  colnames(Colombia_Admin_week)[j] <- gsub('\\.', "/", colnames(Colombia_Admin_week[j]))
  j = j + 1
}

write.csv(Colombia_Admin_week, "./Colombia_Admin_week.csv")
