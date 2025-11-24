library(dplyr)
library(reshape2)
library(haven)
library(tidyr)

US_Monthly_GDP_Hist$Date <- as.Date(US_Monthly_GDP_Hist$Date, format = "%m/%d/%Y")
US_Pop_Thousands_$DATE <- as.Date(US_Pop_Thousands_$DATE, format = "%m/%d/%Y")
unemployment_data_by_region <- melt(unemployment_data_by_region, id.vars = "Date", 
                                    variable.name = "Region", value.name = "unemployment rate")

unemployment_data_by_region$Date <- as.Date(unemployment_data_by_region$Date, format = "%m/%d/%Y")

by <- join_by("Date", "Region")
result <- inner_join(CPI_Macro, unemployment_data_by_region, by)

Region_Wage_Growth_Macro <- subset(Region_Wage_Growth_Macro, select=-c(Overall))
df_melted <- melt(Region_Wage_Growth_Macro, id.vars = "Date", variable.name = "Region", value.name = "Wage Growth")

df_melted <- drop_na(df_melted, "Date")
head(df_melted)

df_melted$Date <- as.Date(df_melted$Date, format = "%m/%d/%Y")

by <- join_by("Date", "Region")
result <- inner_join(result,df_melted, by)

result$Date <- as.Date(result$Date, format = "%m/%d/%Y")

head(result)

result <- inner_join(result, US_Monthly_GDP_Hist, by = "Date")
by <- join_by("Date" == "DATE")
result <- inner_join(result, US_Pop_Thousands_, by)
result <- result[,2:8]

head(result)

colnames(result)[4] <- "Unemployment_Rate"
colnames(result)[5] <- "wage_growth"

result$monthly_inflation<- result$monthly_inflation/100
result$Unemployment_Rate<- result$Unemployment_Rate/100
result$wage_growth<- result$wage_growth/100

head(result)

result$real_wage_growth <- result$wage_growth - result$monthly_inflation
result$pre_2020 <- ifelse(result$Date < as.Date("2020-01-01"), 0, 1)
result$pre_mid_2020 <- ifelse(result$Date < as.Date("2020-07-01"), 0, 1)
result$post_2020 <- ifelse(result$Date < as.Date("2021-01-01"), 0, 1)

result$Region <- as.factor(result$Region)

head(result)

write_dta(result, "output4.dta")
write.csv(result, "output4.csv")

