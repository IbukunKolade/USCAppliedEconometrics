library(dplyr)
library(reshape2)
library(haven)
library(tidyr)
library(readxl)
library(lubridate)



sheet_names <- excel_sheets("C:/Users/ibuku/OneDrive/ODDocuments/Macro 501/Project Data/inflation specific regions.xlsx")


sheet_list <- lapply(sheet_names, function(sheet) 
  read_excel("C:/Users/ibuku/OneDrive/ODDocuments/Macro 501/Project Data/inflation specific regions.xlsx", sheet = sheet))

names(sheet_list) <- sheet_names
list2env(sheet_list, envir = .GlobalEnv)

for (i in 1:length(sheet_list)) {
  sheet_list[[i]] <- melt(sheet_list[[i]], id.vars = "Year", variable.name = "Month", value.name = "CPI")
  sheet_list[[i]] <- drop_na(sheet_list[[i]],"CPI")
  sheet_list[[i]]$Month_Num <- month(parse_date_time(sheet_list[[i]]$Month, orders = c("b", "B")))
  sheet_list[[i]]$Date <- as.Date(paste(sheet_list[[i]]$Year, sheet_list[[i]]$Month_Num, 1, sep = "-"))
  repeated_vector <- rep(sheet_names[i], times = nrow(sheet_list[[i]]))
  sheet_list[[i]]$Region <- repeated_vector
  sheet_list[[i]] <- subset(sheet_list[[i]], select=-c(Year, Month, Month_Num))
  
  sheet_list[[i]] <- sheet_list[[i]] %>%
    arrange(Date) %>%  
    mutate(monthly_inflation = (CPI - lag(CPI)) / lag(CPI) * 100)
  
  sheet_list[[i]] <- subset(sheet_list[[i]], select=-c(CPI))
  sheet_list[[i]] <- drop_na(sheet_list[[i]],"monthly_inflation")
}

names(sheet_list) <- sheet_names
list2env(sheet_list, envir = .GlobalEnv)

CPI_Macro <- do.call(rbind, sheet_list)
rownames(CPI_Macro) <- NULL

write.csv(CPI_Macro, "CPI_Macro.csv")
