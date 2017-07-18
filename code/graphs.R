#rm(list = ls())

setwd("/Users/bz72/Documents/Ed")

library(dplyr)
library(lubridate)
library(ggplot2)
library(devtools)
library(stringr)
library(tidyr)

# data <- read.csv("data.csv", as.is = T)
# 
# txt_num <- function(x){
#   as.numeric(gsub(",","",x))
# }
# 
# data <- data %>%
#   mutate(EVENT_DT = as.Date(EVENT_DT, format = "%m/%d/%y"),
#          SHIPPED_QUANTITY = txt_num(SHIPPED_QUANTITY),
#          IND_QTY = txt_num(IND_QTY),
#          LAX_QTY = txt_num(LAX_QTY),
#          DFW_QTY = txt_num(DFW_QTY),
#          PHL_QTY = txt_num(PHL_QTY),
#          ATL_QTY = txt_num(ATL_QTY),
#          MCO_QTY = txt_num(MCO_QTY),
#          Sales_Rev = txt_num(Sales_Rev),
#          Sales_Cost = txt_num(Sales_Cost))
# 
# save(data, file = "data.Rdata")

# [10:11 AM, 7/18/2017] Eduardo Candela: una super cool con un mapa de estados unidos estaría padre                        
# [10:11 AM, 7/18/2017] Eduardo Candela: entonces maybe como de número de ventas o profit o algo así en un mapa                        
# [10:12 AM, 7/18/2017] Eduardo Candela: también puedes hacer una bonita de serie de tiempo, o sea como profit vs time? pero que tenga varias en la misma gráfica, como las 6 lineas de profit vs time por lugar                        

load("data.Rdata")

data <- data %>%
  mutate(profit = Sales_Rev-Sales_Cost)

data_agg <- data %>%
  group_by(EVENT_DT) %>%
  summarise_at(funs(sum), .vars = vars(IND_QTY, LAX_QTY, PHL_QTY, ATL_QTY, DFW_QTY, MCO_QTY, SHIPPED_QUANTITY,
                 Sales_Rev, Sales_Cost, profit))

#Cost and Revenue time series

ggplot(data_agg, aes(x = EVENT_DT, y = Sales_Rev))+geom_line(colour = "blue") +
  geom_line(aes(x = EVENT_DT, y = Sales_Cost), colour = "red")

region <- data %>%
  select(EVENT_DT, IND_QTY, LAX_QTY, PHL_QTY, ATL_QTY, DFW_QTY, MCO_QTY) %>%
  gather(AIRPORT, COUNT, - EVENT_DT) %>%
  group_by(EVENT_DT, AIRPORT) %>%
  summarise(QTY = sum(COUNT))

ggplot(region, aes(x = EVENT_DT, y = QTY, color = as.factor(AIRPORT)))+geom_line()
  