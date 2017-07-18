#rm(list = ls())

setwd("/Users/bz72/Documents/Ed")

library(dplyr)
library(lubridate)
library(ggplot2)
library(devtools)
library(stringr)

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
