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

############# GEOGRAPHIC ANALYSIS #############

region <- data %>%
  select(EVENT_DT, IND_QTY, LAX_QTY, PHL_QTY, ATL_QTY, DFW_QTY, MCO_QTY) %>%
  gather(AIRPORT, COUNT, - EVENT_DT) %>%
  mutate(region = as.factor(gsub("_QTY","",AIRPORT)))%>%
  group_by(EVENT_DT, region) %>%
  summarise(QTY = sum(COUNT))

# time series by region

ggplot(region, aes(x = EVENT_DT, y = QTY, color = region))+geom_line()+
  facet_grid(region~.)+guides(color = F) + ylab("Quantity")+xlab("Date")+
  scale_x_date(date_breaks = "2 day", date_labels = "%d %Y")+
  ggtitle("Título")+scale_color_manual(values=c('#007dc6','#78b9e7','#76c043','#367c2b','#ffc220','#f47321','#004c91'))
 
#stacked bar

ggplot(region,aes(x = EVENT_DT, y = QTY, fill = region))+
  geom_bar(stat="identity",position='fill')+
  scale_x_date(date_breaks = "3 day", date_labels = "%d %Y")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values=c('#007dc6','#78b9e7','#76c043','#367c2b','#ffc220','#f47321','#004c91'))+
  xlab('Date')+ggtitle('Distribution of units sold among regions')+ylab('')+
  theme(
    plot.title = element_text( size=14, face="bold",hjust=0.5))  

ggplot(region,aes(x = EVENT_DT, y = QTY, fill = region))+
  geom_bar(stat="identity",position='stack')+
  scale_x_date(date_breaks = "3 day", date_labels = "%d %Y")+
  #scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values=c('#007dc6','#78b9e7','#76c043','#367c2b','#ffc220','#f47321','#004c91'))+
  xlab('Date')+ggtitle('Distribution of units sold among regions')+ylab('')+
  theme(
    plot.title = element_text( size=14, face="bold",hjust=0.5))  

############# ANALYSIS BY DEPARTMENT #############

super.dpt_agg <- data %>%
  group_by(EVENT_DT, SUPER_DEPT) %>%
  summarise(SHIPPED_QTY = sum(SHIPPED_QUANTITY),
            REV = sum(Sales_Rev),
            COST = sum(Sales_Cost))%>%
  ungroup() %>%
  mutate(PROFIT = REV-COST,
         SUPER_DEPT = as.factor(SUPER_DEPT))

# time series by region

ggplot(super.dpt_agg, aes(x = EVENT_DT, y = SHIPPED_QTY, color = SUPER_DEPT))+geom_line()+
  facet_grid(SUPER_DEPT~., scales = "free")+guides(color = F) + ylab("Quantity")+xlab("Date")+
  scale_x_date(date_breaks = "2 day", date_labels = "%d %Y")+
  ggtitle("Título")+scale_color_manual(values=c('#007dc6','#78b9e7','#76c043','#367c2b','#ffc220','#f47321','#004c91'))

#si quieres que el y axis esté en la misma escala quita el scales = "free" en facet_grid

#stacked bar

ggplot(region,aes(x = EVENT_DT, y = QTY, fill = region))+
  geom_bar(stat="identity",position='fill')+
  scale_x_date(date_breaks = "3 day", date_labels = "%d %Y")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values=c('#007dc6','#78b9e7','#76c043','#367c2b','#ffc220','#f47321','#004c91'))+
  xlab('Date')+ggtitle('Distribution of units sold among regions')+ylab('')+
  theme(
    plot.title = element_text( size=14, face="bold",hjust=0.5))  

ggplot(region,aes(x = EVENT_DT, y = QTY, fill = region))+
  geom_bar(stat="identity",position='stack')+
  scale_x_date(date_breaks = "3 day", date_labels = "%d %Y")+
  #scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values=c('#007dc6','#78b9e7','#76c043','#367c2b','#ffc220','#f47321','#004c91'))+
  xlab('Date')+ggtitle('Distribution of units sold among regions')+ylab('')+
  theme(
    plot.title = element_text( size=14, face="bold",hjust=0.5))  

