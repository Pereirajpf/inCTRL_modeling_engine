# COVID-19 modeling
#using the COVID-19 model

# COVID-19 modeling
#Load engine
source("functions/ModelEngine.R")

library(dplyr)

#===============================================================================
#Covid-19 initial outbreak modeling
#===============================================================================

#Scenario 0 - no intevention
inCTRL_model_S0 <- ModelEngine$new(model="COVID19_inCTRL",default_file = "models/COVID19_inCTRL/Scenario0.RData")

inCTRL_model_S0$run()
res_S0 <- inCTRL_model_S0$result_compact()
#add date
res_S0$date <-  as.Date(res_S0$time, origin=inCTRL_model_S0$start_date)
#cut data
res_S0_new <- dplyr::filter(res_S0,res_S0$date >= as.Date("2021-01-01"))
#plots
inCTRL_model_S0$plot(compact = T, filter = c("H","ICU"))
inCTRL_model_S0$plot(compact = F, filter = c("S"))
inCTRL_model_S0$plot(compact = F, filter = c("H"))
inCTRL_model_S0$plot(compact = F, filter = c("ICU"))
inCTRL_model_S0$plot(compact = F, filter = c("R"))

#Scenario 1 - 1 month school closure and lockdown
inCTRL_model_S1 <- ModelEngine$new(model="COVID19_inCTRL",default_file = "models/COVID19_inCTRL/Scenario1.RData")

inCTRL_model_S1$run()
res_S1 <- inCTRL_model_S1$result_compact()
#add date
res_S1$date <-  as.Date(res_S1$time, origin=inCTRL_model_S1$start_date)
#cut data
res_S1 <- dplyr::filter(res_S1,res_S1$date >= as.Date("2021-01-01"))
#plots
inCTRL_model_S1$plot(compact = T, filter = c("H","ICU"))
inCTRL_model_S1$plot(compact = F, filter = c("S"))
inCTRL_model_S1$plot(compact = F, filter = c("H"))
inCTRL_model_S1$plot(compact = F, filter = c("ICU"))
inCTRL_model_S1$plot(compact = F, filter = c("R"))

#Scenario 2 - 45 days school closure and lockdown
inCTRL_model_S2 <- ModelEngine$new(model="COVID19_inCTRL",default_file = "models/COVID19_inCTRL/Scenario2.RData")

inCTRL_model_S2$run()
res_S2 <- inCTRL_model_S2$result_compact()
#add date
res_S2$date <-  as.Date(res_S2$time, origin=inCTRL_model_S2$start_date)
#cut data
res_S2 <- dplyr::filter(res_S2,res_S2$date >= as.Date("2021-01-01"))
#plots
inCTRL_model_S2$plot(compact = T, filter = c("H","ICU"))
inCTRL_model_S2$plot(compact = F, filter = c("S"))
inCTRL_model_S2$plot(compact = F, filter = c("H"))
inCTRL_model_S2$plot(compact = F, filter = c("ICU"))
inCTRL_model_S2$plot(compact = F, filter = c("R"))

#Scenario 3 - 2 month school closure and lockdown
inCTRL_model_S3 <- ModelEngine$new(model="COVID19_inCTRL",default_file = "models/COVID19_inCTRL/Scenario3.RData")

inCTRL_model_S3$run()
res_S3 <- inCTRL_model_S3$result_compact()
#add date
res_S3$date <-  as.Date(res_S3$time, origin=inCTRL_model_S3$start_date)
#cut data
res_S3 <- dplyr::filter(res_S3,res_S3$date >= as.Date("2021-01-01"))
#plots
inCTRL_model_S3$plot(compact = T, filter = c("H","ICU"))
inCTRL_model_S3$plot(compact = F, filter = c("S"))
inCTRL_model_S3$plot(compact = F, filter = c("H"))
inCTRL_model_S3$plot(compact = F, filter = c("ICU"))
inCTRL_model_S3$plot(compact = F, filter = c("R"))


#PLOTS==========================================================================
library(ggplot2)

#ICU plot
COVID19_inCTRL_ICU_plot <- ggplot() + 
  theme_bw() +
  geom_line(aes(x = res_S0$date, y = res_S0$ICU, group = 1, colour = "no intervention"), size=1)+
  geom_line(aes(x = res_S1$date, y = res_S1$ICU, group = 2, colour = "1 month lockdown"), size=1) +
  geom_line(aes(x = res_S2$date, y = res_S2$ICU, group = 3, colour = "45 days lockdown"), size=1)+
  geom_line(aes(x = res_S3$date, y = res_S3$ICU, group = 4, colour = "2 months lockdown"), size=1)+
  #start lockdown and school closure
  geom_vline(xintercept = as.Date("2021-01-15"), linetype="dotted", color = "red1", size=0.8, alpha=0.8) +
  geom_vline(xintercept = as.Date("2021-01-22"), linetype="dotted", color = "red2", size=0.8, alpha=0.8) +
  #Scenario lockdown ends
  geom_vline(xintercept = as.Date("2021-02-22"), linetype="dashed", color = "lemonchiffon4", size=0.8, alpha=0.8) +
  geom_vline(xintercept = as.Date("2021-03-09"), linetype="dashed", color = "goldenrod2", size=0.8, alpha=0.8) +
  geom_vline(xintercept = as.Date("2021-03-15"), linetype="dashed", color = 'deepskyblue3', size=0.8, alpha=0.8) +
  
  scale_x_date(date_breaks="1 month", date_labels = "%b %y") +
  labs(y="ICU ocupancy", x=NULL) +
  scale_color_manual(name='Scenarios',
                     breaks=c("no intervention", "1 month lockdown" , "45 days lockdown", "2 months lockdown"),
                     values=c('no intervention'='red3', '1 month lockdown'="lemonchiffon4", '45 days lockdown'="goldenrod2", '2 months lockdown'='deepskyblue3')) +
  theme(legend.position = c(1-0.12, 0.86),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.key.size = unit(0.2, 'cm'),
        legend.text = element_text(size=8),
        legend.title = element_text(size=10),
        axis.text.x = element_text(angle = 45, hjust=1, size = 8)) +
  scale_y_continuous(breaks = seq(0, 10000, 1000))

COVID19_inCTRL_ICU_plot

#save in save folder
ggsave(
  'saves/COVID19_inCTRL/Scenarios_ICU.jpg',
  plot = COVID19_inCTRL_ICU_plot,
  width = 16,
  height = 09,
  units = "cm",
  dpi = 600
)
