# COVID-19 modeling
#using the COVID-19 Vaccination model

#Load engine
source("functions/ModelEngine.R")

library(dplyr)
#S1 scenario====
inCTRLVac_model_S1 <- ModelEngine$new(model="COVID19_inCTRL_Vac",default_file = "models/COVID19_inCTRL_Vac/S1_scenario.RData")
#inCTRLVac_model_S1$print()

inCTRLVac_model_S1$run()
S1_result <- inCTRLVac_model_S1$result
S1_result_comp <- inCTRLVac_model_S1$result_compact()
S1_result_comp$date <-  as.Date(S1_result_comp$time, origin=inCTRLVac_model_S1$start_date)
#Plots
inCTRLVac_model_S1$plot(compact = T, filter = c("H","ICU"))
inCTRLVac_model_S1$plot(compact = F, filter = c("S"))
inCTRLVac_model_S1$plot(compact = F, filter = c("H"))
inCTRLVac_model_S1$plot(compact = F, filter = c("ICU"))
inCTRLVac_model_S1$plot(compact = F, filter = c("R"))


#S2 scenario====
inCTRLVac_model_S2 <- ModelEngine$new(model="COVID19_inCTRL_Vac",default_file = "models/COVID19_inCTRL_Vac/S2_scenario.RData")
#inCTRLVac_model_S2$print()

inCTRLVac_model_S2$run()
S2_result <- inCTRLVac_model_S2$result
S2_result_comp <- inCTRLVac_model_S2$result_compact()
S2_result_comp$date <-  as.Date(S2_result_comp$time, origin=inCTRLVac_model_S2$start_date)
#Plots
inCTRLVac_model_S2$plot(compact = T, filter = c("H","ICU"))
inCTRLVac_model_S2$plot(compact = F, filter = c("S"))
inCTRLVac_model_S2$plot(compact = F, filter = c("H"))
inCTRLVac_model_S2$plot(compact = F, filter = c("ICU"))
inCTRLVac_model_S2$plot(compact = F, filter = c("R"))


#S2 booster scenario====
inCTRLVac_model_S2booster <- ModelEngine$new(model="COVID19_inCTRL_Vac",default_file = "models/COVID19_inCTRL_Vac/S2booster_scenario.RData")
#inCTRLVac_model_S2booster$print()

inCTRLVac_model_S2booster$run()
S2booster_result <- inCTRLVac_model_S2booster$result
S2booster_result_comp <- inCTRLVac_model_S2booster$result_compact()
#add dates
S2booster_result_comp$date <-  as.Date(S2booster_result_comp$time, origin=inCTRLVac_model_S2$start_date)
#Plots
inCTRLVac_model_S2booster$plot(compact = T, filter = c("H","ICU"))
inCTRLVac_model_S2booster$plot(compact = F, filter = c("S"))
inCTRLVac_model_S2booster$plot(compact = F, filter = c("H"))
inCTRLVac_model_S2booster$plot(compact = F, filter = c("ICU"))
inCTRLVac_model_S2booster$plot(compact = F, filter = c("R"))


#S3 scenario====
inCTRLVac_model_S3 <- ModelEngine$new(model="COVID19_inCTRL_Vac",default_file = "models/COVID19_inCTRL_Vac/S3_scenario.RData")
#inCTRLVac_model_S3$print()

inCTRLVac_model_S3$run()
S3_result <- inCTRLVac_model_S3$result
S3_result_comp <- inCTRLVac_model_S3$result_compact()
#add date
S3_result_comp$date <-  as.Date(S3_result_comp$time, origin=inCTRLVac_model_S3$start_date)
#Plots
inCTRLVac_model_S3$plot(compact = T, filter = c("H","ICU"))
inCTRLVac_model_S3$plot(compact = F, filter = c("S"))
inCTRLVac_model_S3$plot(compact = F, filter = c("H"))
inCTRLVac_model_S3$plot(compact = F, filter = c("ICU"))
inCTRLVac_model_S3$plot(compact = F, filter = c("R"))


#S3 booster scenario====
inCTRLVac_model_S3booster <- ModelEngine$new(model="COVID19_inCTRL_Vac",default_file = "models/COVID19_inCTRL_Vac/S3booster_scenario.RData")
#inCTRLVac_model_S3booster$print()

inCTRLVac_model_S3booster$run()
S3booster_result <- inCTRLVac_model_S3booster$result
S3booster_result_comp <- inCTRLVac_model_S3booster$result_compact()
#add dates
S3booster_result_comp$date <-  as.Date(S3booster_result_comp$time, origin=inCTRLVac_model_S3$start_date)
#Plots
inCTRLVac_model_S3booster$plot(compact = T, filter = c("H","ICU"))
inCTRLVac_model_S3booster$plot(compact = F, filter = c("S"))
inCTRLVac_model_S3booster$plot(compact = F, filter = c("H"))
inCTRLVac_model_S3booster$plot(compact = F, filter = c("ICU"))
inCTRLVac_model_S3booster$plot(compact = F, filter = c("R"))


#PLOTS==========================================================================
library(ggplot2)

#Hospitalization (non-ICU plot)
COVID19_inCTRL_Vac_Hosp_plot <- ggplot() + 
  theme_bw() +
  geom_line(aes(x = S1_result_comp$date, y = (S1_result_comp$H + S1_result_comp$H_v), group = 1, colour = "no wane"), size=1)+
  geom_line(aes(x = S2_result_comp$date, y = (S2_result_comp$H + S2_result_comp$H_v), group = 2, colour = "1 year" ), size=1) +
  geom_line(aes(x = S3_result_comp$date, y = (S3_result_comp$H + S3_result_comp$H_v), group = 3, colour = "3 years"), size=1)+
  #booster shot
  geom_line(aes(x = S2booster_result_comp$date, y = (S2booster_result_comp$H + S2booster_result_comp$H_v), group = 4, colour = "1 year"), size=1,linetype=3) +
  geom_line(aes(x = S3booster_result_comp$date, y = (S3booster_result_comp$H + S3booster_result_comp$H_v), group = 5, colour = "3 years"     ), size=1,linetype=3) +
  scale_x_date(date_breaks="1 month", date_labels = "%b %y") +
  labs(y="non-ICU ocupancy", x=NULL) +
  scale_color_manual(name='Scenarios',
                     breaks=c("no wane", "1 year" , "3 years"),
                     values=c('no wane'='green4', '1 year'="royalblue", '3 years'="red3")) +
  theme(legend.position = c(0.09, 0.86),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.key.size = unit(0.2, 'cm'),
        legend.text = element_text(size=8),
        legend.title = element_text(size=10),
        axis.text.x = element_text(angle = 45, hjust=1, size = 8)
        ) +
  scale_y_continuous(breaks = seq(0, 100000, 2500))
#save in save folder
ggsave(
  'saves/COVID19_inCTRL_Vac/Hosp_plot.jpg',
  plot = COVID19_inCTRL_Vac_Hosp_plot,
  width = 16,
  height = 09,
  units = "cm",
  dpi = 600
)


#ICU
COVID19_inCTRL_Vac_ICU_plot <- ggplot() + 
  theme_bw() +
  geom_line(aes(x = S1_result_comp$date, y = (S1_result_comp$ICU + S1_result_comp$ICU_v), colour = "no wane"), size=1)+
  geom_line(aes(x = S2_result_comp$date, y = (S2_result_comp$ICU + S2_result_comp$ICU_v), colour = "1 year" ), size=1) +
  geom_line(aes(x = S3_result_comp$date, y = (S3_result_comp$ICU + S3_result_comp$ICU_v), colour = "3 years"), size=1)+
  #booster sh
  geom_line(aes(x = S2booster_result_comp$date, y = (S2booster_result_comp$ICU + S2booster_result_comp$ICU_v), colour = "1 year" ), size=1,linetype=3) +
  geom_line(aes(x = S3booster_result_comp$date, y = (S3booster_result_comp$ICU + S3booster_result_comp$ICU_v), colour = "3 years"),  size=1,linetype=3) +
  scale_x_date(date_breaks="1 month", date_labels = "%b %y") +
  labs(y="ICU ocupancy", x=NULL) +
  scale_color_manual(name='Scenarios',
                     breaks=c("no wane", "1 year" , "3 years"),
                     values=c('no wane'='green4', '1 year'="royalblue", '3 years'="red3")) +
  theme(legend.position = c(0.09, 0.86),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.key.size = unit(0.2, 'cm'),
        legend.text = element_text(size=8),
        legend.title = element_text(size=10),
        axis.text.x = element_text(angle = 45, hjust=1, size = 8)
  ) +
  scale_y_continuous(breaks = seq(0, 10000, 250))
#save in save folder
ggsave(
  'saves/COVID19_inCTRL_Vac/ICU_plot.jpg',
  plot = COVID19_inCTRL_Vac_ICU_plot,
  width = 16,
  height = 09,
  units = "cm",
  dpi = 600
)

#M
COVID19_inCTRL_Vac_M_plot <- ggplot() + 
  theme_bw() +
  geom_line(aes(x = S1_result_comp$date, y = (S1_result_comp$M + S1_result_comp$M_v), colour = "no wane"), size=1)+
  geom_line(aes(x = S2_result_comp$date, y = (S2_result_comp$M + S2_result_comp$M_v), colour = "1 year" ), size=1) +
  geom_line(aes(x = S3_result_comp$date, y = (S3_result_comp$M + S3_result_comp$M_v), colour = "3 years"), size=1)+
  #booster sh
  geom_line(aes(x = S2booster_result_comp$date, y = (S2booster_result_comp$M + S2booster_result_comp$M_v), colour = "1 year" ), size=1,linetype=3) +
  geom_line(aes(x = S3booster_result_comp$date, y = (S3booster_result_comp$M + S3booster_result_comp$M_v), colour = "3 years"),  size=1,linetype=3) +
  scale_x_date(date_breaks="1 month", date_labels = "%b %y") +
  labs(y="Fatalities", x=NULL) +
  scale_color_manual(name='Scenarios',
                     breaks=c("no wane", "1 year" , "3 years"),
                     values=c('no wane'='green4', '1 year'="royalblue", '3 years'="red3")) +
  theme(legend.position = c(0.09, 0.86),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.key.size = unit(0.2, 'cm'),
        legend.text = element_text(size=8),
        legend.title = element_text(size=10),
        axis.text.x = element_text(angle = 45, hjust=1, size = 8)
  ) +
  scale_y_continuous(breaks = seq(0, 100000, 10000))
#save in save folder
ggsave(
  'saves/COVID19_inCTRL_Vac/M_plot.jpg',
  plot = COVID19_inCTRL_Vac_M_plot,
  width = 16,
  height = 09,
  units = "cm",
  dpi = 600
)
