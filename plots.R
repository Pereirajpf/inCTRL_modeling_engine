# Plots
library(readr)
library(tidyverse)
library(ggthemes)

#Load results
load("models/COVID19_inCTRL_Vac/results.Rdata")
load("models/COVID19_inCTRL_Vac/results_comp.Rdata")

#Resultados Constantino
const_res <- read.csv("models/COVID19_inCTRL_Vac/table_output.csv",sep = ";")

#Plot constantino
plotConst = function(data_long, age_group){
  
  data_long %>%
    filter(agegroup==age_group) %>%
    ggplot(aes(x=time,y=value,color=vaccination))+
    geom_line()+
    facet_wrap(~compartment,scales = "free",labeller = labeller(compartment = labels))+
    theme_gdocs(base_size = 6)+
    labs(title=paste0(age_group," anos"))#+
  #ggsave(filename = "convida.jpg",width=12,height=8)
}

chartr("_v",".v_",res_pivot$name)

res_pivot <- res %>% pivot_longer(-time)
res_pivot$name1 <-  gsub("[[:digit:]]","",res_pivot$name)
res_pivot$group <-  gsub("[^[:digit:]]","",res_pivot$name)
res_pivot$name1 <- chartr("_v",".v",res_pivot$name1)
  
res_pivot %>% 
  mutate(vacination = if_else(endsWith(name1,".v"),true = "V",false = "NV"))

for (i in seq(0,16)) {
  chartr(as.character(i),unique(const_res$agegroup)[i],res_pivot$group)
}


#My plots
#0-4 Age group
ggplot2::ggplot(data = res) +
  geom_line(aes(x= time, y=M1),   colour=2) +
  geom_line(aes(x= time, y=M_v1), colour=4)

#75+
ggplot2::ggplot(data = res) +
  geom_line(aes(x= time, y=R16),   colour=2) +
  geom_line(aes(x= time, y=R_v16), colour=4)


#Comparing

plotConst(const_res,"0 - 4")
