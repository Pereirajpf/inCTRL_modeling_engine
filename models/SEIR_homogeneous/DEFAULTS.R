# Create default values as RData object for SEIR model
model_folder <- "models/SEIR_homogeneous"
#Start and end date===
time <- c(0,200)
#save(time, file = past0(model_folder,",/start_end_dates.RData")
#write.csv(time,past0(model_folder,",/start_end_dates.csv")


#Initial values for the compartments====
comparts <- c(S=500, E=0, I=1, R=0)
#save(comparts, file = past0(model_folder,",/compart_comparts.RData")
#write.csv(comparts, file = past0(model_folder,",/compart_comparts.csv")


#Parameters and breakpoints====
params_list <- list(timeline = c(0,50,as.numeric(time[length(time)])),
                    params = list(list(
                      beta = 0.8,
                      sigma = 0.3,
                      gamma = 0.2
                    ),
                    list(
                      beta = 0.5,
                      sigma = 0.3,
                      gamma = 0.2
                    ))
)

#Wrapper to use
wrapper <- NA
#save(params_list, file = past0(model_folder,",/parameters.RData")
#write.csv(params_list, file = past0(model_folder,",/parameters.csv")

save(time, comparts, params_list, wrapper, file = paste0(model_folder,"/defaults.RData"))
