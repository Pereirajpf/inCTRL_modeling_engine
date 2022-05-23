# Create default values as RData object for SEIR model
model_folder <- "models/SEIR_heterogeneous"

#Start and end date===
time <-  0
tmax <- 80

#Initial values for the compartments====
comparts <- c(S=c(300,200,100,50), E=c(0,0,0,0), I=c(1,2,3,4), R=c(0,0,0,0))

#Parameters and breakpoints====
params_list <- list(timeline = c(0,15,tmax),
                    params = list(list(
                      beta = 0.8,
                      sigma = 0.3,
                      gamma = 0.2
                    ),
                    list(
                      beta = 0.3
                    ))
)

#Wrapper to use
wrapper <- "hetero_model_wrapper"

save(time, comparts, params_list, wrapper, file = paste0(model_folder,"/defaults.RData"))
