#SEIR heterogenious model example

source("functions/ModelEngine.R")

#initializing the model
SEIR_model <- ModelEngine$new(model="SEIR_heterogeneous")

#run the model
SEIR_model$run()

#results of the model
View(SEIR_model$result)

#plot the results
SEIR_model$plot()


#Sequencial modeling====
SEIR_model2 <- ModelEngine$new(model="SEIR_heterogeneous", load_default = F)

#starting date
start_date <- as.Date("2020-03-01")

#End after 80 days
tmax = 80

#initial compartiment values
init_comparts <- c(S=c(300,200,100,50), E=c(0,0,0,0), I=c(1,2,3,4), R=c(0,0,0,0))

#parameter list, includes the timeline and parameters
parameters_list <- list(timeline = c(0,10,tmax),
                    params = list(list(
                      beta = 0.8,
                      sigma = 0.3,
                      gamma = 0.2
                    ),
                    list(
                      beta = 0.2
                    ))
)

#manually load wrapper function to user as a function
source("functions/hetero_model_wrapper.R")

#input parameters in the model
SEIR_model2$set_args(start_date = start_date,
                     comparts = init_comparts,
                     params_list = parameters_list,
                     wrapper = hetero_model_wrapper)

#model object informations
SEIR_model2$print()

#replace the start date
SEIR_model2$set_args(start_date = as.Date("2020-03-11"), replace = T)

#add new breakpoint
SEIR_model2$add_breakpoint(time = as.Date("2020-04-20"),
                           params = list(beta = 0.8))

#check in variable if breakpoint parameters was inserted
SEIR_model2$params_list$params[[3]]

#NOT WORKING
#SEIR_model2$remove_breakpoint(time = as.Date("2020-04-24"))

#run the model
SEIR_model2$run()

head(SEIR_model2$result_compact())

SEIR_model2$plot(compact = F,filter = "I")



