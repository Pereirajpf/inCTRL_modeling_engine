#ModelEngine test script, using the SEIR heterogeneous model

#Load engine
source("functions/ModelEngine.R")

#Run SEIR model homogenious
seir_model_homog <- ModelEngine$new(model="SEIR_homogenious")
seir_model_homog
seir_model_homog$print(extended = T)
seir_model_homog$run()
seir_model_homog$set_args(time = c(2,250))
tail(seir_model_homog$result)
seir_model_homog$set_args(time = c(as.Date("2020-03-01"),as.Date(150,origin="2020-03-01")))
seir_model_homog$tmax
seir_model_homog$plot

#Change date
seir_model_homog$set_time(time = c(0,101))
seir_model_homog$run()
seir_model_homog$plot()


#DEBUG
source("functions/run_model.R")
res <- run_model(model = seir_model_homog$ODEs,
                 init_comparts = seir_model_homog$comparts,
                 params_list = seir_model_homog$params_list,
                 wrapper=NA)

plot(x=seir_model_homog$result$time,y=seir_model_homog$result$S,type="l")


#Run SEIR model heterogenious
seir_model_heteg <- ModelEngine$new(model="SEIR_heterogenious")
seir_model_heteg
seir_model_heteg$run()
tail(seir_model_heteg$result)
seir_model_heteg$plot()

#Run heterogenious COVID-19 Vacination in-CTRL model
test_inCTRL_heteg <- ModelEngine$new(model = "COVID19_inCTRL_Vac")
test_inCTRL_heteg
test_inCTRL_heteg$run()
res <- test_inCTRL_heteg$result
test_inCTRL_heteg$plot(compact = T)

#save results
save(res, file = "models/COVID19_inCTRL_Vac/results.RData")

res_comp <- test_inCTRL_heteg$result_compact()
save(res_comp, file = "models/COVID19_inCTRL_Vac/results_comp.RData")

library(ggplot2)
res_comp <- test_inCTRL_heteg$result_compact()
#0-4 Age group
ggplot2::ggplot(data = res) +
  geom_line(aes(x= time, y=M1),   colour=2) +
  geom_line(aes(x= time, y=M_v1), colour=4)

#75+
ggplot2::ggplot(data = res) +
  geom_line(aes(x= time, y=R16),   colour=2) +
  geom_line(aes(x= time, y=R_v16), colour=4)

              
