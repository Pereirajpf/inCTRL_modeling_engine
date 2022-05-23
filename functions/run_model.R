# Funtion to run selected model
run_model <- function(model,init_comparts,params_list,wrapper=NA,time_prec=1,method="ode23"){
  library(deSolve)
  
  #Timeline, periods to be separately simulated
  timeline <- params_list$timeline
  time_max <- timeline[length(timeline)]

  #Time vector to be suplied to the model
  times <- seq(0, time_max, length.out=time_max/time_prec+1)

  #Initialize df
  df_model <- as.data.frame(c(time = 0, as.list(init_comparts)))

  #Use wrapper function if supplied
  if (!is.function(wrapper)) {
    #Iterate over timeline periods
    for (i in seq_along(timeline[-1])) {
      seir_values <- ode(
        y = array(unlist(tail(df_model[-1],1)),dimnames = list(colnames(df_model[-1]))),
        times = subset(times,times<=timeline[i+1] & times>=timeline[i]),
        func = model,
        parms = params_list$params[[i]],
        method = method
      )
    df_model <- rbind(df_model,seir_values[-1,])
    }
  } else {
    #Iterate over timeline periods
    for (i in seq_along(timeline[-1])) {
      seir_values <- ode(
        y = array(unlist(tail(df_model[-1],1)),dimnames = list(colnames(df_model[-1]))),
        times = subset(times,times<=timeline[i+1] & times>=timeline[i]),
        func = wrapper,
        parms = params_list$params[[i]],
        model = model,
        method = method
      )
      df_model <- rbind(df_model,seir_values[-1,])
    }
  }

 df_model 
}
