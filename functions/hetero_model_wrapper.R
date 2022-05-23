# wrapper function do enable use of age groups in the model equations, as a heterogeneous model
hetero_model_wrapper<- function(time, comparts, parameters, model){
  #cat("comparts: ",comparts,"/")
  init_vals <- comparts
  #Remove numbers corresponding to age
  names(comparts) <- gsub("[[:digit:]]","",names(comparts))
  
  #Group by compartment
  var_matrix <- matrix(comparts, ncol = length(unique(names(comparts))))

  #transform in list of lists and add the names of the compartments
  var_matrix <- lapply(seq_len(ncol(var_matrix)), function(i) var_matrix[,i])
  names(var_matrix) <- unique(names(comparts))
  
  #RUN MODEL
  res <- model(time,var_matrix, parameters)[[1]]
  #give the corresponding column name, Compartiment + Age group
  names(res) <- names(init_vals)
  
  return(list(res))
}