#save inCTRLEngine function

#Save object
save_inCTRLEngine = function(obj, name, folder = "saves", overwrite = F){
  stopifnot(class(obj)[1] == "ModelEngine")
  stopifnot(is.character(name))
  
  #Check if file already exists in folder
  if(file.exists(paste0(folder,"/",name,".Rdata")) & !isTRUE(overwrite) ){
    stop(paste0(folder,"/",name,".Rdata already exists, use option overwrite = T"))
  } 
  
  saveRDS(obj, file = paste0("saves/",name,".RData"))
}