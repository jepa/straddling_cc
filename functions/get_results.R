#GetResults

# This function simply loads the results
GetResults=function(Spp,type = "strad"){
  
  if(type == "strad"){
    # Set the path for each file
    Distpath <- paste(my_path("R","Straddling_realm_rfmo/"),Spp,"_straddling.csv",sep="")
  }
  
  if(type == "ob"){
    # Set the path for each file
    Distpath <- paste(my_path("R","Straddling_realm_ob/"),Spp,"_straddling.csv",sep="")
  }
  if(type == "prop"){
    Distpath <- paste(my_path("R","Per_change_realm_rfmo/"),Spp,"_perchg.csv",sep="")
  }
  
  if(type == "prop_ob"){
    Distpath <- paste(my_path("R","Per_change_realm_ob/"),Spp,"_perchg.csv",sep="")
  }
  if(type == "rfmo"){
    Distpath <- paste(my_path("R","Per_change_rfmo_rfmo/"),Spp,"_perchg.csv",sep="")
  }
  
  # Loads all files in a df
  # Load_Data <- bind_rows(lapply(Distpath, FUN=fread))
  Load_Data <- fread(Distpath)
  
  if(nrow(Load_Data)>0){
    
    return(Load_Data)
  }
}
