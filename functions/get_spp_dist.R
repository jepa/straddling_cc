# GetSppDistFun

# Function used to get species original distribution 

GetSppDist=function(Spp,Model,Coord){
  
  Data_Path <- my_path("G","FishForVisa")  
  Distpath <- paste(Data_Path,"Data/Distribution/",sep="")
  INDEX = seq(1,259200,1)
  # INDEX <- Coordinates$INDEX
  
  # SAU Distributions
  if(Model == "SAU_D"){
    File_Name <- paste("SAU_Distribution/DIST_GOOD_SP_")
  }
  
  # SAU Catch
  if(Model == "SAU_C"){
    File_Name <- paste("SAU_data_per_species/CATCH_SP_")
  }
  
  # Occurence
  if(Model == "Occ"){
    File_Name <- paste("Occurence/OCCURENCE_JULIANO_")
  }
  
  # ENM Model
  if(Model == "ENM"){
    File_Name <- paste("ENM/ENM_JULIANO_")
  }
  
  
  
  if(Model == "All"){
    
    Models_List <- c(paste(Distpath,"SAU_Distribution/DIST_GOOD_SP_",Spp,".mat",sep =""),
                     paste(Distpath,"Occurence/OCCURENCE_JULIANO_",Spp,".mat",sep=""),
                     paste(Distpath,"ENM/ENM_JULIANO_",Spp,".mat",sep="")
    )
    
    # Jumps species not modeled. NOTE: We only need one as ENM, Occ and SAU Dis have all the same 939 spp
    if(file.exists(Models_List[1])){
      
      Load <- lapply(Models_List, FUN=R.matlab::readMat, na.strings=0)
      
      sppdist <- as.data.frame(bind_cols(Load)) %>% 
        mutate(
          INDEX = INDEX,
          TaxonKey = Spp
        )
      colnames(sppdist) <- c("SAU_D","Occ","ENM","INDEX","TaxonKey")
      
      #### Step for SAU catch data that has to be averaged
      File_Name <- paste("SAU_data_per_species/CATCH_SP_")
      SppPath <- paste(Distpath,File_Name,Spp,".mat",sep="")
      
      # For now we're using only the last 10 years average, basicaaly if the species has been fished in any of these years, its considered present
      SAU_C_data <- as.data.frame(R.matlab::readMat(SppPath)) %>% 
        select(CATCH.1:CATCH.65) %>% 
        mutate(INDEX = INDEX) %>% 
        gather("Year","Catch",CATCH.56:CATCH.65) %>% # Last 10 years of data
        group_by(INDEX) %>% 
        summarise(SAU_C = mean(Catch,na.rm=T))
      
      # Join both tables
      sppdist <- sppdist %>% 
        left_join(SAU_C_data,
                  by = "INDEX") %>% 
        select(TaxonKey,INDEX,everything()) %>% 
        left_join(CoorG,
                  by = "INDEX")
      
      # Fix coordinate system incompatibility between Gab and DBEM
      sppdist <- suppressWarnings(sppdist[order(sppdist$latitude, rev(sppdist$longitude),decreasing=TRUE), ] %>% 
                                    mutate(INDEX = seq(1,259200,1)) %>% 
                                    gather("Model","Value",3:6) %>%
                                    mutate(Value = ifelse(is.na(Value), 0, Value)) # Converting NA's to ceros
      )
      
      getSppDist = sppdist
      
    }
    
  }else{  
    
    # Merge paths
    SppPath <- paste(Distpath,File_Name,Spp,".mat",sep="")
    
    if(file.exists(SppPath) == TRUE){
      
      #Install (if needed) R.matlab package
      if(!require(R.matlab)){
        install.packages("R.matlab")
      }
      
      # Read Files
      
      sppdist <- as.data.frame(R.matlab::readMat(SppPath)) %>% 
        mutate(INDEX = INDEX,
               Species = Spp) %>% 
        left_join(Coord) 
      
      # Fix coordinate system incompatibility between Gab and DBEM
      sppdist <- sppdist[order(sppdist$latitude, rev(sppdist$longitude),decreasing=TRUE), ] %>% 
        mutate(INDEX = seq(1,259200,1))
      
      # Return 
      getSppDist=sppdist
      
    }else{
      print(paste("No info for this species",Spp, "in",Model))
    }
    
  }
}