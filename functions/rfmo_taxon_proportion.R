
# This function estimate proportion change between RFMOs similar to `taxon_proportion.R`

# NOTES:
# requires: rfmo_neighbour data 
# paths embeded in code

rfmo_taxon_proportion <- function(taxon_key){
  
  print(taxon_key)
  
  stradd_data <- GetResults(taxon_key)
  # category <- unique(stradd_data$cat)
  
  rfmo_areas <- stradd_data %>% 
    pull(rfmo_name) %>% 
    unique()
  
  files_path <-list.files(my_path("G","dbem/dbem_cmip6/r_data"),full.names = T)[-3] # remove RCP45B
  
  to_read <- paste0(taxon_key,"Abd.RData")
  
  for(m in 1:6){
    # m = 1
    #################################
    # List esm folders
    file_to_read <- paste0(files_path[m],"/",to_read)
    
    if(file.exists(file_to_read)){
      
      load(file_to_read)
      
      
      if(exists("sppabdfnl")){
        spp_data <- as.data.frame(sppabdfnl) %>% 
          rowid_to_column("index")
        colnames(spp_data) <- c("index", seq(1951, 2100, 1))
        rm(sppabdfnl) 
      }else{
        spp_data <- as.data.frame(data) %>% 
          rowid_to_column("index")
        colnames(spp_data) <- c("index", seq(1951, 2100, 1))
        rm(data) 
      }
      
      
      # Get amount abundance per region
      spp_data_area <- spp_data %>%
        gather("year","value",`1951`:`2100`) %>%
        left_join(rfmo_index,
                  by = c("index"
                  ),
                  relationship = "many-to-many"
        ) %>% 
        filter(rfmo_name %in% rfmo_areas) %>%
        group_by(year,rfmo_name) %>% 
        summarise(total_rfmo_abd = sum(value,na.rm = T),
                  .groups = "drop") %>% 
        mutate(taxon_key = as.integer(taxon_key))
      
      # Estimate proportions of each region
      
      # RFMO values
      # rfmo_values <- spp_data_area %>% 
      #   filter(name %in% stradd_data$rfmo_name) %>% 
      #   rename(rfmo_total_zone_abd = total_zone_abd,
      #          rfmo_name = name) %>% 
      #   left_join(stradd_data,
      #             by = c("rfmo_name","taxon_key"),
      #             relationship = "many-to-many") %>% 
      #   rename(region_name = region)
      
      
      # Get neighbors 
      
      
      neighbour_data <- spp_data_area %>% 
        rename(neighbour = rfmo_name, neighbour_abd = total_rfmo_abd)
      
      rfmo_values_neigh <- spp_data_area %>% 
        rename(territory = rfmo_name) %>% 
        left_join(rfmo_neighbour,
                  by = "territory",
                  relationship = "many-to-many") %>% 
        rename(territory_abd = total_rfmo_abd) %>% 
        left_join(neighbour_data,
                  by = c("year", "taxon_key", "neighbour") 
        ) %>% 
        # remove potential sameless
        mutate(
          same = ifelse(territory == neighbour,"same","not")
        ) %>% 
        filter(same == "not") %>% 
        select(- same)
      
      
      # combine both regions to estimate proportions
      spp_proportion <- rfmo_values_neigh %>% 
        mutate(region_total = territory_abd+neighbour_abd) %>% 
        mutate(per_rfmo_t = territory_abd/region_total) %>%  # done with RFMO 
        mutate(per_rfmo_n = neighbour_abd/region_total) %>%  # done with RFMO 
        filter(region_total >0) # Remove cases where there is no catch data in any area
      
      # How it looks
      # spp_proportion %>% 
      #   # filter(territory == "IATTC") %>% 
      # ggplot() +
      #   geom_area(
      #     aes(
      #       x = as.numeric(year),
      #       y = per_rfmo_t#,
      #       # fill = neighbour
      #       # fill = high_mig_spa
      #     )
      #   ) +
      #   facet_wrap(territory~neighbour)
      
      
      # Estimate changes in abd proportion
      
      # Early proportions
      hs_historical_means <- spp_proportion %>% 
        filter(year <= 2014) %>%
        group_by(taxon_key,territory,neighbour) %>% 
        summarise_at(
          c("per_rfmo_t","per_rfmo_n"),
          c(mean = "mean",
            sd = "sd"), na.rm = T
        ) %>%
        mutate(top_tresh_t = per_rfmo_t_mean+2*per_rfmo_t_sd,
               low_tresh_t = per_rfmo_t_mean-2*per_rfmo_t_sd,
               top_tresh_n = per_rfmo_n_mean+2*per_rfmo_n_sd,
               low_tresh_n = per_rfmo_n_mean-2*per_rfmo_n_sd) %>% 
        select(-per_rfmo_t_mean:-per_rfmo_n_sd)
      
      # Estimate future proportions that overshoot the mean =- 2 sd
      prop_chng <- spp_proportion %>% 
        mutate(period = ifelse(year >2021 & year <2040,"ear",
                               ifelse(year > 2041 & year <2060,"mid",NA)
        )
        ) %>% 
        filter(!is.na(period)) %>% 
        group_by(taxon_key,territory,neighbour,period) %>% 
        summarise_at(
          c("per_rfmo_t","per_rfmo_n"),
          mean, na.rm = T
        ) %>% 
        left_join(hs_historical_means,
                  by = c("taxon_key","territory","neighbour"),
                  relationship = "many-to-many") %>% 
        mutate(change_t = ifelse(per_rfmo_t > top_tresh_t,"gain",
                                 ifelse(per_rfmo_t < low_tresh_t,"lost","same")
        ),
        change_n = ifelse(per_rfmo_n > top_tresh_n,"gain",
                          ifelse(per_rfmo_n < low_tresh_n,"lost","same")
        )
        ) 
      
      
      # Incorporate rcp and esm info to data
      # str_count(files_path[m]) # 90
      
      final_data <- prop_chng %>% 
        mutate(esm = str_to_lower(str_sub(files_path[m],80,83)),
               rcp = paste0("rcp",str_sub(files_path[m],84,85)),
               esm = ifelse(esm == "mpi2","mpi", ifelse(esm == "mpi8","mpi",esm)),
               rcp = ifelse(rcp == "rcp6F","rcp26", ifelse(rcp == "rcp5F","rcp85",rcp)),
        ) %>% 
        select(taxon_key,esm,rcp,period,rfmo_t = territory, low_tresh_t,mean_t = per_rfmo_t,top_tresh_t,change_t,
               rfmo_n = neighbour,low_tresh_n,mean_n = per_rfmo_n,top_tresh_n,change_n)
      
      if(m == 1){
        final_output <- final_data
      }else{
        final_output <- bind_rows(final_output,final_data)
      }
      
      
    } # Close models for loop
    
  }
  
  
  if(nrow(final_output) > 0){
    # Save file
    name_file <- my_path("R","Per_change_rfmo_rfmo", paste0(taxon_key,"_perchg.csv"))
    write_csv(final_output, name_file)
    return(paste("Data for",taxon_key, "saved"))
  }else{
    return(paste("No shared data for",taxon_key))
  }
  
}
