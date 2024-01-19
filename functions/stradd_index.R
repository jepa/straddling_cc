
#StraddIndexFun

# This function determines which transboundary stocks are straddling

StraddIndex <- function(Spp, Model = "All", Neighbours, Coord, Index_Code, treshold = F){
  
  # Get model data from spps
  SppDist <- GetSppDist(Spp,Model,Coord)
  
  
  # ggplot(SppDist %>% filter(Value >0)) +
  #   geom_tile(
  #     aes(
  #       x = longitude,
  #       y = latitude,
  #       fill = Value,
  #       color = Value
  #     )
  #   ) +
  #   facet_wrap(~Model) +
  #   geom_sf(data = rnaturalearth::ne_countries(scale = 'small', returnclass = c("sf")))
  
  
  
  # Result 1. Number of Countries that share the species
  
  #____________ ESTIMATING MODEL INDEX (TRESHOLD 1)_________ #
  Trans_Spp <- SppDist %>%
    filter(Model != "SAU_C") %>% 
    mutate(Value = ifelse(Value > 0, 1,0)) %>%  
    group_by(TaxonKey, INDEX) %>%
    summarise(Model_Index = sum(Value,na.rm=T)/3,
              .groups = "drop") %>%
    # filter(Model_Index > 0.4) %>% # at least 2 sources agree
    # ____________ ESTIMATING FUNDAMENTAL NICHE (TRESHOLD 2)_________ #
    left_join(SppDist,
              by = c("TaxonKey","INDEX")) %>%
    filter(Model == "SAU_C", # Only keeping cells where SAU catch exists
           Value > 0) %>%
    # mutate(Model_Index = Model_Index*100) %>%
    select(-Model, -Value, -latitude,-longitude,-Model_Index) %>%
    rename(index = INDEX) %>% 
    
    # Trans_Spp %>%
    #   left_join(coords_dbem) %>%
    #   ggplot() +
    #   geom_tile(
    #     aes(x = lon,
    #         y = lat,
    #         fill = index)
    #   ) +
    #   geom_tile( data = Trans_Spp %>% left_join(coords_dbem) %>% filter(index %in% c(72227,72228,72946)),
    #       aes(x = lon,
  #         y = lat),
  #       fill = "red"
  #     )
  
  # joint eez names
  left_join(meow_dbem_grid, # No EEZs but MEOWs
            by = "index") %>% 
    # joint rfmo area names
    left_join(rfmo_index,
              by = "index") %>% 
    mutate(
      region = ifelse(is.na(rfmo_name),realm,rfmo_name),
      category = ifelse(!is.na(rfmo_name),"rfmo","realm"),
    ) %>%
    select(1:3,region,category) %>%
    clean_names()
  
  ### ----------  Distributional test  ---------- ###
  # test <- Trans_Spp %>%
  # left_join(coords_dbem) #%>% 
  # filter(is.na(territory))
  
  # sau_index_code %>% filter(eez_name != "High seas") %>% 
  #   left_join(coords_dbem) %>% 
  #   ggplot() +
  #   geom_tile(
  #     aes(
  #       x = lon,
  #       y = lat
  #     ),
  #     fill = "red"
  #   ) + 
  #   geom_tile(data = meow_dbem_grid %>% left_join(coords_dbem),
  #     aes(
  #       x = lon,
  #       y = lat,
  #       # fill = rfmo_name
  #     ),
  #     fill = "black"
  #   ) + 
  #   geom_sf(data = sf_land, aes())
  
  # 
  # ggplot(test) +
  #   geom_tile(
  #     aes(
  #       x = lon,
  #       y = lat,
  #       fill = territory
  #     )
  #   ) +
  #   geom_sf(data = sf_land, aes())
  
  
  ### ----------------------------------- ###
  
  # Get realms and RFMOs
  rfmo_to_realm <- Neighbours %>% 
    filter(region %in% c(Trans_Spp %>% filter(category == "rfmo") %>% pull(region) %>% unique()),
           adjacent_region %in% c(Trans_Spp %>% filter(category == "realm") %>% pull(region) %>% unique()))
  
  
  #____________ ESTIMATING DISTRIBUTION INDEX (TRESHOLD 3)_________ #
  # The number of species' cells present within each REALM and RFMO
  
  Spp_Grid <- Trans_Spp %>% 
    group_by(taxon_key,
             region,
             category) %>% 
    summarise(n_cells_territory = length(unique(index)),
              .groups = "drop")
  
  # Split dataframes to merge latter
  Territory_T <- Spp_Grid %>% 
    filter(category == "realm") %>% 
    ungroup() %>% 
    select(
      taxon_key,
      Name=region,
      n_cells_territory
    )
  
  Neighbour_T <- Spp_Grid %>%
    filter(category == "rfmo") %>% 
    ungroup() %>% 
    select(
      taxon_key,
      n_cells_territory,
      Name=region
    ) 
  
  
  rfmo_grid <- Spp_Grid %>% 
    filter(category == "rfmo") %>% 
    left_join(rfmo_to_realm,
              by = "region") %>% 
    select(Name = adjacent_region,
           rfmo_name = region,
           n_cells_territory)
  
  # Merge dataframes to get totals per Neighbourds
  Area_Index_D <-
    full_join(Territory_T,
              Neighbour_T, 
              by = c("Name","taxon_key")
    ) %>%
    left_join(rfmo_grid,
              by ="Name") %>% 
    filter(!is.na(rfmo_name)) %>% # remove non-straddling stocks
    rowwise() %>%
    mutate(spp_total = sum(n_cells_territory.x,n_cells_territory,na.rm=T)) %>% # Total gridcelles per Neighbours
    distinct() %>% # Removes false duplicates from `full_join()`
    rename(realm = Name,
           n_cells_realm = n_cells_territory.x,
           n_cells_rfmo = n_cells_territory) %>%
    # Estimate if it is straddling by counting the percentage of the distribution in the RFMO
    mutate(area_rfmo = (n_cells_rfmo/spp_total)*100,
           area_realm = (n_cells_realm/spp_total)*100
    )
  
  
  output <- Area_Index_D %>% 
    select(taxon_key,
           realm,
           rfmo_name,
           area_rfmo,
           area_realm)
  
  File_Name <- paste(Spp,"_straddling.csv",sep = "")
  Save_Path <- my_path("R","Straddling_realm_rfmo", File_Name)
  
  if(nrow(output) >0){
    
    write_csv(output,
              Save_Path)
    
    return(print(paste("Completed analysis for taxon key",Spp)))
  }else{
    return(print(paste("Taxon key",Spp,"is not a straddling stock")))
  }
  
} # closes treshold ifstatement

} # closes function