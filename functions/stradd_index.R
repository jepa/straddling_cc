
#StraddIndexFun

# This function determines which transboundary stocks are straddling

stradd_index <- function(spp, model = "All", neighbours, coord, index_code,hs_index){
  
  colnames(hs_index)[2] <- "hs_region"
  
  
  # Get model data from spps
  spp_dist <- GetSppDist(Spp = spp,Model = model,Coord = coord) %>% 
    clean_names()
  
  
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
  trans_spp <- spp_dist %>%
    filter(model != "SAU_C") %>% 
    mutate(value = ifelse(value > 0, 1,0)) %>%  
    group_by(taxon_key, index) %>%
    summarise(model_index = sum(value,na.rm=t)/3,
              .groups = "drop") %>%
    # filter(model_index > 0.4) %>% # at least 2 sources agree
    # ____________ estimating fundamental niche (treshold 2)_________ #
    left_join(spp_dist,
              by = c("taxon_key","index")) %>%
    filter(model == "SAU_C", # only keeping cells where sau catch exists
           value > 0) %>%
    # mutate(model_index = model_index*100) %>%
    select(-model, -value, -latitude,-longitude,-model_index) %>%
    rename(index = index) %>% 
    
    # trans_spp %>%
    # left_join(coords_dbem) %>%
    # ggplot() +
    # geom_tile(
    #   aes(x = lon,
    #       y = lat,
    #       fill = index)
    # ) +
    # geom_tile( data = trans_spp %>% left_join(coords_dbem) %>% filter(index %in% c(72227,72228,72946)),
    #     aes(x = lon,
    #       y = lat),
    #     fill = "red"
    #   )
  
  # joint eez names
  left_join(meow_dbem_grid, # No EEZs but MEOWs
            by = "index") %>% 
    # joint hs_names
    left_join(hs_index,
              by = "index") %>% 
    mutate(
      region = ifelse(is.na(hs_region),realm,hs_region),
      category = ifelse(!is.na(hs_region),"hs_region","realm"),
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
  
  # Get realms and ocean basins
  hs_to_realm <- neighbours %>% 
    filter(region %in% c(trans_spp %>% filter(category == "hs_region") %>% pull(region) %>% unique()),
           adjacent_region %in% c(trans_spp %>% filter(category == "realm") %>% pull(region) %>% unique()))
  
  
  #____________ ESTIMATING DISTRIBUTION INDEX (TRESHOLD 3)_________ #
  # The number of species' cells present within each REALM and RFMO
  
  spp_grid <- trans_spp %>% 
    group_by(taxon_key,
             region,
             category) %>% 
    summarise(n_cells_territory = length(unique(index)),
              .groups = "drop")
  
  # Split dataframes to merge latter
  territory_t <- spp_grid %>% 
    filter(category == "realm") %>% 
    ungroup() %>% 
    select(
      taxon_key,
      name=region,
      n_cells_territory
    )
  
  neighbour_t <- spp_grid %>%
    filter(category == "hs_region") %>% 
    ungroup() %>% 
    select(
      taxon_key,
      n_cells_territory,
      name=region
    ) 
  
  
  hs_grid <- spp_grid %>% 
    filter(category == "hs_region") %>% 
    left_join(hs_to_realm,
              by = "region") %>% 
    select(name = adjacent_region,
           hs_name = region,
           n_cells_territory)
  
  # Merge dataframes to get totals per Neighbourds
  area_index_d <-
    full_join(territory_t,
              neighbour_t, 
              by = c("name","taxon_key")
    ) %>%
    left_join(hs_grid,
              by ="name") %>% 
    filter(!is.na(hs_name)) %>% # remove non-straddling stocks
    rowwise() %>%
    mutate(spp_total = sum(n_cells_territory.x,n_cells_territory,na.rm=T)) %>% # Total gridcelles per Neighbours
    distinct() %>% # Removes false duplicates from `full_join()`
    rename(realm = name,
           n_cells_realm = n_cells_territory.x,
           n_cells_rfmo = n_cells_territory) %>%
    # Estimate if it is straddling by counting the percentage of the distribution in the RFMO
    mutate(area_hs = (n_cells_rfmo/spp_total)*100,
           area_realm = (n_cells_realm/spp_total)*100
    )
  
  
  output <- area_index_d %>% 
    select(taxon_key,
           realm,
           hs_name,
           area_hs,
           area_realm)
  
  File_Name <- paste(Spp,"_straddling.csv",sep = "")
  Save_Path <- my_path("R","straddling_realm_hs", File_Name)
  
  if(nrow(output) >0){
    
    write_csv(output,
              Save_Path)
    
    return(print(paste("Completed analysis for taxon key",Spp)))
  }else{
    return(print(paste("Taxon key",Spp,"is not a straddling stock")))
  }
  
} # closes function
