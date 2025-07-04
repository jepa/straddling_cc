# Function to create circluar figures 3 and 5
# Note that characteristics change depending on the figure type 
# Paths embeeded in code

circular_plot <- function(data,period_val,rcp_val,change_val,level = "realm"){
  
  if(change_val != "net"){
    # Gain Data
    circle_data <- data %>% 
      filter(
        period == period_val,
        rcp == rcp_val,
        change == change_val
      ) %>% 
      ungroup() %>% 
      select(rowname = name,
             key = hs_name,
             value = n_per)
    
    
    name <- paste0("./results/figures/circle/",level,"_circular_",change_val,"_",period_val,"_",rcp_val,".png")
    
    if(level == "realm"){
      png(name,
          width = 1300, height = 1300, res = 150, units = "px")
    }else(
      png(name,
          width = 700, height = 700, res = 150, units = "px")
    )
    chordDiagram(
      x = circle_data %>% arrange(rowname), 
      grid.col = mycolor_scale,
      directional = ifelse(change_val == "gain",1, -1),
      # directional = -1,
      direction.type = ifelse(change_val == "same","diffHeight","arrows"),
      # direction.type = c("diffHeight","arrows"),
      link.arr.type = "big.arrow",
      link.arr.length = 0.1,
      link.sort = T,
      link.auto = T,
      link.largest.ontop = TRUE,
      transparency = 0.30,
      scale = F,
      symmetric = T,
      big.gap = 5,
      annotationTrack = "grid", 
      preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(circle_data)))))
    )
    
    circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
      xlim = get.cell.meta.data("xlim")
      ylim = get.cell.meta.data("ylim")
      sector.name = get.cell.meta.data("sector.index")
      circos.text(mean(xlim), ylim[1] - .1, 
                  labels = sector.name[!sector.name %in% c("High Seas","Arctic")], 
                  facing = "clockwise", 
                  niceFacing = TRUE, #Orientation of label 
                  adj = c(1.3, 0.5) # adjust position of label (0.5 middle)
      )
      circos.axis(h = "top", 
                  # labels.font	= "Times",
                  labels.cex = 1, 
                  major.tick.length = 1, 
                  sector.index = sector.name,
                  track.index = 2)
    }, bg.border = NA)
    
    # Arctic label
    circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
      xlim = get.cell.meta.data("xlim")
      ylim = get.cell.meta.data("ylim")
      sector.name = get.cell.meta.data("sector.index")
      circos.text(mean(xlim), ylim[1] - .3, 
                  labels = sector.name[sector.name %in% c("Arctic")], 
                  facing = "clockwise", 
                  niceFacing = TRUE, #Orientation of label 
                  adj = c(1.3, 0.5) # adjust position of label (0.5 middle)
      )
    }, bg.border = NA)
    
    # High Seas label
    circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
      xlim = get.cell.meta.data("xlim")
      ylim = get.cell.meta.data("ylim")
      sector.name = get.cell.meta.data("sector.index")
      circos.text(mean(xlim), ylim[1] - .1, 
                  labels = sector.name[sector.name %in% c("High Seas")], 
                  facing = "bending.outside", 
                  niceFacing = TRUE, #Orientation of label 
                  adj = c(0.5, 4) # adjust position of label (0.5 middle)
      )
    }, bg.border = NA)
    
    dev.off()
    
    
    # Same plots
    
    if(change_val == "same"){
      name <- paste0("./results/figures/circle/",level,"_circular_",change_val,"_",period_val,"_",rcp_val,".png")
      
      if(level == "realm"){
        png(name,
            width = 1300, height = 1300, res = 150, units = "px")
        chordDiagram(
          x = circle_data %>% arrange(rowname), 
          grid.col = mycolor_scale,
          directional = ifelse(change_val == "gain",1, -1),
          direction.type = ifelse(change_val == "same","diffHeight","arrows"),
          link.arr.type = "big.arrow",
          link.arr.length = 0.1,
          link.sort = T,
          link.auto = T,
          link.largest.ontop = TRUE,
          transparency = 0.30,
          scale = F,
          symmetric = T,
          big.gap = 5,
          annotationTrack = "grid",
          preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(circle_data)))))
        )
        
        # we go back to the first track and customize sector labels
        circos.track(track.index = 1, panel.fun = function(x, y) {
          circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
                      facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
        }, bg.border = NA)
        # here set bg.border to NA is important
      }else{
        png(name,
            width = 700, height = 700, res = 150, units = "px")
        chordDiagram(
          x = circle_data %>% arrange(rowname), 
          grid.col = mycolor_scale,
          directional = ifelse(change_val == "gain",1, -1),
          direction.type = ifelse(change_val == "same","diffHeight","arrows"),
          link.arr.type = "big.arrow",
          link.arr.length = 0.1,
          link.sort = T,
          link.auto = T,
          link.largest.ontop = TRUE,
          transparency = 0.30,
          scale = F,
          symmetric = T,
          big.gap = 5
        )
      }
      
      dev.off()
      
    }
    
  }else{
    
    # Net Change Data
    net_change <-
      data %>% 
      select(-n_per) %>% 
      filter(change != "same") %>%
      pivot_wider(
        names_from = change,
        values_from = n) %>% 
      mutate_at(.var = c("gain","lost"),
                .funs = replace_na,0
      ) %>% 
      mutate(
        value = gain - lost,
        rowname_b = ifelse(value > 0,name,hs_name),
        key_b = ifelse(value < 0,name,hs_name)) %>% 
      # View()
      ungroup() %>%
      filter(period == period_val,
             rcp == rcp_val) %>% 
      select(rowname = rowname_b,
             key = key_b,
             value)
    
    if(level =="realm"){
      net_change <- net_change %>% 
        filter(value != 0)
    }else{
      net_change <- net_change %>% 
        filter(value > 0)
    }
    
    
    
    
    name_net <- paste0("./results/figures/circle/",level,"_circular_net_",period_val,"_",rcp_val,".png")
    
    
    if(level =="realm"){
      png(name_net,
          width = 1300, height = 1300, res = 150, units = "px")
      chordDiagram(
        x = net_change, 
        grid.col = mycolor_scale,
        directional = 1, 
        direction.type = c("diffHeight","arrows"),
        link.arr.type = "big.arrow",
        link.arr.length = 0.1,
        link.sort = F,
        link.largest.ontop = TRUE,
        transparency = 0.30,
        scale = F,
        symmetric = T,
        annotationTrack = "grid", 
        preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(net_change)))))
      )
      
      circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
        circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
        circos.axis(h = "top", labels.cex = 1, major.tick.length = 0.2, sector.index = sector.name, track.index = 2)
      }, bg.border = NA)
      
    }else{
      # Plot for RFMO 
      png(name_net,
          width = 850, height = 850, res = 150, units = "px")
      chordDiagram(
        x = net_change,
        grid.col = mycolor_scale,
        directional = 1, 
        direction.type = c("diffHeight","arrows"),
        link.arr.type = "big.arrow",
        link.arr.length = 0.1,
        link.sort = T,
        link.largest.ontop = TRUE,
        transparency = 0.30,
        scale = TRUE,
        symmetric = TRUE,
      )
    }
    
    dev.off() 
  }
}

