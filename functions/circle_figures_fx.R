

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
             key = rfmo_name,
             value = n)
    
    
    name <- paste0("./results/figures/circle/",level,"_circular_",change_val,"_",period_val,"_",rcp_val,".png")
    
    png(name,
        width = 1300, height = 1300, res = 150, units = "px")
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
      scale = T,
      symmetric = T,
      big.gap = 5
    )
    dev.off()
    
    if(change_val == "same"){
      name <- paste0("./results/figures/circle/",level,"_circular_",change_val,"_",period_val,"_",rcp_val,".png")
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
        scale = T,
        symmetric = T,
        big.gap = 5,
        if(level == "realm"){
        c(annotationTrack = "grid",
        preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(circle_data))))))
        }
      )
      if(level == "realm"){
      # we go back to the first track and customize sector labels
      circos.track(track.index = 1, panel.fun = function(x, y) {
        circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
                    facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
      }, bg.border = NA) # here set bg.border to NA is important
      }
    
    dev.off()
    
    }
    
  }else{
    
    # Net Change Data
    net_change <-
      data %>% 
      pivot_wider(
        names_from = change,
        values_from = n) %>% 
      mutate_at(.var = c("gain","lost"),
                .funs = replace_na,0
      ) %>% 
      mutate(
             value = gain - lost,
             rowname_b = ifelse(value > 0,name,rfmo_name),
             key_b = ifelse(value < 0,name,rfmo_name)) %>% 
      # View()
      ungroup() %>%
      mutate(
        n_shifting = gain+lost,
        per = value/(n_shifting),
        per_non = same/(gain+lost+same)*100
      ) %>% 
      # View()
      filter(period == period_val,
             rcp == rcp_val) %>% 
      select(rowname = rowname_b,
             key = key_b,
             value) %>% 
      filter(value != 0)
    
    
    
    name_net <- paste0("./results/figures/circle/",level,"_circular_net_",period_val,"_",rcp_val,".png")
    png(name_net,
        width = 1300, height = 1300, res = 150, units = "px")
    
    chordDiagram(
      x = net_change %>% mutate(key = ifelse(key == "Temperate\nN. Pacific", "T.N. Pacific",
                                             ifelse(key == "Temperate\nN. Atlantic", "T.N. Atlantic",key))), 
      grid.col = mycolor_scale,
      directional = 1, 
      direction.type = c("diffHeight","arrows"),
      link.arr.type = "big.arrow",
      link.arr.length = 0.1,
      link.sort = F,
      link.largest.ontop = TRUE,
      transparency = 0.30,
      scale = F,
      symmetric = F,
      if(level == "realm"){
      c(annotationTrack = "grid", 
      preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(net_change))))))
      }
    )
    if(level == "realm"){
    circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
      xlim = get.cell.meta.data("xlim")
      ylim = get.cell.meta.data("ylim")
      sector.name = get.cell.meta.data("sector.index")
      circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
      circos.axis(h = "top", labels.cex = 0.5, major.tick.length = 0.2, sector.index = sector.name, track.index = 2)
    }, bg.border = NA)
    }
    dev.off() 
    
  }
  
}

