#' IndexPlotExtrapolation
#'
#'@description
#'
#'  Print fDB and TT violin plots with best and most frequent value
#'
#' @param results ClusterAnalysis output
#'
#'
#' @return fDB and TT violin plots with best and most frequent value
#'

#' @seealso ClusterAnalysis
#'
#' @import gghalves ggplot2 MetBrewer
#' 
#'
setGeneric("IndexPlotExtrapolation", function(results) {
  standardGeneric("IndexPlotExtrapolation")
})
setMethod("IndexPlotExtrapolation",signature(), function(results) {
  indexes = do.call(rbind, lapply(seq_along(results$Clusterings), function(x) {
        xx = results$Clusterings[[x]]
        df = data.frame(xx$TTandfDBandSil)
        df$freq = xx$freq
        df$which = x
        return(df)
    }))
  
  # palette di colori picasso
  pastel_colors <- met.brewer("VanGogh1", max(indexes$G))
  
  #uncount di tidyverse per replicare le frequenze
  indexesfull = indexes[rep(row.names(indexes), times = indexes$freq), ] %>% 
    tidyr::gather(-G,-freq,-which,value = "IndexesV",key = "Indexes")
  indexesfilteredMaxFreq <- indexes %>%
    tidyr::gather(-G, -freq, -which, value = "IndexesV", key = "Indexes") %>%
    group_by(G, Indexes) %>%
    filter(freq == max(freq)) %>%
    arrange(G, Indexes, IndexesV) %>% 
    slice(1) %>%  
    ungroup()
  
  # Modifica per prendere solo il valore più basso quando ci sono duplicati
  indexesfilteredMinfDB = indexes %>%
    group_by(G) %>%
    filter(fDB == min(fDB)) %>%
    tidyr::gather(-G,-freq,-which,value = "IndexesV",key = "Indexes") %>%
    group_by(G, Indexes) %>%
    arrange(G, Indexes, IndexesV) %>% 
    slice(1) %>%  
    ungroup()
  
  indexesfilteredMaxSil = indexes %>%
    group_by(G) %>%
    filter(Sil == max(Sil)) %>%
    tidyr::gather(-G,-freq,-which,value = "IndexesV",key = "Indexes")%>%
    group_by(G, Indexes) %>%
    arrange(G, Indexes, IndexesV) %>% 
    slice(1) %>%  
    ungroup()

  
  a<-indexes%>%filter(G==6)
  # Creiamo un dataframe per la legenda dei G
  g_values <- unique(indexes$G)
  g_labels <- paste("G =", g_values)
  names(pastel_colors) <- g_labels[1:length(pastel_colors)]
  
  return(suppressWarnings(
    indexesfull %>% 
      ggplot(aes(x = G, y = IndexesV)) +
      geom_half_point(aes(color = paste("G =", G)), width = 0.8, alpha = 1, 
                      side = "l", transformation = position_jitter(height = 0), size = 2) +
      geom_half_violin(aes(fill = paste("G =", G)), side = "r", alpha = 0.7, scale = "width") +
      geom_half_boxplot(aes(fill = paste("G =", G)), alpha = 0.7, fatten = 1) +
      
      geom_point(data = indexesfilteredMaxFreq, 
                 aes(x = G, y = IndexesV, color = "MaxFreq"), 
                 shape = 8, size = 4, stroke=1.5, position = position_nudge(x = -0.3)) +
      geom_point(data = indexesfilteredMinfDB, 
                 aes(x = G, y = IndexesV, color = "minfDB"), 
                 shape = 6, size = 4, stroke=1.5, position = position_nudge(x = 0)) +
      geom_point(data = indexesfilteredMaxSil,
                 aes(x = G, y = IndexesV, color = "maxSilhouette"), 
                 shape = 2, size = 4, stroke=1.5, position = position_nudge(x = 0.3)) +
      
      geom_hline(data = indexesfull %>% filter(Indexes == "fDB"), 
                 aes(yintercept = 1), linetype = "dashed") +
      scale_fill_manual("G Values", values = pastel_colors) +
      scale_color_manual("Metrics", 
                         values = c("MaxFreq" = "#DF6D27", 
                                    "minfDB" = "#FBB13C",
                                    "maxSilhouette" = "#944D5F"))    +
      
      facet_wrap(~Indexes, scales = "free") +
      theme_bw() +
      xlab("G") +
      ylab("IndexesV")
  ))
})