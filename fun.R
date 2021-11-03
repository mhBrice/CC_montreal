# Barplot 

ci_bar <- function(bp, upper, lower, length = 0.02, col){
  arrows(x0 = bp, y0 = upper, 
         x1 = bp, y1 = lower, 
         angle = 90, code = 3, lwd = 1.2,
         length = length, col = col)
}


bp_clim <- function(clim, indice, saison = "Annuel",
                    val = "Valeur",
                    col = c("#0869a2", "#cb2f3b"),
                    ylab = indice,
                    legpos = "topleft", main = "") {
  
  clim <- clim %>% 
    filter(Indice == indice & Saison == saison) %>%
    arrange(Horizon, Emissions) %>% 
    mutate(diff = Valeur - first(Valeur))  
  
  hist <- filter(clim, Emissions == "hist")
  hh_mat <- filter(clim, Percentile == "p50" & Emissions != "hist")
  
  ll_mat <- filter(clim, Percentile == "p10")
  uu_mat <- filter(clim, Percentile == "p90")
  
  yrange <- range(c(0, clim[[val]]*1.05))
  
  bp <- barplot(hh_mat[[val]], beside = TRUE, 
                border = col, col = alpha(col, .3), space = c(.1,.1,1,.1),
                axes = FALSE, ylim = yrange, axisnames = FALSE, main = main)
  box2(1:2)
  ci_bar(bp = bp, upper = uu_mat[[val]], lower = ll_mat[[val]], col = col)
  
  axis(1, at = c(1.15, 4.25), labels = c("2041-2070", "2071-2100"), 
       line = -.5, tick = F, cex.axis = .9)
  
  axis(2, las = 1, line = -.5, tick = F, cex.axis = .9)
  mtext(ylab, 2, line = 2.5)
  axis(2, cex.axis = .7, labels = FALSE, tcl = -.3)
  
  if(val == "Valeur") {
    abline(h = hist[[val]], col = "grey35", lwd = 2, lty = 2, xpd = FALSE)
    
    legend(legpos, legend = c("Historique", "RCP 4.5", "RCP 8.5"), 
           border = c(NA, col), fill = c(NA, alpha(col, .3)), 
           col = c("grey35", NA, NA), lwd = c(2, NA, NA), lty = c(2, NA, NA),
           bty = "n", cex = 1)
  } else {
    legend(legpos, legend = c("RCP 4.5", "RCP 8.5"), 
           border = col, fill = alpha(col, .3), 
           bty = "n", cex = .9)
  }
  
}



plot_clim <- function(clim, indice, saison = "Annuel",
                    val = "Valeur",
                    col = c("#0869a2", "#cb2f3b"),
                    ylab = indice,
                    legpos = "topleft", main = "") {
  
  clim <- clim %>% 
    filter(Indice == indice & Saison == saison) %>%
    arrange(Horizon, Emissions) %>% 
    mutate(diff = Valeur - first(Valeur))  
  
  hist <- filter(clim, Emissions == "hist")
  hh_mat <- filter(clim, Percentile == "p50" & Emissions != "hist")
  
  ll_mat <- filter(clim, Percentile == "p10")
  uu_mat <- filter(clim, Percentile == "p90")
  
  yrange <- range(c(0, clim[[val]]*1.05))
  
  bp <- barplot(hh_mat[[val]], beside = TRUE, plot = FALSE,
                border = col, col = alpha(col, .3), space = c(.1,.1,1,.1),
                axes = FALSE, ylim = yrange, axisnames = FALSE, main = main)
  plot(bp, hh_mat[[val]], col = col, axes = FALSE, pch = 20, cex = 2,
       ylim = yrange, main = main, xlab = "", ylab = "")
  box2(1:2)
  ci_bar(bp = bp, upper = uu_mat[[val]], lower = ll_mat[[val]], col = col)
  
  axis(1, at = c(1.15, 4.25), labels = c("2041-2070", "2071-2100"), 
       line = -.5, tick = F, cex.axis = .9)
  
  axis(2, las = 1, line = -.5, tick = F, cex.axis = .9)
  mtext(ylab, 2, line = 2.5)
  axis(2, cex.axis = .7, labels = FALSE, tcl = -.3)
  
  if(val == "Valeur") {
    abline(h = hist[[val]], col = "grey35", lwd = 2, lty = 2, xpd = FALSE)
    
    legend(legpos, legend = c("Historique", "RCP 4.5", "RCP 8.5"), 
           col = c("grey35", col), lwd = c(2, NA, NA), lty = c(2, NA, NA), pch = c(NA, 20, 20),
           bty = "n", cex = 1)
  } else {
    legend(legpos, legend = c("RCP 4.5", "RCP 8.5"), 
           border = col, fill = alpha(col, .3), 
           bty = "n", cex = .9)
  }
  
}
