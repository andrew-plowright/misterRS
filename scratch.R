b <- brick("D:/Vc/DerivedData/VerticalImage/VerticalImage_2018/VI_R17C20.tif")


pcas <- list.files("D:/Vc/DerivedData/PCA/PCA_ortho_2018", pattern = "\\.tif$", full.names = T)

pca <- brick("D:/Vc/DerivedData/PCA/PCA_ortho_2018/PCA_R17C20.tif")

pc_stats <- do.call(rbind, lapply(pcas, function(pca){

  pc <- brick(pca)
  pc1 <- pc[[1]][]
  pc2 <- pc[[2]][]

  pc1mean <- mean(pc1)
  pc2mean <- mean(pc2)
  pc1sd   <- sd(pc1)
  pc2sd   <- sd(pc2)

  out <- c(
    range(pc1),
    range(pc2),
    pc1mean - 2 * pc1sd,
    pc1mean + 2 * pc1sd,
    pc2mean - 2 * pc2sd,
    pc2mean + 2 * pc2sd
  )
  names(out) <- c("PC1 min", "PC1 max", "PC2 min", "PC2 max", "PC1 2sdmin", "PC1 2sdmax", "PC2 2sdmin", "PC2 2sdmax")

  out

}))



hist(pc_stats$`PC1 min`, xlim = c(-15,15), breaks = seq(-16,63, 0.5), main = "PC1", border = "red")
hist(pc_stats$`PC1 max`, xlim = c(-15,15), breaks = seq(-16,63, 0.5), add = T, border = "red")

hist(pc_stats$`PC1 2sdmin`, xlim = c(-15,15), breaks = seq(-16,63, 0.5), add = T, border = "blue")
hist(pc_stats$`PC1 2sdmax`, xlim = c(-15,15), breaks = seq(-16,63, 0.5), add = T, border = "blue")
mean(pc_stats$`PC1 2sdmin`)
mean(pc_stats$`PC1 2sdmax`)


hist(pc_stats$`PC2 min`, xlim = c(-15,15), breaks = seq(-16,63, 0.5), main = "PC2", border = "red")
hist(pc_stats$`PC2 max`, xlim = c(-15,15), breaks = seq(-16,63, 0.5), add = T, border = "red")

hist(pc_stats$`PC2 2sdmin`, xlim = c(-15,15), breaks = seq(-16,63, 0.5), add = T, border = "blue")
hist(pc_stats$`PC2 2sdmax`, xlim = c(-15,15), breaks = seq(-16,63, 0.5), add = T, border = "blue")
mean(pc_stats$`PC2 2sdmin`)
mean(pc_stats$`PC2 2sdmax`)


