###################
segPoly_RSDS <- rsds$rufsegPoly



###################

# Read tiles
ts <- segPoly_RSDS@tileScheme

# Get tile neighbours
nbs <- lapply(setNames(row.names(ts@data), row.names(ts@data)), function(tileName){

  nb <- apply(expand.grid(-1:1, -1:1), 1, function(x) x + ts@data[tileName, c("row", "col")])
  unlist(sapply(nb, function(x) row.names(suppressMessages(plyr::match_df( ts@data[,c("row", "col")], x)))))

})

# Create empty list
tl <- list()


for(tileName in tileNames){

  poly_paths <- segPoly_RSDS@tilePaths[nbs[[tileName]]]

  for(tileName in names(poly_paths)){

    tl[[tileName]] <-  polys <- sf::st_read(poly_paths[tileName], quiet = TRUE)
  }

  tls <- rbind(tl[[1]], tl[[2]])

  ints <- sf::st_intersects(tls)

  G <- igraph::graph_from_adj_list(ints)

  igraph::components(G)

  y <- st_union(st_union(tl[[1]], tl[[2]]))

  y <- st_cast(st_union(tl[[1]][5078,], tl[[2]][c(6,  15,  21),]), "POLYGON")

  plot(y)

plot(as_Spatial(tl[[1]][5078,]), border = "red", add = T)

  plot(as_Spatial(tl[[2]][c(6,  15,  21),]))

}

