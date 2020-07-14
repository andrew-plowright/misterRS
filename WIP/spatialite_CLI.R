sqlite3 <- function(DBfile, SQL, exeFile = "C:/OSGeo4W64/bin/sqlite3.exe"){

  SQL <- paste0( paste(SQL, collapse = ";"), ";")

  system(paste( shQuote(sqlexe), shQuote(DBfile), shQuote(SQL)), intern = TRUE)
}

in_GPKG <- "C:/Users/Andrew/Desktop/scratch/five_rows.gpkg"



sqlite3(in_GPKG, "select polyID from santa")

sqlite3(in_GPKG, c("SELECT load_extension('mod_spatialite.dll')",
                   "UPDATE santa SET tileNames = (1,2,3,4,5)"))

sqlite3(in_GPKG, "select tileNames from rufsegPoly_R22C12 WHERE polyID < 10")
