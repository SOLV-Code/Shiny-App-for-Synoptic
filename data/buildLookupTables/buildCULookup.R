# map attribute names used in web app to names used in MAIN_CU_LOOKUP_FOR_SOS.csv
attribs <- list(
  Species = 'Species_Short',
  Species_Full = 'Species',
  CU_Name = 'CU_Name',
  CU_Acro = 'CU_Acro',
  Lat = 'latitude',
  Lon = 'long',
  FAZ = 'FAZ',
  MAZ = 'MAZ',
  JAZ = 'JAZ',
  Area = 'Area',
  RunTiming = 'RunTiming',
  AvGen = 'Avg_Gen',
  LifeHistory = 'LifeHistory'
)

lookup <- read.csv('CULookupTemplate.csv', stringsAsFactors = F)
attribMaster <- read.csv('MAIN_CU_LOOKUP_FOR_SOS.csv', stringsAsFactors = F)
attrCols <- lapply(names(attribs), function(a) {
  unlist(lapply(lookup$SoS_Lookup_CU_ID, function(CU) {
    val <- 'NA'
    if (CU %in% attribMaster$CU_ID)
      val <- attribMaster[attribMaster$CU_ID == CU, attribs[[a]]]
    if (is.na(val) || val == '') val <- 'NA'
    val
  }))
})
names(attrCols) <- names(attribs)
lookup <- cbind(lookup, as.data.frame(attrCols))
write.csv(lookup, 'CULookup.csv') 


