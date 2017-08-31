library(dplyr)

## 

## Read table
cit <- read.csv('siteinfo_citations.csv')
cit <- cit %>% mutate( Citation = as.character(Citation) )

bibfile <- readLines("FLUXNET2015v1-1_bs.bib")

for (idx in seq(nrow(cit))){

  ## Isolate name
  tmp <- sub( " et al.", "", substr( cit$Citation[idx], start=2, stop=(regexpr( ',', cit$Citation[idx] )-1) ) )

  # print( paste( "Citation:", cit$Citation[idx], "   isolated name:", tmp ) )

  ## get citation key
  uselines <- grep( tmp, bibfile )
  sub <- bibfile[ uselines ]
  uselines <- grep( '@article', sub )
  if (length(uselines)>0){
    tmp2 <- sub[uselines]
    cit$citekey[idx] <- substr( tmp2, start=10, stop=(regexpr(',', tmp2 )[1]-1) )
  } else {
    ## try with all upper-case
    tmp <- toupper(tmp)
    uselines <- grep( tmp, bibfile )
    sub <- bibfile[ uselines ]
    uselines <- grep( '@article', sub )
    if (length(uselines)>0){
      tmp2 <- sub[uselines]
      cit$citekey[idx] <- substr( tmp2, start=10, stop=(regexpr(',', tmp2 )[1]-1) )
    }    
  }

  ## remove brackets
  cit$Citation[idx] <- substr( cit$Citation[idx], start=2, stop=(regexpr(')', cit$Citation[idx] )[1]-1) )

}

write.csv( cit, file="siteinfo_citations_L1.csv", row.names=FALSE )

## Combine with my information
load( "data/overview_data_fluxnet2015_L5.Rdata" ) # loads 'overview'

cit <- cit %>% rename( mysitename=Site )
overview <- overview %>% left_join( dplyr::select( cit, mysitename, Citation, citekey ), by="mysitename" )

overview$Cluster <- overview$finalcluster
overview$Cluster[which(overview$finalcluster==1)] <- "cDD"
overview$Cluster[which(overview$finalcluster==2)] <- "cGR"
overview$Cluster[which(overview$finalcluster==3)] <- "cLS"
overview$Cluster[which(overview$finalcluster==4)] <- "cNA"

out <- overview %>% arrange( finalcluster ) %>% 
                    mutate( lon=format(lon, digits=2), lat=format(lat, digits=3)) %>%
                    dplyr::select( mysitename, lon, lat, year_start, year_end, classid, Cluster, Citation, citekey ) %>%
                    dplyr::rename( Site=mysitename, Longitude=lon, Latitude=lat, Startyear=year_start, Endyear=year_end, IGBPclass=classid, Reference=Citation, Citekey=citekey )


write.csv( out, file="siteinfo_citations_L2.csv", row.names=FALSE )
