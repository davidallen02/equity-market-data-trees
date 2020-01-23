EqMKTDT <- function(index){
  library(magrittr)
  
  Rblpapi::blpConnect()
  
  dat <-  index %>%
    stringr::str_to_upper() %>%
    paste('Index') %>%
    Rblpapi::bds('INDX_MEMBERS') %>%
    dplyr::pull() %>%
    paste('Equity') %>%
    Rblpapi::bdp(
      fields = c('SHORT_NAME',
                 'GICS_SECTOR_NAME',
                 'GICS_INDUSTRY_GROUP_NAME',
                 'GICS_INDUSTRY_NAME',
                 'GICS_SUB_INDUSTRY_NAME')
    ) %>%
    tibble::rownames_to_column(var = 'TICKER') 
  
  readr::write_csv(dat, path = paste0('./data/', index, '.csv'))

  dat$pathString <- paste(index,
                          dat$GICS_SECTOR_NAME,
                          dat$GICS_INDUSTRY_GROUP_NAME,
                          dat$GICS_INDUSTRY_NAME,
                          dat$GICS_SUB_INDUSTRY_NAME,
                          dat$SHORT_NAME,
                          sep = '|')

  market <- dat %>%
    data.tree::as.Node(pathDelimiter = '|') %>%
    data.tree::ToListExplicit(unname = TRUE) %>%
    networkD3::radialNetwork()
  

return(market)
  
}

