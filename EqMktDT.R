EqMKTDT <- function(index, sector = NA){
  library(magrittr)
  
  # Rblpapi::blpConnect()
  # 
  # dat <-  index %>%
  #   stringr::str_to_upper() %>%
  #   paste('Index') %>%
  #   Rblpapi::bds('INDX_MEMBERS') %>%
  #   dplyr::pull() %>%
  #   paste('Equity') %>%
  #   Rblpapi::bdp(
  #     fields = c('SHORT_NAME',
  #                'GICS_SECTOR_NAME',
  #                'GICS_INDUSTRY_GROUP_NAME',
  #                'GICS_INDUSTRY_NAME',
  #                'GICS_SUB_INDUSTRY_NAME')
  #   ) %>%
  #   tibble::rownames_to_column(var = 'TICKER') 
  # 
  # readr::write_csv(dat, path = paste0('./data/', index, '.csv'))
  
  dat <- paste0('./data/', index, '.csv') %>% readr::read_csv(col_types = 'cccccc')
  
  dat <- dat %>%
    dplyr::mutate(
      GICS_SECTOR_NAME = GICS_SECTOR_NAME %>% 
        stringr::str_replace_na(replacement = 'Unclassified'),
      GICS_INDUSTRY_GROUP_NAME = GICS_INDUSTRY_GROUP_NAME %>%
        stringr::str_replace_na(replacement = 'Unclassified'),
      GICS_INDUSTRY_NAME = GICS_INDUSTRY_NAME %>%
        stringr::str_replace_na(replacement = 'Unclassified'),
      GICS_SUB_INDUSTRY_NAME = GICS_SUB_INDUSTRY_NAME %>%
        stringr::str_replace_na(replacement = 'Unclassified')
    ) 
  
  if(!is.na(sector)){
    dat <- dat %>% dplyr::filter(GICS_SECTOR_NAME == sector)
  }
  
  
  
  dat$pathString <- paste(index,
                          dat$GICS_SECTOR_NAME,
                          dat$GICS_INDUSTRY_GROUP_NAME,
                          dat$GICS_INDUSTRY_NAME,
                          dat$GICS_SUB_INDUSTRY_NAME,
                          dat$SHORT_NAME,
                          sep = '|')
  
  n <- dat %>% dplyr::tally() %>% dplyr::pull()
  
  market <- dat %>%
    data.tree::as.Node(pathDelimiter = '|') %>%
    data.tree::ToListExplicit(unname = TRUE) %>%
    networkD3::diagonalNetwork(
      height = n*10,
      width = 1000,
      nodeColour = 'black',
      nodeStroke = 'black')
  
  
  return(market)
  
}

# margin = c(top = -1700, bottom = -1700, left = 250)
