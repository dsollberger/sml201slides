library("yfR")
today <- Sys.Date()
start_date <- "2024-01-01"
stock_market_example <- yf_collection_get(collection = "SP500", 
                                          first_date = start_date, 
                                          last_date = today)