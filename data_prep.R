# Ethereum cloud mining profitability calculator
# Frank Thomson <f.a.thomson@gmail.com>
# August 5, 2017

#
# R script too: 
# 1. Retrieve block and transaction data from ether chain and convert to custom objects
# 2. Save custom objects to sql
# 3. Retrieve custom objects from sql and aggregate per day and write this to Rda file which is used in dashboard.

# json rpc api
# cmd: geth --rpc --rpccorsdomain localhost --cache=4096 --datadir /mnt/volume-sfa2-01/ethereum
# cmd: geth --rpc --rpccorsdomain localhost --cache=4096 --datadir "D:/Ethereum"

# load libs
library(RSQLite)
library(dplyr)
library(bit64) # to save large ints in sqlitedb


# system("geth --rpc --rpccorsdomain localhost --cache=256 --datadir ")
#
# json rpc api functions. 
# thranks to: https://github.com/BSDStudios/ethr
# documentation: https://github.com/ethereum/wiki/wiki/JSON-RPC

eth_blockNumber <- function(rpc_address = "http://localhost:8545") {
  post_body <- list(jsonrpc = "2.0", method = "eth_blockNumber", params = "", id = 83)
  post_return <- httr::POST(url = rpc_address, body = post_body, encode = "json")
  post_content <- httr::content(post_return, as = "parsed")
  block_number <- post_content$result
  return(block_number)
}
eth_getBlockByNumber <-function(block_number, full_list, rpc_address = "http://localhost:8545") {
  block_number <- as.character(block_number)
  full_list <- as.logical(full_list)
  body <- list(jsonrpc = "2.0", method = "eth_getBlockByNumber", params = list(block_number, full_list), id = 1)
  block_return <- httr::POST(url = rpc_address, body = body, encode = "json")
  block_dat <- httr::content(block_return)$result
  return(block_dat)
}
eth_getTransactionReceipt <- function(transaction_hash, rpc_address = "http://localhost:8545") {
  body <- list(jsonrpc = "2.0", method = "eth_getTransactionReceipt",  params = list(transaction_hash), id = 1)
  TransReceipt_return <- httr::POST(url = rpc_address, body = body, encode = "json")
  TransReceipt <- httr::content(TransReceipt_return)$result
  return(TransReceipt)
}
eth_getUncleByBlockNumberAndIndex <- function(block_number, index, rpc_address = "http://localhost:8545") {
  body <- list(jsonrpc = "2.0", method = "eth_getUncleByBlockNumberAndIndex",  params = list(block_number, index), id = 1)
  UncleBlock_return <- httr::POST(url = rpc_address, body = body, encode = "json")
  UncleBlock <- httr::content(UncleBlock_return)$result
  return(UncleBlock)
}


#
# helper functions

getNumber <- function(hex){
  class(hex) <- "numeric"
  return(as.numeric(sprintf("%.0f",hex)))
}
getHex <-  function(number){
  return(paste0("0x",sprintf("%x",number)))
}

enrichETHblock <- function(hexBlock){
  
  blockReward <- 5
  
  #
  # transaction fees
  #
  txFees <- 0
  ethValue <- 0
  for(transaction in hexBlock$transactions){
    
    ethValue <- ethValue + getNumber(transaction$value)/1e18
    tr <- eth_getTransactionReceipt(transaction$hash)
    if(is.null(tr))
      next
    gasUsed <- getNumber(tr$gasUsed)
    txFees <- txFees + getNumber(tr$gasUsed) * (getNumber(transaction$gasPrice)/1e18)
  }
  
  
  #
  # uncle eth
  #
  uncleETH <- 0
  for(i in 0:1){
    uncle <- eth_getUncleByBlockNumberAndIndex(hexBlock$number,getHex(i))
    if(!is.null(uncle))
      uncleETH <- uncleETH + ((8-(getNumber(hexBlock$number) - getNumber(uncle$number)))/8) * blockReward
  }
  
  #
  # miner reward for uncle
  #
  minerRewardUncle <- length(hexBlock$uncles) * 1/32 * blockReward
  
  
  
  #
  # create enriched ETH block dataframe
  #
  block <- data.frame(number = getNumber(hexBlock$number),
                      timestamp = as.POSIXct(getNumber(hexBlock$timestamp), origin="1970-01-01"),
                      difficulty=  getNumber(hexBlock$difficulty),
                      txCount = length(hexBlock$transactions),
                      txEther = ethValue,
                      txFees = txFees,
                      uncleETH = uncleETH,
                      minerUncleReward = minerRewardUncle,
                      totalETHreward = blockReward + txFees + minerRewardUncle + uncleETH)
  return(block)
}


#
# runner script
# NOTE: when running the first time it takes a while. 

# script start time
start.time <- Sys.time()

# connect to database and retrieve latest block
dbHandle = dbConnect(RSQLite::SQLite(), dbname="ethereum.db")
maxBlockInDB <- dbGetQuery(dbHandle, "select number from eth_blocks order by number desc limit 1")

# get the latest block in database 
maxBlockInDB <- as.numeric(maxBlockInDB)

# when no block is found, assume that there are no block in the db yet
if(is.na(maxBlockInDB[1]))
  maxBlockInDB <- 0

# start block is latest block in db + 1
startBlock <- maxBlockInDB + 1

# stop block is last block in chain
stopBlock <- getNumber(eth_blockNumber())

# blocks that will be added in this session. 1 when stopblock and startblok are equal.
print(paste0("Starting update operation, blocks to add: : ",stopBlock -maxBlockInDB ))

# get new blocks from chain and write to database    
for (i in startBlock:stopBlock){
  # get block i from chain
  block <- eth_getBlockByNumber(getHex(i), TRUE)
  if(is.null(block))
    break
  
  # convert to database format and save in database.
  rBlock <- enrichETHblock(block)
  DBI::dbExecute(dbHandle, "INSERT INTO eth_blocks VALUES (?,?,?,?,?,?,?,?,?)", unname(as.list(rBlock) ))

  # once every 500 blocks show status message 
  if(i %% 10 == 1){
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(paste0("Current block : ",i," Operation took:", as.numeric(time.taken)   ))
    start.time <- Sys.time()
  }
}
# when done show status message again.
print(paste0("Done! added: : ",stopBlock - maxBlockInDB," blocks" ))


#
# Aggregation 
#

# get latest aggregated value
# connect to database and retrieve latest block
maxAggInDB <- dbGetQuery(dbHandle, "select day from eth_daily order by day desc limit 1")

# when noting is found fetch from start
if(nrow(maxAggInDB)==0)
  maxAggInDB <- as.numeric(as.POSIXct('2015-07-28', tz = 'GMT'))


# retrieve unaggregated blocks from database
query <- paste0("select * from eth_blocks where timestamp >= '", maxAggInDB, "' order by number asc")
notAggregatedBlocks <- dbGetQuery(dbHandle, query)


notAggregatedBlocks$timestamp <- as.POSIXct(notAggregatedBlocks$timestamp, origin = '1970-01-01', tz = 'GMT')
notAggregatedBlocks$difficulty <- as.numeric(notAggregatedBlocks$difficulty)

# determine over how many days the blocks are spread and print
days <- round(as.numeric(difftime(max(notAggregatedBlocks$timestamp), min(notAggregatedBlocks$timestamp), units = "days")),0)
print(paste0("Got ",nrow(notAggregatedBlocks), " blocks spread over ",days, " days" ))

# aggregate block data 
dailyDifficulty <- notAggregatedBlocks %>% 
  dplyr::mutate(toTimestamp = dplyr::lead(timestamp))   %>%
  dplyr::mutate(duration = as.numeric(difftime(toTimestamp,timestamp, units = "secs"))) %>%
  dplyr::mutate(weigthedDifficulty = difficulty*duration)  %>%
  filter(!is.na(toTimestamp)) %>%
  dplyr::group_by(Day = as.Date(format(as.Date(timestamp, "%d/%m/%Y", tz=Sys.timezone())))) %>% 
  dplyr::summarise(netHash = sum(weigthedDifficulty)/sum(duration) / mean(duration), 
                   avgBlocktime = mean(duration),
                   ethFromBlocks = n()*5,
                   ethFromUnclesForMiner = sum(uncleETH),
                   ethFromUnclesForIncluder = sum(minerUncleReward),
                   ethFromtxFees = sum(txFees), 
                   ethTotal = sum(ethFromBlocks,ethFromUnclesForMiner,ethFromUnclesForIncluder,ethFromtxFees)) 


# retrieve unaggregated blocks from database
#dailyDifficulty$netHash <- as.integer64(dailyDifficulty$netHash)
dailyDifficulty$Day <- as.numeric(as.POSIXct(dailyDifficulty$Day))

# remove partial day 
deleteQuery = paste0("DELETE FROM eth_daily WHERE day >= ",maxAggInDB,"")
DBI::dbExecute(dbHandle, deleteQuery)
# add new aggregations
DBI::dbExecute(dbHandle, "INSERT INTO eth_daily VALUES (?,?,?,?,?,?,?,?)", unname(as.list(dailyDifficulty) ))
# retrieve daily aggregations and write to file.
query <- paste0("SELECT * FROM eth_daily ORDER BY day ASC")
aggregatedBlocks <- dbGetQuery(dbHandle, query)
aggregatedBlocks$day <- as.Date(as.POSIXct(aggregatedBlocks$day, origin='1970-01-01', tz='GMT'))

# RStudio filedir
# write to dashboard dir
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
saveRDS(aggregatedBlocks, paste0(dir, "/dashboard/data/daily.Rda"))
#write.csv(aggregatedBlocks,paste0(dir, "/dashboard/data/daily.csv"))


# clode db connection
dbDisconnect(dbHandle)



