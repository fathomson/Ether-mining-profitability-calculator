# Ether mining profitability calculator 

## Instructions
#### data_prep.R
R script too: 
1. Retrieve block and transaction data from ether chain and convert to custom objects
2. Save custom objects to sql
3. Retrieve custom objects from sql and aggregate per day and write this to Rda file which is used in dashboard.

#### dashboard/app.R
Calcultes your expected returns based on your mining efforts, for either cloud or local mining. Based on the data collected with data_prep.R the app.R forecasts the net hash rate and amount of ether that is rewarded to the miners. Together with your hashrate and investment app.R predicts your expected earnings. 
For a working version check: https://ethereum.shinyapps.io/dashboard/