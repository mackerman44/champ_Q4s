# load data
load_data = function(ls) {
  
  loadRData = function(fileName) {
    # loads an RData file, and returns it
    load(fileName)
    get(ls()[ls() != "fileName"])
  }
  
  # load dataset according to life stage
  if(ls == "sum") {
    ls_df = loadRData("../../QRFcapacity/data/fh_sum_champ_2017.rda") # summer juvenile parr paired fish/CHaMP habitat data through 2017
  }
  if(ls == "win") {
    ls_df = loadRData("../../QRFcapacity/data/fh_win_champ_2017.rda") # winter juvenile presmolts paired fish/CHaMP habitat data through 2017
  }
  if(ls == "spw") {
    ls_df = loadRData("../../QRFcapacity/data/fh_redds_champ_2017.rda") # paired redd/CHaMP habitat data through 2017
  }
  
  return(ls_df)
}