

# function to grab the most recent file in a folder 
  # Input: working directory (e.g. "workingdirectory/more/more/path") and data type (e.g. ".Rda" or ".csv")
  # Output: dataframe of most recently created file (can be an .Rda object instead of df)
  grab_recent<-function(wd, data_type){
    # set working directory 
    setwd(wd)
    # create list of files with correct data type 
    file_list<-list.files(pattern=paste(data_type))
    # Find hte file info 
    file_info<-file.info(file_list)
    # Find the index for the most recently created file 
    most_recent_index<-which.max(file_info$mtime)
    # Select the name for the most recent file 
    most_recent_file<-file_list[most_recent_index]
    # Now read it in 
    if(data_type==".csv"){
      # load the csv file into the data_frame object 
      grab_recent_out<-read.csv(paste0(most_recent_file))
    }else if(data_type==".Rda"){
      # create temp environment 
      temp_env<-new.env()
      # load the .Rda file into the temporary environment 
      load(paste0(most_recent_file), envir=temp_env)
      # Find the name of the .Rda object once it is uploaded 
      loaded_object<-ls(temp_env)
      # Put the loaded .Rda object into the output
      grab_recent_out<-temp_env[[paste(loaded_object)]]
    }else {
      print("Error, cannot find data type")
      grab_recent_out<-"Error cannot find data type"
    }
    return(grab_recent_out)
  }
  
  
