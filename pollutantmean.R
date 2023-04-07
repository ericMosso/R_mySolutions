# "Calculates the mean of a pollutant (sulfate or nitrate) across 
# a specified list of monitors. 

# The function 'pollutantmean' takes three arguments: 
# 'directory', 'pollutant', and 'id'. 

# Given a vector monitor ID numbers, 'pollutantmean' reads that 
# monitors' particulate matter data from the directory specified 
# in the 'directory' argument and returns the mean of the pollutant 
# across all of the monitors, ignoring any missing values coded as NA."

pollutantmean <- function ( directory, 
                            pollutant, 
                            id = 1:332 ) {
    
    # variable that will reset working directory to the current
    # before the function runs
    
    pollutes <- c ()
    
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the 'specdata' folder that contains .CSV files


    fileList <- list.files ( directory )
    
    # Saving the original working directoy before setting the 
    # the working directory to the CSV file folder.
    
    wd1 <- getwd ()
    
    setwd ( directory )
    
    # Iterating over the id vector to read in the specified file data.
    
    for ( i in id ) {
        
        data <- read.csv ( fileList [i], 
                           header = TRUE )
        
        # 'pollutant' is a character vector of length 1 indicating
        # the name of the pollutant for which we will calculate the mean;
        # either "sulfate" or "nitrate".
        
        pollutes <- c ( pollutes, 
                        data [, pollutant] )
    } 
    
    # Resetting back to the prev saved working directory.
    
    setwd ( wd1 )
    
    # Applying mean(), 
    # and removing NA's
    
    pollutesMean <- mean ( pollutes, 
                           na.rm = TRUE )
    
    # Returning the mean of the pollutant across the specified monitors.
    
    pollutesMean
  
}