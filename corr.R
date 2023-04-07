# "Threshold indicates the number of complete observations in a file.

# Takes a directory of data files and a threshold for complete cases 
# and calculates the correlation between sulfate and nitrate for monitor 
# locations where the number of completely observed cases (on all variables) 
# is greater than the threshold. 

# The function should return a vector of correlations for the monitors 
# that meet the threshold requirement. 

# If no monitors meet the threshold requirement, then the function 
# should return a numeric vector of length 0."

corr <- function ( directory, 
                   threshold = 0 ) {
     
    # Saving the original working directory to a variable. 
    
     wd1 <- getwd()
     
     # List of file names to read with lapply().
     
     fileList <- list.files ( directory )
     
     # Sets the working directory to the CSV file folder.
     
     setwd ( directory )
     
     # Reads all csv files identified in the fileList. 
     
     dfList <- lapply ( fileList, 
                        read.csv ) 
    
     # Resetting to the original working directory.
     
     setwd ( wd1 )
     
     # Creates a comprehensive list of all monitor IDs
     # and no of complete obs.
     
     completesFrame <- complete ( directory, 
                                  1:332 )
     
     # In a way, this filters the dataframe 'completesFrame'
     # and creates a new one that has only data from files
     # that meet or exceed the minimum threshold.
     
     meets_threshold <- subset ( completesFrame, 
                                 nobs >= threshold, 
                                 select = c ( id, nobs ) )
     
     
     # Vector of the id's that meet the threshold reqs
     
     id_vector <- meets_threshold$id

     # Vector to hold the placeholder values 'corlValue'
     
     corlVect <- c ()
     
     # Iterates over the list of identity or file 'id' values in our 'id_vector'
      
     for ( i in id_vector ) {
         
         # A placeholder variable to hold the correlation 
         # between the two pollutant values
         # monitors meeting the threshold reqs. 
         
         corlValue <- cor ( dfList[[i]]$nitrate,
                            dfList[[i]]$sulfate,
                            use = 'na.or.complete',
                            method = c ( 'pearson' ) )
         
        # Appends the placeholder value to the vector of correlations.
         
         corlVect <- append ( corlVect, 
                              corlValue )

     }
     
     # Returns the vector of correlations.
     
     corlVect
}