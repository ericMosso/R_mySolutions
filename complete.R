complete <- function ( directory, id = 1:332 ) {
    
    ## 'directory' is a character vector of length 1  
    ## indicating the location of the CSV files
    
    # Saving the original working directory to be reset after reading in the 
    # specdata files. 
    
    wd1 <- getwd()
    
    # A list of files names in the folder containing our files of interest.
    
    fileList <- list.files ( directory )
    
    # Creating an empty list to hold the sums of the files below.
    
    sum_of_rows <- list ()
    
    # Setting the working directory containing the CSV files. 
    
    setwd ( directory )
    
    ## 'id' is an integer vector indicating the
    ## monitor ID numbers to be used
    
    # using 'sapply()' because 'fileList' is a character string
    
    dfList <- lapply ( fileList[id], 
                       read.csv )
    
    # reset the working directory
    
    setwd ( wd1 )
    
    # Need to make a length placeholder for the below 'for' loop.
    
    lenVar <- length(id)
    
    # Vector that will hold a vector from 1 to the number of files to be read
    # for complete cases below.
    
    lenVect <- 1:lenVar
    
    ## using complete.cases()
    
    for ( i in lenVect ) {
    
        # 'complete.cases' creates a dataframe of logical values
        # if no NAs are detected in the row within the file
        # in dfList, then it is a complete case, so the value will be TRUE
        
        logicalFrame <- complete.cases ( as.data.frame ( dfList[i] ) )
        
        # The numerical value for TRUE is 1, so summing the number of TRUES
        # will give us the amount of complete rows in each file. 
        
        sum_of_rows <- append ( sum_of_rows, 
                                sum ( logicalFrame, 
                                      na.rm = TRUE ) )
        
    }
    
    # Flattening the list of lists to a simple vector
    
    nobs <- unlist ( sum_of_rows )
    
    # Using the two vectors as columns in the new dataframe
    
    ccFrame <- data.frame ( id, nobs )
    
    # Return the dataframe
    ccFrame

}