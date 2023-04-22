
# The function reads the outcome-of-care-measures.csv file and 
# returns a character vector with the name of the hospital that 
# has the ranking specified by the num argument.

# num = "best" is the default argument for num.

rankhospital <- function ( state, 
                           outcome, 
                           num = "best" ) {
    
    # Reads outcome data.
    
    dfoutcomes <- read.csv ( 'outcome-of-care-measures.csv', 
                             colClasses = 'character' )
    
    # Checks that state and outcome are valid.
    
    if ( length 
         ( which 
             ( state.abb == state ) ) == 0 )
        
        stop ( 'Invalid state' )
    
    possible_outcomes <- c ( 'heart attack', 
                             'heart failure', 
                             'pneumonia' )
    
    if ( length 
         ( which 
             ( possible_outcomes == outcome ) ) == 0 )
        
        stop ( 'Invalid outcome' )
    
    # Need to filter this data frame for this specific state.
    
    # Retaining all columns but only returning rows where states 
    # in the  State column == the state string function argument.
    
    dfoutcomes <- subset ( dfoutcomes, State == state )
    
    # Essentially, similar to best.R's bestfunc function,
    # read comments within function for differences/details. 
    
    rankfunc <- \ ( outcome, 
                    num, 
                    dfoutcomes ) {
      
      if ( outcome == 'heart attack' ) {
        
        dfoutcomes [ , 11 ] <- as.numeric ( 
          as.character ( dfoutcomes [ , 11 ] ) )
        
        mortality_rate_30d <- dfoutcomes [ , 11 ]
        
      }
        
      if ( outcome == 'heart failure' ) {
        
        dfoutcomes [ , 17 ] <- as.numeric ( 
          as.character ( dfoutcomes [ , 17 ] ) )
        
        mortality_rate_30d <- dfoutcomes [ , 17 ]
        
      }
        
      if ( outcome == 'pneumonia' ) {
        
        dfoutcomes [ , 23 ] <- as.numeric ( 
          as.character ( dfoutcomes [ , 23 ] ) )
        
        mortality_rate_30d <- dfoutcomes [ , 23 ]
        
      }
    
      # Creating a list of hospital names in the subset. 
        
      hospital_name <- dfoutcomes [ , 2 ]
      
      # Making a data frame from the lists we created.
      
      rankdf <- data.frame ( hospital_name, 
                             mortality_rate_30d )
      
      # Removing rows from the dataframe which have NA values
      # in any of the columns. 
      
      rankdf <- na.omit ( rankdf )
      
      # Ordering the dataframe by mortality rate, the hospital names 
      # will then be alphabetized to break any ties. 
      
      rankdf <- rankdf [ order ( 
        rankdf$mortality_rate_30d,
        rankdf$hospital_name ), ]
      
      # If the num argument is reasonable, we go ahead and return the rows
      # that are indicated by the function argument, "num."
      
      # If the "best" is selected, we want to only return one row.
      
      if(num == "best") {
        
        num <- 1
        
        # To break a tie, we need to use the head() function to return 
        # only the top row from the subset to find the highest ranked hospital.
        
        return ( head ( rankdf [[ 1 ]], 1 ) )
        
      }
      
      # If the "worst" is selected, we want to to only return one row as well.
      
      if(num == "worst") {
        
        # So instead of the top row from the subset (if there is a tie),
        # we return the bottom row to find the lowest ranked hospital.
          
        num <- length(rankdf$hospital_name)
        
        return ( tail ( rankdf [[ 1 ]], 1 ) )
        
      }
      
      # If we get this far, then if num is in between the best and worst
      # we return the rank argument index corresponding to 'num' argument. 
      
      if ( num != "best" || num != "worst" ) 
        
        return ( rankdf [[ 1 ]] [ num ] ) 
      
      
      # Checking if the num argument makes reasonable demands on the dataset.
      
      # If num, the rank specified, is greater than the list of hospitals
      # in the dataframe, there will be an error.
      
      if ( num > length ( rankdf$hospital_name ) || 
           num <= 0 )
        
        stop ( 'NA' )
      
    }
    
    # A call to the anonymous function above.
    
    rankfunc ( outcome, 
               num, 
               dfoutcomes )
    
    # Below are just my references to the CSV file column numbers for reference.
        
        # 11 - heart attack
        # 23 - pneumonia
        # 17 - heart failure
        # 2 - hospital name
        # 7 - State
    }