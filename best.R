# The function reads the outcome-of-care-measures.csv file 
# and returns a character vector with the name of the hospital 
# that has the best (i.e. lowest) 
# 30-day mortality for the specified outcome in that state.

# If there is a tie for the best hospital for a given outcome, 
# then the hospital names should be sorted in alphabetical order 
# and the first hospital in that set should be chosen 
# (i.e. if hospitals “b”, “c”, 
# and “f” are tied for best, then hospital “b” should be returned).

best <- function ( state, outcome ) {
  
    # Reads outcome data.
    
    dfoutcomes <- read.csv ( 'outcome-of-care-measures.csv', 
                           colClasses = 'character' )
    
    # Check that state and outcome are valid.
    
    if ( length 
       ( which 
         ( state.abb == state ) ) == 0 )
    
    stop ( 'Invalid state' )
  
    # The vector of valid outcomes that will be compared with
    # the outcome argument.
    
    possible_outcomes <- c ( 'heart attack', 
                           'heart failure', 
                           'pneumonia' )
    
    # Checking that the outcome argument is a valid input.
    # So if the length of the output from the which function
    # is 0, meaning that which() does match any of the possible_outcomes
    # with the outcome argument, then it stops the script and
    # returns an error message. 
    
    if ( length 
       ( which 
         ( possible_outcomes == outcome ) ) == 0 )
    
    stop ( 'Invalid outcome' )
    
    # This reduces the dataframe to only the State specified by the 
    # the 'state' argument. 
    
    dfoutcomes <- subset ( dfoutcomes, State == state )
    
    
    # Uses the new anonymous function syntax to reduce redundant code. 
    # Whichever outcome is chosen, this function can handle it. 
    
    bestfunc <- \ ( dfoutcomes,
                  state,
                  outcome ) {
    
    # Converting the outcome column in the dfoutcomes dataframe
    # to numeric type. This is so the order function further down
    # works. Same for the other if statements for the other outcomes.
      
    if ( outcome == 'heart attack' ) {
      
      dfoutcomes [ , 11 ] <- 
        as.numeric ( as.character( dfoutcomes [ , 11 ] ) )
    
      mortality_rate_30d <- dfoutcomes [ , 11 ]
    
    }
    
    if ( outcome == 'heart failure' ) {
      
      dfoutcomes [ , 17 ] <- 
        as.numeric ( as.character( dfoutcomes [ , 17 ] ) )
      
      mortality_rate_30d <- dfoutcomes [ , 17 ]
      
    }
    
    if ( outcome == 'pneumonia' ) {
      
      dfoutcomes [ , 23 ] <- 
        as.numeric ( as.character( dfoutcomes [ , 23 ] ) )
      
      mortality_rate_30d <- dfoutcomes [ , 23 ]
      
    }
    
    # Making a vector of hospital names from the dfoutcomes' 2nd column.
      
    hospital_name <- dfoutcomes [ , 2 ]
    
    # Creating a new dataframe with columns for the name of the best hospitals
    # and the associated mortality rates for the outcome specified of that 
    # hospital.
    
    best_hospital_df <- data.frame ( hospital_name,
                                     mortality_rate_30d )
    
    # Na.omit effectively removes the rows with any NA values.
    
    best_hospital_df <- na.omit ( best_hospital_df )
    
    # Now we order the best hospitals by their 30 day mortality rates
    # and then break mortality rate ties by alphabetization.
    
    orderedBest <- best_hospital_df [ order ( 
      best_hospital_df$mortality_rate_30d,
      best_hospital_df$hospital_name ), ]
    
    # The assignment only calls for the no. 1 best hospital,
    # so we return only the first row and column (hospital name).
    
    return ( orderedBest [ 1, 1 ] )
    
    }
    
    # A call to the function above. 
    
    bestfunc ( dfoutcomes, 
             state, 
             outcome )
    
    # These are just my quick references to the CSV file columns' numbers.
      # 11 - heart attack
      # 23 - pneumonia
      # 17 - heart failure
      # 2 - hospital name
      # 7 - State
    
}