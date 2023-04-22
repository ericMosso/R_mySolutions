# Reads the outcome-of-care-measures.csv file and returns a 2-column data frame
# containing the hospital in each state that has the ranking specified in num. 

# For example the function call rankall ( "heart attack", "best" ) would return 
# a data frame containing the names of the hospitals that are the best in their 
# respective states for 30-day heart attack death rates. 

# The function should return a value for every state (some may be NA). 

# The first column in the data frame is named hospital, which contains the 
# hospital name, and the second column is named state, which contains the 
# 2-character abbreviation for the state name. 

# Hospitals that do not have data on a particular outcome should be excluded 
# from the set of hospitals when deciding the rankings.

rankall <- function ( outcome, 
                      num = "best" ) {
    
    # Reads outcome data.
    
    dfoutcomes <- read.csv ( 'outcome-of-care-measures.csv' )
    
    
    # Check that state and outcome are valid.
    
    possible_outcomes <- c ( 'heart attack', 
                             'heart failure', 
                             'pneumonia' )
    
    # Verifying that the outcome argument is one of the valid choices.
    
    # If which(possible_outcomes is not equivalent to the outcome argument),
    # the the length() function will return 0, stopping the script and returning
    # an error message.
    
    if ( length 
         ( which 
             ( possible_outcomes == outcome ) ) == 0 ) {
        
        stop ( 'Invalid outcome' )
        
    }
    
    # Using the new R anonymous function shorthand to reduce code redundancy.
    
    # Any of the outcomes can be specified and the function will take care
    # of it and arrange the dataframe before returning the rows specified.
    
    rankfunc <- \ ( outcome, 
                    num, 
                    dfoutcomes ) {
        
        stateRankdf <- data.frame()
        
        stateRankdf <- setNames ( data.frame 
                                  ( matrix 
                                    ( ncol = 2, 
                                      nrow = 0 ) ), 
                                  c ( "Hospital", 
                                      "State" ) )
        
        # We need to set a counter to iterate properly below. 
        
        counter <- 1
      
        # We loop around each state in the R library of US state
        # abbreviations.
        
        for ( ab in state.abb ) {
            
            # We only read in the data where the state in dfoutcomes
            # matches the state specified by the user.
            
            stateOutcomes <- subset ( dfoutcomes, 
                                      State == ab )
            
            if ( outcome == 'heart attack' ) {
                
                stateOutcomes [ , 11 ] <- as.numeric ( 
                    as.character ( stateOutcomes [ , 11 ] ) )
                
                mortality_rate_30d <- stateOutcomes [ , 11 ]
                
            }
            
            if ( outcome == 'heart failure' ) {
                
                stateOutcomes [ , 17 ] <- as.numeric ( 
                    as.character ( stateOutcomes [ , 17 ] ) )
                
                mortality_rate_30d <- stateOutcomes [ , 17 ]
                
            }
            
            if ( outcome == 'pneumonia' ) {
                
                stateOutcomes [ , 23 ] <- as.numeric ( 
                    as.character ( stateOutcomes [ , 23 ] ) )
                
                
                mortality_rate_30d <- stateOutcomes [ , 23 ]
                
            }		
            
            hospital_name <- stateOutcomes [ , 2 ]
            
            hospital_state <- stateOutcomes [ , 7 ]
            
    
            rankdf <- data.frame ( hospital_name, 
                                   mortality_rate_30d,
                                   hospital_state )
            
            # Removes rows that have NA values.
            
            rankdf <- na.omit ( rankdf )
            
            # 2d ordering to sort and break ties.
            
            rankdf <- rankdf [ order ( 
              rankdf$mortality_rate_30d,
              rankdf$hospital_name ), ]
            
            # For times when we want a rank that is not actually the highest 
            # or the lowest.
            
            if ( num != "best" && num != "worst" ) {
                
                # The number of hospitals in a state might not be enough
                # to properly return the rank specified by the user.
                
                # In that case, we return an NA value for that state as a 
                # placeholder for the name of the hospital. 
                
                # Otherwise, returns the name of the hospital for that rank
                # and state.
              
              if ( num > length ( rankdf$hospital_name ) ) {
                
                stateRankdf [ counter, ] <- c ( NA, 
                                                rankdf$hospital_state [ 1 ] ) 
                
              } else {
                
                stateRankdf [ counter, ] <- c ( rankdf$hospital_name [ num ],
                                                rankdf$hospital_state [ num ] )
                
              }
              
            } 
            
            # Makes sure to return the top of the list for that state
            # because best was specified.
            
            if ( num == "best" ) {
              
              stateRankdf [ counter, ] <- c ( rankdf$hospital_name [ 1 ],
                                           rankdf$hospital_state [ 1 ] )
              
            }
            
            # Where the worst was specified, 
            # makes sure to return the last row. 
            
            if ( num == "worst" ) {
              
                # length() gives us the row we are after to find the "worst" ranked
                # hospital.
                    
              worstNum <- length ( rankdf$hospital_name )
              
              
              stateRankdf [ counter, ] 
              <- c ( rankdf$hospital_name [ worstNum ],
                     rankdf$hospital_state [ worstNum ] )
              
            }
            
            # Iterates the counter up one to keep the ball rolling.
            
            counter <- counter + 1
    
        }
        
        # Returns the dataframe that ranks the hospitals by state.
        
        stateRankdf
        
    }
    
    # Call function.
    
    rankfunc ( outcome, 
               num, 
               dfoutcomes )
    
    # Below are just my references to the CSV file column numbers for reference.
    
        ## 11 - heart attack
        ## 23 - pneumonia
        ## 17 - heart failure
        ## 2 - hospital name
        ## 7 - State
    
}