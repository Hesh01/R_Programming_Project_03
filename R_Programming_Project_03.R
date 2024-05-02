#Plot the 30-day mortality rates for heart attack

data<-download.file('https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip',destfile="Hospital.zip")
data<-unzip("Hospital.zip")

data<-read.csv("hospital-data.csv")
data<-read.csv("outcome-of-care-measures.csv")

library("data.table")
  
# Reading in data
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

# Convert the 30-day mortality rates to numeric
outcome[, 11] <- as.numeric(outcome[, 11])

# Plot the histogram of 30-day death rates from heart attack
hist(outcome[, 11], main = "Histogram of 30-day Death Rates from Heart Attack", xlab = "30-day Death Rate", ylab = "Frequency",col=2)



#Finding the best hospital in a state


best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!state %in% unique(data$State)){
    stop('Invalid state.')
  } else if (!tolower(outcome) %in% c("heart attack", "heart failure", "pneumonia")){
    stop('Invalid outcome.')
  } else {
    # Filter data for the specified state and outcome
    state_data <- subset(data, State == state)
    
    # Extract the outcome measure column based on the input
    outcome_column <- switch(tolower(outcome),
                             "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                             "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                             "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    
    # Convert outcome measure column to numeric
    state_data[, outcome_column] <- as.numeric(state_data[, outcome_column])
    
    # Find hospital(s) with the lowest 30-day death rate
    min_value <- min(state_data[, outcome_column], na.rm = TRUE)
    best_hospitals <- state_data$Hospital.Name[state_data[, outcome_column] == min_value]
    
    # Sort hospitals if needed
    best_hospitals <- sort(best_hospitals)
    
    return(best_hospitals)
  }
}

#Ranking hospitals by outcome in a state

# Read outcome data outside the function
outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

rankhospital <- function(state, outcome, rank = "best") {
  # Check that state and outcome are valid
  if (!state %in% unique(outcome_data$State)) {
    stop('Invalid state')
  }
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop('Invalid outcome')
  }
  # Filter data for the specified state and outcome
  state_data <- subset(outcome_data, State == state)
  
  # Extract outcome column index
  outcome_col_index <- switch(outcome,
                              "heart attack" = 11,
                              "heart failure" = 17,
                              "pneumonia" = 23)
  
  # Convert outcome column to numeric
  state_data[, outcome_col_index] <- as.numeric(state_data[, outcome_col_index])
  
  if (is.numeric(rank)) {
    # Rank based on numeric rank
    sorted_data <- state_data[order(state_data[, outcome_col_index], state_data$Hospital.Name), ]
    output <- sorted_data$Hospital.Name[rank]
  } else if (rank == "best") {
    # Find best hospital
    best_hospital <- best(state, outcome)
    output <- best_hospital[1]  # If multiple hospitals have the same best score, return the first one
  } else if (rank == "worst") {
    # Find worst hospital
    sorted_data <- state_data[order(state_data[, outcome_col_index], state_data$Hospital.Name, decreasing = TRUE), ]
    output <- sorted_data$Hospital.Name[1]
  } else {
    stop('Invalid rank')
  }
  
  return(output)
}



# Ranking hospitals in all states

# Read outcome data outside the function
outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

rankall <- function(outcome, num = "best") {
  # Check that outcome is valid
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop('Invalid outcome')
  }
  # Filter data for the specified outcome
  state_data <- outcome_data[, c("Hospital.Name", "State", outcome)]
  state_data[, outcome] <- as.numeric(state_data[, outcome])
  
  if (is.numeric(num)) {
    # Rank based on numeric num
    ranked_data <- state_data[order(state_data[, outcome]), ]
    output <- ranked_data[num, ]
  } else {
    # Calculate best or worst hospitals for each state
    aggregate_fun <- ifelse(num == "best", which.min, which.max)
    agg_result <- aggregate(state_data[, outcome], by = list(state_data$State), FUN = aggregate_fun)
    colnames(agg_result) <- c("State", "Rank", "Hospital.Name")
    output <- agg_result
  }
  
  return(output)
}
