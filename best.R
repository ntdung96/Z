best <- function(state, outcome) {
      data <- read.csv("outcome-of-care-measures.csv", 
                       na.strings = "Not Available",
                       stringsAsFactors = FALSE)[, c(2, 7, 11, 17, 23)]
      names(data) <- c("hospital", "state", "heart attack",
                       "heart failure", "pneumonia")
      if(!state %in% data$state) {
            stop("invalid state\n")
      }
      if(!outcome %in% names(data)[c(3, 4, 5)]) {
            stop("invalid outcome\n")
      }
      filtered <- data[which(data$state == state),
                       c("hospital", "state", outcome)]
      complete <- filtered[complete.cases(filtered), ]
      result <- complete[order(complete[, outcome], complete$hospital), ]
      result[1, "hospital"]
}