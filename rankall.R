rankall <- function(outcome, num = "best") {
      data <- read.csv("outcome-of-care-measures.csv", 
                       na.strings = "Not Available",
                       stringsAsFactors = FALSE)[, c(2, 7, 11, 17, 23)]
      names(data) <- c("hospital", "state", "heart attack",
                       "heart failure", "pneumonia")
      if(!outcome %in% names(data)[c(3, 4, 5)]) {
            stop("invalid outcome\n")
      }
      filtered <- data[, c("hospital", "state", outcome)]
      complete <- filtered[complete.cases(filtered), ]
      result <- complete[order(complete$state, complete[, outcome],
                               complete$hospital), ]
      split.data <- split(result, result$state)
      if(num == "worst") {
            final.split <- lapply(split.data, function(fx) tail(fx, 1)[, 1:2])
            output <- do.call(rbind, final.split)
      } else {
            if(num == "best") {
                  num <- 1L
            }
            final.split <- lapply(split.data, function(fx) fx[num, 1:2])
            output <- do.call(rbind, final.split)
      }
}