complete <- function(directory, id = 1:332) {
      file <- list.files(directory, full.names = TRUE)
      case <- vector(mode = "integer", length = length(file))
      for(i in seq_along(file)) {
            case[i] <- nrow(read.csv(file[i])[complete.cases(read.csv(file[i])), ])
      }
      nobs <- case[id]
      data.frame(id, nobs)
}