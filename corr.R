corr <- function(directory, threshold = 0) {
      file <- list.files(directory, full.names = TRUE)
      nobs <- vector(mode = "integer", length = length(file))
      for(i in seq_along(file)) {
            nobs[i] <- nrow(read.csv(file[[i]])[complete.cases(read.csv(file[[i]])), ])
      }
      compare <- nobs > threshold
      filter <- file[compare]
      data <- vector(mode = "list", length = length(filter))
      for(j in seq_along(filter)) {
            data[[j]] <- read.csv(filter[[j]])[complete.cases(read.csv(filter[[j]])), ]
      }
      correl <- vector(mode = "numeric", length = length(filter))
      for(j in seq_along(filter)) {
            correl[j] <- cor(data[[j]][, "sulfate"], data[[j]][, "nitrate"])
      }
      print(correl)
}