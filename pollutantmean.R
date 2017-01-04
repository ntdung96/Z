pollutantmean <- function(directory, pollutant, id = 1:332) {
      file <- list.files(directory, full.names = TRUE)
      dat <- vector(mode = "list", length = length(file))
      for (i in seq_along(file)) {
            dat[[i]] <- read.csv(file[[i]])
      }
      output <- do.call(rbind, dat)
      output_subset <- output[which(output[, "ID"] %in% id),]
      mean(output_subset[, pollutant], na.rm = TRUE)
}