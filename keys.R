## W2: specdata codes:

pollutantmean <- function(directory, pollutant, id = 1:332) {
      file <- list.files(directory, full.names = TRUE)
      dat <- vector(mode = "list", length = length(file))
      for (i in seq_along(file)) {
            dat[[i]] <- read.csv(file[i])
      }
      output <- do.call(rbind, dat)
      output_subset <- output[which(output[, "ID"] %in% id),]
      mean(output_subset[, pollutant], na.rm = TRUE)
}
complete <- function(directory, id = 1:332) {
      file <- list.files(directory, full.names = TRUE)
      case <- vector(mode = "integer", length = length(file))
      for(i in seq_along(file)) {
            case[i] <- nrow(read.csv(file[i])[complete.cases(read.csv(file[i])), ])
      }
      nobs <- case[id]
      data.frame(id, nobs)
}
corr <- function(directory, threshold = 0) {
      file <- list.files(directory, full.names = TRUE)
      nobs <- vector(mode = "integer", length = length(file))
      for(i in seq_along(file)) {
            nobs[i] <- nrow(read.csv(file[i])[complete.cases(read.csv(file[i])), ])
      }
      compare <- nobs > threshold
      filter <- file[compare]
      data <- vector(mode = "list", length = length(filter))
      for(j in seq_along(filter)) {
            data[[j]] <- read.csv(filter[j])[complete.cases(read.csv(filter[j])), ]
      }
      correl <- vector(mode = "numeric", length = length(filter))
      for(j in seq_along(filter)) {
            correl[j] <- cor(data[[j]][, "sulfate"], data[[j]][, "nitrate"])
      }
      print(correl)
}



## W3: Matrix caching code with explanation:

## The project will return the inverse of an invertible matrix..
## The result is cached so that it can be used in future computation.

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() {x}
      setInverse <- function(inverse) {i <<- inverse}
      getInverse <- function() i
      list(set = set, get = get, setInverse = setInverse,
           getInverse = getInverse)
}


## This function computes the inverse of the matrix returned by the function
## above. If the inverse is calculated previously, this cacheSolve function
## will retrieve the result from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      i <- x$getInverse()
      if(!is.null(i)) {
            message("data retrieved from cache")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setInverse(i)
      i
}



## W4: Hospital assessment code:

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
rankhospital <- function(state, outcome, num = "best") {
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
      if(num == "best") {
            num <- 1L
      }
      if(num == "worst") {
            num <- nrow(result)
      }
      result[num, "hospital"]
}
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