pollutantmean <- function(directory, pollutant, id = 1:332) {
  path <- file.path(getwd(), directory, fsep = .Platform$file.sep)
  totals <- c()

  for (i in id) {
    if (i < 10) {
      data <-
        read.csv(paste(path, .Platform$file.sep, '00', i, '.csv', sep = ''))
    }

    else if (i < 100) {
      data <-
        read.csv(paste(path, .Platform$file.sep, '0', i, '.csv', sep = ''))
    }

    else {
      data <-
        read.csv(paste(path, .Platform$file.sep, i, '.csv', sep = ''))
    }

    bad <- is.na(data[pollutant])
    scores <- data[pollutant][!bad]
    totals <- append(totals, scores, after = length(totals))
  }
  pmean <- mean(totals)
  return(pmean)
}

