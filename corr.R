corr <- function(directory, threshold = 0) {
    library(dplyr)
    path <- file.path(getwd(), directory, fsep = .Platform$file.sep)
    results <- numeric()

    files <- c(list.files(path , pattern = "csv"))
    # print(files)
    for (i in files) {
        data <-
                read.csv(paste(path, .Platform$file.sep, i, sep = ""))
        data.f <- filter(data, is.na(sulfate) == FALSE & is.na(nitrate) == FALSE)
        good_count <- NROW(data.f)
        if (good_count > threshold) {
            xcol <- data.f$sulfate
            ycol <- data.f$nitrate
            correl <- cor(xcol, ycol)
            # print(head(correl))
        results <- append(results, correl, after = length(results))

        }

        }
    return(results)
}

cr <- corr("specdata")
cr <- sort(cr)
RNGversion("3.5.1")
set.seed(868)
out <- round(cr[sample(length(cr), 5)], 4)
print(out)
