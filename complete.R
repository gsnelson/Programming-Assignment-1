complete <- function(directory, id = 1:332) {
	library(dplyr)
    path <- file.path(getwd(), directory, fsep = .Platform$file.sep)
    print(path)
    print(id)
    id_list <- c()
    nobs_list <- c()

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

	    data.f <- filter(data, is.na(sulfate) == FALSE & is.na(nitrate) == FALSE)
	    good_count <- NROW(data.f)
	    id_list <- append(id_list, i, after = length(id_list))
	    nobs_list <- append(nobs_list, good_count, after = length(nobs_list))
	}

    complete_obs <- data.frame(id = id_list, nobs = nobs_list)
    return(complete_obs)
}

cr <- corr("specdata", 1000)
print(length(cr))
