corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
		##Step 1 - From previously created function complete that generates list number of complete cases observations
		## import in varable.
		
	    if (directory == "specdata") 
			{
			list_of_nobs <- complete(directory)
			}
	    else {
          	  return("Incorrect input directory: please provide a correct input name specdata")
			}		
		## Step 2: Get IDs from all observations where observations are greater than threshold
		## and then calculate correlation of sulfate and nitrate
			ids = list_of_nobs[list_of_nobs$nobs > threshold, 1]
			corrrelation = numeric()
		## Loop through all the Ids greater than threshold
		for (i in ids) 
		{
			read_files = read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
						".csv", sep = ""))
			## Generate the one dimensional vector for the complete cases
			list_of_nobs = read_files[complete.cases(read_files), ]
			## Generate the correlation of sulfate and nitrate.
			corrrelation = c(corrrelation, cor(list_of_nobs$sulfate, list_of_nobs$nitrate))
		}
		return(corrrelation)
	}