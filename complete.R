complete <- function(directory, id = 1:332) {
         ## 'directory' is a character vector of length 1 indicating
         ## the location of the CSV files
		## 'id' is an integer vector indicating the monitor ID numbers
		## to be used
		## Return a data frame of the form:
		## id nobs
		## 1 117
		## 2 1041
		## ...
		## where 'id' is the monitor ID number and 'nobs' is the
		## number of complete cases

         ##Step 1: Get the list of files from specified input directory, Also check if it is "specdata"
		 directory <- c(tolower(directory) )
		 id        <- c(id)
		 ## If directory name is not "specdata" it should return with a message that it is an incorrect input directory
	    if (directory == "specdata") 
			{
					    list_files <- list.files(directory,full.names=TRUE)
			}
	    else {
          	  return("Incorrect input directory: please provide a correct input name specdata")
			}

        ##Step 2: Create a dataframe for the ID value by traversing through the iterator id.
	    ## fetch all data from all input files
 		fetch_data <- data.frame() 
		##fetch only the complete cases for the input files
		complete_cases <- data.frame()
		##number of observations for the data frame
		nobs <- data.frame()
		## If id is not in valid range of 1 to 332 then also return a message to user that id not in valid inpput range
		if (id >= 1 && id <= 332) 
		    {
				for (i in id) 
					{
					fetch_data <- read.csv(list_files[i])
					nobs <- sum(complete.cases(fetch_data))
					complete_cases <- rbind(complete_cases,data.frame(i,nobs))
					}	
            }
		else{
				return("Incorrect range for input parameter id. Please enter valid range between 1 - 332")
			}
			return (complete_cases)
}
