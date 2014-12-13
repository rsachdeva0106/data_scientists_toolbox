pollutantmean <- function (directory, pollutant, id = 1:332) {
         ## 'directory' is a character vector of length 1 indicating
		 ## the location of the CSV files
         ## 'pollutant' is a character vector of length 1 indicating
         ## the name of the pollutant for which we will calculate the
         ## mean; either "sulfate" or "nitrate".
         ## 'id' is an integer vector indicating the monitor ID numbers
         ## to be used
         ## Return the mean of the pollutant across all monitors list
         ## in the 'id' vector (ignoring NA values)
         
	## Declare Return Variable and assign a value 0 to it.

	    ret_mean <- 0

         ##Step 1: Get the list of files from specified input directory, Also check if it is "specdata"
		 directory <- c(tolower(directory) )
		## If directory name is not "specdata" it should return with a message that it is an incorrect input directory
	    if (directory == "specdata") 
			{
					    list_files <- list.files(directory,full.names=TRUE)
			}
	    else {
          	  return("Incorrect input directory: please provide a correct input name specdata")
			}

         ##Step 2: Create a dataframe for the ID value by traversing through the iterator id.
	    fetch_data <- data.frame()
		## If id is not in valid range of 1 to 332 then also return a message to user that id not in valid inpput range
		if (id >= 1 && id <= 332) 
		    {
				for (i in id) 
					{
					fetch_data <- rbind(fetch_data, read.csv(list_files[i]))
					}	
            }
		else{
				return("Incorrect range for input parameter id. Please enter valid range between 1 - 332")
			}
          ##Step 3: Calculate Mean on the basis of input pollutant i.e. check if pollutant is sulfate or nitrate
		if (tolower(pollutant) == 'sulfate') 
		{
			ret_mean <- mean(fetch_data$sulfate, na.rm = TRUE)
		} 
		else if (tolower(pollutant) == 'nitrate') 
		{
			ret_mean <- mean(fetch_data$nitrate, na.rm = TRUE)
		}
		else 
		{
			return("Pollutant should either be sulfate or nitrate")
		}	  
        return(ret_mean)
}

