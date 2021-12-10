# ------------------------------------------------------------
# Function to clean up the location names for merging 
# ------------------------------------------------------------

##function that takes three parameters: the dataset you want cleaned, and the two vectors we created above: 
strip_chars <- function(dt){
  
  ## vector dictionary of special characters to regular characters
  unwanted_array = list('ã'='a', 'é'="e", "Í"="I", "í"="i", "Ō"="O", "ō"="o", "á"="a", "Á"="A")
  
  # vector of characters or phrases to remove
  remove_chars <- c(" ", "[\u2018\u2019\u201A\u201B\u2032\u2035]","[\u201C\u201D\u201E\u201F\u2033\u2036]"
                    , "[[:punct:]]", "[^[:alnum:]]","\"", ",") 
  
  
  #Save an original copy of location name
  dt$orig_location <- copy(dt$location)

  ##remove special characters and blank spaces
  dt$location <- tolower(dt$location)
  dt$location <- gsub(paste(remove_chars, collapse="|"), "",dt$location)
  dt$location <- chartr(paste(names(unwanted_array), collapse=''),
                        paste(unwanted_array, collapse=''),
                        dt$location)
  return(dt)
}