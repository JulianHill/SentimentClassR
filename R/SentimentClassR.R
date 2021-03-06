

NewsClass <- function(api_key,token_diff,token_shares,url) {

library(RCurl)
library(RJSONIO)
library(stringr)
library(XML)


# Read and parse HTML file
doc.html = htmlTreeParse(url,
           useInternal = TRUE)
 
# Extract all the paragraphs (HTML tag is p, starting at
# the root of the document). Unlist flattens the list to
# create a character vector.
doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))
 
# Replace all \n by spaces
doc.text = gsub('\\n', ' ', doc.text)
 
# Join all the elements of the character vector into a single
# character string, separated by spaces
doc.text = paste(doc.text, collapse = ' ')

 
# get mood probability
article_text = doc.text





 text_clean = article_text

 text_short = substr(article_text, 0, 359)

text_num = 1

text_df = data.frame(text = text_short, sentiment=1:text_num, subject=1:text_num, topic=1:text_num,date=1:text_num,length=1:text_num,shares=1:text_num, stringsAsFactors=FALSE)


# apply function getSentiment
sentiment = rep(0, text_num)
for (i in 1:text_num)
{
tmp = getSentiment(text_clean[i], api_key)


 
 text_df$sentiment[i] = tmp$sentiment
 text_df$subject[i] = tmp$subject
 text_df$topic[i] = tmp$topic

 text_df$date[i] = toString(getDate(url,token_diff))
 text_df$length[i] = sapply(gregexpr("\\W+", text_clean), length) + 1
 text_df$shares[i] = getShares(url,token_shares)

}


#Put the df frame (text_df) in the rCharts table

library(rCharts)
tab2=dTable(text_df[], sPaginationType = "full_numbers")
tab2$templates$script =  "http://timelyportfolio.github.io/rCharts_dataTable/chart_customsort.html" 

tab2$params$table$aoColumns =
  list(
    list(sType = "string_ignore_null", sTitle = "text"),
    list(sType = "string_ignore_null", sTitle = "sentiment"),
    list(sType = "string_ignore_null", sTitle = "subject"),
    list(sType = "string_ignore_null", sTitle = "topic"),
    list(sType = "string_ignore_null", sTitle = "date"),
    list(sType = "string_ignore_null", sTitle = "length"),
    list(sType = "string_ignore_null", sTitle = "shares")
  )

tab2$save("output.html", cdn = TRUE)


}


getShares <- function(url, token_shares)
{

  data <- getURL(paste("http://api.sharedcount.com/?apikey=", token_shares, "&url=",url ,sep=""))
 
js <- fromJSON(data, asText=TRUE);

shares <- js$Facebook[[3]] + js$Twitter[[1]]
 
# get mood probability
date = toString(shares)


}


getDate <- function(url,token)
{

	
	data <- getURL(paste("http://api.diffbot.com/v2/analyze?token=", token, "&url=",url,"&date" ,sep=""))
 
js <- fromJSON(data, asText=TRUE);
 
# get mood probability
date = toString(js$date)

if(length(date)==1)
{
	return("NA")
}
else{
return(date)

}
}



getSentiment <- function (text, key){
 
 
 
text <- URLencode(text);
 
#save all the spaces, then get rid of the weird characters that break the API, then convert back the URL-encoded spaces.
text <- str_replace_all(text, "%20", " ");
text <- str_replace_all(text, "%\\d\\d", "");
text <- str_replace_all(text, " ", "%20");
 
 
if (str_length(text) > 360){
text <- substr(text, 0, 359);
}
##########################################
 
data <- getURL(paste("http://api.datumbox.com/1.0/TwitterSentimentAnalysis.json?api_key=", key, "&text=",text, sep=""))
 
js <- fromJSON(data, asText=TRUE);
 
# get mood probability
sentiment = js$output$result
 
###################################
 
data <- getURL(paste("http://api.datumbox.com/1.0/SubjectivityAnalysis.json?api_key=", key, "&text=",text, sep=""))
 
js <- fromJSON(data, asText=TRUE);
 
# get mood probability
subject = js$output$result
 
##################################
 
data <- getURL(paste("http://api.datumbox.com/1.0/TopicClassification.json?api_key=", key, "&text=",text, sep=""))
 
js <- fromJSON(data, asText=TRUE);
 
# get mood probability
topic = js$output$result
 

 

 
return(list(sentiment=sentiment,subject=subject,topic=topic))
}


clean.text <- function(some_txt)
{
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
some_txt = gsub("@\\w+", "", some_txt)
some_txt = gsub("[[:punct:]]", "", some_txt)
some_txt = gsub("[[:digit:]]", "", some_txt)
some_txt = gsub("http\\w+", "", some_txt)
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)
 
# define "tolower error handling" function
try.tolower = function(x)
{
y = NA
try_error = tryCatch(tolower(x), error=function(e) e)
if (!inherits(try_error, "error"))
y = tolower(x)
return(y)
}
 
some_txt = sapply(some_txt, try.tolower)
some_txt = some_txt[some_txt != ""]
names(some_txt) = NULL
return(some_txt)
}
