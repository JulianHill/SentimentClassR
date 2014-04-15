Main <- function(file, api_key,token_diff,token_shares)
{

URLS <- read.csv(header = TRUE, file = file)
  number <- nrow(URLS)

df = data.frame(text = 1:number, sentiment=1:number, subject=1:number, topic=1:number,date=1:number,length=1:number,shares=1:number, stringsAsFactors=FALSE)



for (i in 1:number)
{

url <- toString(URLS[i,1])

tmp <- NewsClass(api_key,token_diff,token_shares,url)

 df$text[i] = tmp$text
 df$sentiment[i] = tmp$sentiment
 df$subject[i] = tmp$subject
 df$topic[i] = tmp$topic

 df$date[i] = tmp$date
 df$length[i] = tmp$length
 df$shares[i] = tmp$shares

}







  #Put the df frame (text_df) in the rCharts table

library(rCharts)
tab2=dTable(df[], sPaginationType = "full_numbers")
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




NewsClass <- function(api_key,token_diff,token_shares,url) {

library(RCurl)
library(RJSONIO)
library(stringr)
library(XML)


# download html
html <- getURL(toString(url), followlocation = TRUE)
 
# parse html
doc = htmlParse(html, asText=TRUE)
plain.text <- xpathSApply(doc, "//p", xmlValue)
txt <- paste(plain.text, collapse = "\n")


 
# get mood probability
article_text = txt





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

return(text_df)



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


if(exists("js$date")){
  date = js$date
  return(date)
	
}
else{
return("NA")

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


