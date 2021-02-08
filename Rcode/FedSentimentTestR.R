#Web Scrapping the Central Banks
#Part -1: Get start time
start_time <- Sys.time()

#Part 0: The Libraries
#For Data Analysis
library(dplyr)
#For Sentiment Analysis
library(tidytext)
library(sentimentr)
#For Data Scraping
#install.packages("xml2")
library(xml2)
#install.packages("rvest")
library(rvest)

#Part 1: Web Scrapping 

#Part 1.1: The Fed Watcher News
#This reads in the website and looks for the 'article' html tag and extracts the text from the articles
fedurl <- read_html('https://blogs.uoregon.edu/timduyfedwatch/') %>%
  html_nodes("article") %>%
  html_text()

fedurl

#Part 1.2 Reserve Bank of Australia News
#RBA, ECB and BOJ can not be done the same way as the the Fed Watcher because the main page is a list of 
#links, so we first have to extract the links from the main page and then read in the html from each of the
#articles collected
ausurl <- ("https://www.rba.gov.au/news/") 
ausdoc <- read_html(ausurl)
#Looking for the specific links the 'a' html tag is were the links are located (the node) and
#the has the link (the attribute of the node)
auslinks <- html_attr(html_nodes(ausdoc, "a"), "href")
head(auslinks)
auslinks
#This is to locate where the relevant news links begin generated from the list of links above
newauslinks <- auslinks[c(22:31)]
length(newauslinks)

#This creates an empty character vector to hold the news content
ausnewcontent = vector(mode = "character")

#This loop is to get the article content for analysis, this loop first accesses each link by given by 
#the list, in the AUS case, the href only had the extension, so the website opening was required creating
#the url variable.
#Next, inside the tryCatch, the url is again parsed by the read_html function and all of the 'p' html tags
#are taken and their text stripped out.
#Next, each of the 'p' text are combined into one character vector, and then added to the greater content
#vector. Finally a three second sleep is added to the end of the loop
for(i in 1:length(newauslinks)){
  
  #Getting the links
  link = newauslinks[i]
  url = paste("https://www.rba.gov.au",link, sep ="")
  tryCatch({
  ausarticle <- read_html(url) %>%
    html_nodes("p") %>%
    html_text()
  }, error = function(e){})
  auscontent = paste(ausarticle, collapse = " ")
  ausnewcontent[i] = auscontent
  Sys.sleep(3)
  
}

head(ausnewcontent)

#Part 1.3 European Central Bank News
#Same as AUS
ecburl <- ("https://www.cnbc.com/european-central-bank/") 
ecbdoc <- read_html(ecburl)
ecblinks <- html_attr(html_nodes(ecbdoc, "a"), "href")
head(ecblinks)
ecblinks
#The line below will have to be changed everyday, so not ideal will have to be, but check the ecblinks
#See if there is a way to do every third entry: (see link below)
#https://stackoverflow.com/questions/5237557/extract-every-nth-element-of-a-vector
#newecblinks <- ecblinks[c(147, 150, 153, 156, 159, 162, 165, 168, 172, 174)]
newecblinkstest <- ecblinks[seq(147, length(ecblinks), 3)]
newecblinks10 <- newecblinkstest[c(1:10)]
length(newecblinks10)

#Empty character vector
ecbnewcontent = vector(mode = "character")

#This loop is to get the article content for analysis, same as the AUS loop except the href link has 
#the full link so the additional url step is not needed
for(i in 1:length(newecblinks10)){
  
  #Getting the links
  link = newecblinks10[i]
  tryCatch({
    ecbarticle <- read_html(link) %>%
      html_nodes("p") %>%
      html_text()
  }, error = function(e){})
  ecbcontent = paste(ecbarticle, collapse = " ")
  ecbnewcontent[i] = ecbcontent
  Sys.sleep(3)
  
}

head(ecbnewcontent)


#Part 1.4: Bank of Japan News
bojurl <- ("https://www.japantimes.co.jp/tag/boj/") 
bojdoc <- read_html(bojurl)
bojlinks <- html_attr(html_nodes(bojdoc, "a"), "href")
head(bojlinks)
bojlinks
#The line below will have to be changed everyday, so not ideal will have to be
#Since this is not a set difference between each news link, this will have to updated manually
newbojlinks <- bojlinks[c(90, 92, 97, 105, 112, 119, 126, 132, 139, 144)]
length(newbojlinks)

#Empty Character Vector
bojnewcontent = vector(mode = "character")

#This loop is to get the article content for analysis, same as the ECB loop
for(i in 1:length(newbojlinks)){
  
  #Getting the links
  link = newbojlinks[i]
  tryCatch({
    bojarticle <- read_html(link) %>%
      html_nodes("p") %>%
      html_text()
  }, error = function(e){})
  bojcontent = paste(bojarticle, collapse = " ")
  bojnewcontent[i] = bojcontent
  Sys.sleep(3)
  
}

head(bojnewcontent)


#Part 1.5: Bank of England News, this is different as they use xml instead of html so changes were needed
#https://stackoverflow.com/questions/33446888/r-convert-xml-data-to-data-frame
#This reads in the link, parses the xml with the read_xml function. Next, using the xml_find_all, all the 
#link tags were found and cleaned with the trimws function
boeurl <- ("https://www.bankofengland.co.uk/rss/news")
boedoc <- read_xml(boeurl)
boelinks <- xml_find_all(boedoc, "//link")
head(boelinks)
boelinksclean <- trimws(xml_text(boelinks))
boelinksclean
#Skip the first line of the link bc it will be recusive, and take the top links
newboelinks <- boelinksclean[c(2:20)]
length(newboelinks)

#Empty Character vector
boenewcontent = vector(mode = "character")

#This loop is to get the article content for analysis, this is the same as the ECB loop
for(i in 1:length(newboelinks)){
  
  #Getting the links
  link = newboelinks[i]
  tryCatch({
    boearticle <- read_html(link) %>%
      html_nodes("p") %>%
      html_text()
  }, error = function(e){})
  boecontent = paste(boearticle, collapse = " ")
  boenewcontent[i] = boecontent
  Sys.sleep(3)
  
}

head(boenewcontent)

#The UK did work for both Python and R, its just that the cookies thing is at the top of everyone of it 

#PART 2: SENTIMENT ANALYSIS

#PART 2.1 FED WATCH NEWS

#Creating the dataframe with the tibble function which is needed to tokenize the content scrapped 
#from the Fed Watch Website
lengthdf <- length(fedurl)
lengthdf
text <- fedurl
text_df <- tibble(line = 1:lengthdf, text = text)
text_df

#Creating the one word per text data frame
txdf1 <- text_df %>% 
  unnest_tokens(word, text)

#How to deal with weird words that appear in the text if needed
custom_stop_words <- bind_rows(stop_words)

#Removing stop words such as the, of, to
txdf2 <- txdf1 %>%
  dplyr::anti_join(custom_stop_words)

rm(txdf1)

#count the number of words frequency, specifically looking for "inflation" as it is an indication of 
#economic and currency stability
fedwordcount <- txdf2 %>% dplyr::count(word, sort = TRUE)
inflationcountfed <- fedwordcount %>% filter(word == "inflation")
inflationcountfed

#Getting the Length of of the number of words used
lengthdf2 <- length(txdf2$line)

#Use the lexicon for finanical language from SentimentAnalysis
library(SentimentAnalysis)

#Positive Words LM Lexicon
LM <- SentimentAnalysis::DictionaryLM
LMpos <- LM[["positive"]]
test <- tidytext::get_sentiments("afinn")
lenLMpos <- length(LMpos)
LMpostable <- tibble(Value = "positive", word = LMpos)

#How many positive words have been used LM Lexicon
Fedpos <- txdf2 %>%
  inner_join(LMpostable) %>%
  dplyr::count(word, sort = TRUE)

#Total number of positive words used
totalposlm <- sum(Fedpos$n)
totalposlm

#Faction of positive words LM Lexicon compared to total words used
PosFactionlm <- totalposlm / lengthdf2
PosFactionlm


#Negative Words LM Lexicon
LM <- SentimentAnalysis::DictionaryLM
LMneg <- LM[["negative"]]
lenLMneg <- length(LMneg)
LMnegtable <- tibble(Value = "negative", word = LMneg)

#How many negative words have been used
Fedneg <- txdf2 %>%
  inner_join(LMnegtable) %>%
  dplyr::count(word, sort = TRUE)

totalneglm <- sum(Fedneg$n)
totalneglm

#Faction of negative words
NegFactionlm <- totalneglm / lengthdf2
NegFactionlm

#So More Negative Financial News (xxx) than Postive News (xxx)
DifferenceLM <- PosFactionlm - NegFactionlm
DifferenceLM

#DifferenceLM = xxxxx

#Bing Positive and Negative, this are generally words labeled as positive or negative
bing <- tidytext::get_sentiments("bing")
bingneg <- bing %>% filter(sentiment == "negative")
bingpos <- bing %>% filter(sentiment == "positive")

#Negative Words from the Bing Lexicon 
bingFedneg <- txdf2 %>%
  inner_join(bingneg) %>%
  dplyr::count(word, sort = TRUE)

totalnegbingfed <- sum(bingFedneg$n)
totalnegbingfed

##Faction of negative words
NegFactionbingFed <- totalnegbingfed / lengthdf2
NegFactionbingFed

#Positive Words
bingFedpos <- txdf2 %>%
  inner_join(bingpos) %>%
  dplyr::count(word, sort = TRUE)

totalposbingfed <- sum(bingFedpos$n)
totalposbingfed

##Faction of Positive words
PosFactionbingFed <- totalposbingfed / lengthdf2
PosFactionbingFed

#So More Negative Financial News (xxx) than Postive News (xxxx)
DiffBingFed <- PosFactionbingFed - NegFactionbingFed
DiffBingFed


#Average Difference xxxxx in Positive to Negative
#Averaging the Difference between the General Words and the Financial Words used
#If just want to focus on the Financial words than the Bing Step can be removed
AvgDiff = ((DiffBingFed + DifferenceLM) / 2)
AvgDiff

#This is get the amount inflation is used in the text, if it is used heavily, it would be a good indicator
#that a currency will depreciate soon
#How many times Inflation was used: 58
inflationuseFed = inflationcountfed$n/ lengthdf2
inflationuseFed

#So Avg Diff is negative more likely entering a weaker economy, thus can expect currency to depreciate
#Avg Diff = -.31%
#https://www.investopedia.com/terms/c/currency-depreciation.asp#:~:text=Countries%20with%20weak%20economic%20fundamentals,its%20trade%20deficit%20over%20time.
#However inflation made up 1.1% of the words used which is not a good sign

#Going to try using the AFINN as well, it is another lexicon that uses a range instead of just a positive 
#or negative lable. +5 is most positive, -5 is most negative
#AFINN Positive and Negative
afinn <- tidytext::get_sentiments("afinn")
afinnneg <- afinn %>% filter(value < 0)
afinnpos <- afinn %>% filter(value > 0)

#Negative Words
afinnFedneg <- txdf2 %>%
  inner_join(afinnneg)

totalnegafinnfed <- sum(afinnFedneg$value)
totalnegafinnfed

##Faction of negative words
NegFactionafinnFed <- totalnegafinnfed / lengthdf2
NegFactionafinnFed

#Positive Words
afinnFedpos <- txdf2 %>%
  inner_join(afinnpos)

totalposafinnfed <- sum(afinnFedpos$value)
totalposafinnfed

##Faction of Positive words
PosFactionafinnFed <- totalposafinnfed / lengthdf2
PosFactionafinnFed

#So More Positive Words used than Negative ones (5.089%)
DiffafinnFed <- PosFactionafinnFed + NegFactionafinnFed
DiffafinnFed

#So in general the words used to describe the Fed are more positive 


#PART 2.2 RESERVE BANK OF AUSTRALIA

#IMPORT AUS DATA

#Creating the  dataframe
lengthdfaus <- length(ausnewcontent)
lengthdfaus
textaus <- ausnewcontent
text_dfaus <- tibble(line = 1:lengthdfaus, text = textaus)
text_dfaus

#Creating the one word per text data frame
txdf1aus <- text_dfaus %>% 
  unnest_tokens(word, text)

#How to deal with weird words that appear in the text, this includes the opening cookie message
custom_stop_words <- bind_rows(tibble(word = c("javascript", "disabled", "website", "viewed", "enabled",
                                               "interactive", "content", "requires", "webpage",
                                               "subject", "copyright", "terms", "conditions",
                                               "set", "disclaimer", "notice"), 
                                       lexicon = c("custom")),
                                stop_words)

#Removing stop words such as the, of, to
txdf2aus <- txdf1aus %>%
  dplyr::anti_join(custom_stop_words)

rm(txdf1aus)

#count the number of words frequency
auswordcount <- txdf2aus %>% dplyr::count(word, sort = TRUE)
inflationcountaus <- auswordcount %>% filter(word == "inflation")
inflationcountaus

lengthdf2aus <- length(txdf2aus$line)

#How many positive words have been used in aus from LM
Ausposlm <- txdf2aus %>%
  inner_join(LMpostable) %>%
  dplyr::count(word, sort = TRUE)

austotalposlm <- sum(Ausposlm$n)
austotalposlm

#Faction of positive words
AusPosFactionlm <- austotalposlm / lengthdf2aus
AusPosFactionlm

#How many negative words have been used
Ausneglm <- txdf2aus %>%
  inner_join(LMnegtable) %>%
  dplyr::count(word, sort = TRUE)

austotalneglm <- sum(Ausneglm$n)
austotalneglm

#Faction of positive words
AusNegFactionlm <- austotalneglm / lengthdf2aus
AusNegFactionlm

#So More Negative Financial News (xxxxx) than Postive News (xxxx)
AusDifferencelm <- AusPosFactionlm - AusNegFactionlm
AusDifferencelm

#Bing Positive and Negative
bing <- tidytext::get_sentiments("bing")
bingneg <- bing %>% filter(sentiment == "negative")
bingpos <- bing %>% filter(sentiment == "positive")

#Negative Words
bingAusneg <- txdf2aus %>%
  inner_join(bingneg) %>%
  dplyr::count(word, sort = TRUE)

totalnegbingAus <- sum(bingAusneg$n)
totalnegbingAus

##Faction of negative words
NegFactionbingAus <- totalnegbingAus / lengthdf2aus
NegFactionbingAus

#Positive Words
bingAuspos <- txdf2aus %>%
  inner_join(bingpos) %>%
  dplyr::count(word, sort = TRUE)

totalposbingAus <- sum(bingAuspos$n)
totalposbingAus

##Faction of Positive words
PosFactionbingAus <- totalposbingAus / lengthdf2aus
PosFactionbingAus

#So More Positive News than Negative News
DiffBingAus <- PosFactionbingAus - NegFactionbingAus
DiffBingAus

#Average Difference
AvgDiffaus = ((DiffBingAus + AusDifferencelm) / 2)
AvgDiffaus

#How many times Inflation was used
inflationuseAus = inflationcountaus$n / lengthdf2aus
inflationuseAus

#Going to try using the AFINN as well 
#Negative Words
afinnAusneg <- txdf2aus %>%
  inner_join(afinnneg)

totalnegafinnAus <- sum(afinnAusneg$value)
totalnegafinnAus

##Faction of negative words
NegFactionafinnAus <- totalnegafinnAus / lengthdf2aus
NegFactionafinnAus

#Positive Words
afinnAuspos <- txdf2aus %>%
  inner_join(afinnpos)

totalposafinnAus <- sum(afinnAuspos$value)
totalposafinnAus

##Faction of Positive words
PosFactionafinnAus <- totalposafinnAus / lengthdf2aus
PosFactionafinnAus

#So More Positive than Negative News
DiffafinnAus <- PosFactionafinnAus + NegFactionafinnAus
DiffafinnAus



#PART 2.3 EUROPEAN CENTRAL BANK 

#IMPORT ECB DATA
#Creating the dataframe
lengthdfecb <- length(ecbnewcontent)
lengthdfecb
textecb <- ecbnewcontent
text_dfecb <- tibble(line = 1:lengthdfecb, text = textecb)
text_dfecb

#Creating the one word per text data frame
txdf1ecb <- text_dfecb %>% 
  unnest_tokens(word, text)

#How to deal with weird words that appear in the text
custom_stop_words <- bind_rows(stop_words)

#Removing stop words such as the, of, to
txdf2ecb <- txdf1ecb %>%
  dplyr::anti_join(custom_stop_words)

rm(txdf1ecb)

#count the number of words frequency
ecbwordcount <- txdf2ecb %>% dplyr::count(word, sort = TRUE)
inflationcountecb <- ecbwordcount %>% filter(word == "inflation")
inflationcountecb

lengthdf2ecb <- length(txdf2ecb$line)

#How many positive words have been used in ecb from LM
ecbposlm <- txdf2ecb %>%
  inner_join(LMpostable) %>%
  dplyr::count(word, sort = TRUE)

ecbtotalposlm <- sum(ecbposlm$n)
ecbtotalposlm

#Faction of positive words
ecbPosFactionlm <- ecbtotalposlm / lengthdf2ecb
ecbPosFactionlm

#How many negative words have been used
ecbneglm <- txdf2ecb %>%
  inner_join(LMnegtable) %>%
  dplyr::count(word, sort = TRUE)

ecbtotalneglm <- sum(ecbneglm$n)
ecbtotalneglm

#Faction of positive words
ecbNegFactionlm <- ecbtotalneglm / lengthdf2ecb
ecbNegFactionlm

#So More Negative Financial News (4.5%) than Postive News (1.2%)
ecbDifferencelm <- ecbPosFactionlm - ecbNegFactionlm
ecbDifferencelm

#Bing Negative Words
bingecbneg <- txdf2ecb %>%
  inner_join(bingneg) %>%
  dplyr::count(word, sort = TRUE)

totalnegbingecb <- sum(bingecbneg$n)
totalnegbingecb

##Faction of negative words
NegFactionbingecb <- totalnegbingecb / lengthdf2
NegFactionbingecb

#Positive Words
bingecbpos <- txdf2ecb %>%
  inner_join(bingpos) %>%
  dplyr::count(word, sort = TRUE)

totalposbingecb <- sum(bingecbpos$n)
totalposbingecb

##Faction of Positive words
PosFactionbingecb <- totalposbingecb / lengthdf2ecb
PosFactionbingecb

#So More Positive General Words (xxxxx) than Negative General Words (xxxxx)
DiffBingecb <- PosFactionbingecb - NegFactionbingecb
DiffBingecb

#Average Difference
AvgDiffecb = ((DiffBingecb + ecbDifferencelm) / 2)
AvgDiffecb

#How many times Inflation was used
inflationuseecb = inflationcountecb$n / lengthdf2ecb
inflationuseecb

#So Avg Diff is negative more likely entering a weaker economy, thus can expect currency to depreciate
# Avg Diff = -1.13%
#However inflation made up 0.2% of the words used which is a better sign that the currency will remain stable

#Going to try using the AFINN as well 
#Negative Words
afinnecbneg <- txdf2ecb %>%
  inner_join(afinnneg)

totalnegafinnecb <- sum(afinnecbneg$value)
totalnegafinnecb

##Faction of negative words
NegFactionafinnecb <- totalnegafinnecb / lengthdf2ecb
NegFactionafinnecb

#Positive Words
afinnecbpos <- txdf2ecb %>%
  inner_join(afinnpos)

totalposafinnecb <- sum(afinnecbpos$value)
totalposafinnecb

##Faction of Positive words
PosFactionafinnecb <- totalposafinnecb / lengthdf2ecb
PosFactionafinnecb

#So slightly more Negative than Positive News
Diffafinnecb <- PosFactionafinnecb + NegFactionafinnecb
Diffafinnecb
#Afinn Diff: -.xxx%

#So even amount of very negative and very positive words but leans towards negative 

#PART 2.4: BANK OF JAPAN NEWS

#IMPORT BOJ DATA
#Creating the textvector dataframe
lengthdfboj <- length(bojnewcontent)
lengthdfboj
textboj <- bojnewcontent
text_dfboj <- tibble(line = 1:lengthdfboj, text = textboj)
text_dfboj

#Creating the one word per text data frame
txdf1boj <- text_dfboj %>% 
  unnest_tokens(word, text)

#How to deal with weird words that appear in the text
custom_stop_words <- bind_rows(stop_words)

#Removing stop words such as the, of, to
txdf2boj <- txdf1boj %>%
  dplyr::anti_join(custom_stop_words)

rm(txdf1boj)

#count the number of words frequency
bojwordcount <- txdf2boj %>% dplyr::count(word, sort = TRUE)
inflationcountboj <- bojwordcount %>% filter(word == "inflation")
inflationcountboj

lengthdf2boj <- length(txdf2boj$line)

#How many positive words have been used in boj from LM
bojposlm <- txdf2boj %>%
  inner_join(LMpostable) %>%
  dplyr::count(word, sort = TRUE)

bojtotalposlm <- sum(bojposlm$n)
bojtotalposlm

#Faction of positive words
bojPosFactionlm <- bojtotalposlm / lengthdf2boj
bojPosFactionlm

#How many negative words have been used
bojneglm <- txdf2boj %>%
  inner_join(LMnegtable) %>%
  dplyr::count(word, sort = TRUE)

bojtotalneglm <- sum(bojneglm$n)
bojtotalneglm

#Faction of positive words
bojNegFactionlm <- bojtotalneglm / lengthdf2boj
bojNegFactionlm

#So More Negative Financial News (xxx) than Postive News (xxxx)
bojDifferencelm <- bojPosFactionlm - bojNegFactionlm
bojDifferencelm

#Bing Negative Words
bingbojneg <- txdf2boj %>%
  inner_join(bingneg) %>%
  dplyr::count(word, sort = TRUE)

totalnegbingboj <- sum(bingbojneg$n)
totalnegbingboj

##Faction of negative words
NegFactionbingboj <- totalnegbingboj / lengthdf2boj
NegFactionbingboj

#Positive Words
bingbojpos <- txdf2boj %>%
  inner_join(bingpos) %>%
  dplyr::count(word, sort = TRUE)

totalposbingboj <- sum(bingbojpos$n)
totalposbingboj

##Faction of Positive words
PosFactionbingboj <- totalposbingboj / lengthdf2boj
PosFactionbingboj

#So More Positive General Words (xxx) than Negative General Words (xxxx)
DiffBingboj <- PosFactionbingboj - NegFactionbingboj
DiffBingboj

#Average Difference
AvgDiffboj = ((DiffBingboj + bojDifferencelm) / 2)
AvgDiffboj

#How many times Inflation was used
inflationuseboj = inflationcountboj$n / lengthdf2boj
inflationuseboj

#So Avg Diff is negative more likely entering a weaker economy, thus can expect currency to depreciate
# Avg Diff = xxxx
#However inflation made up xxxx% of the words used which is a better sign that the currency will remain 
#stable

#Going to try using the AFINN as well 
#Negative Words
afinnbojneg <- txdf2boj %>%
  inner_join(afinnneg)

totalnegafinnboj <- sum(afinnbojneg$value)
totalnegafinnboj

##Faction of negative words
NegFactionafinnboj <- totalnegafinnboj / lengthdf2boj
NegFactionafinnboj

#Positive Words
afinnbojpos <- txdf2boj %>%
  inner_join(afinnpos)

totalposafinnboj <- sum(afinnbojpos$value)
totalposafinnboj

##Faction of Positive words
PosFactionafinnboj <- totalposafinnboj / lengthdf2boj
PosFactionafinnboj

#So more very Negative than Positive Wprds
Diffafinnboj <- PosFactionafinnboj + NegFactionafinnboj
Diffafinnboj


#PART 2.5: BANK OF ENGLAND NEWS

#Creating the dataframe
lengthdfboe <- length(boenewcontent)
lengthdfboe
textboe <- boenewcontent
text_dfboe <- tibble(line = 1:lengthdfboe, text = textboe)
text_dfboe

#Creating the one word per text data frame
txdf1boe <- text_dfboe %>% 
  unnest_tokens(word, text)

#How to deal with weird words that appear in the text, or removing the opening cookies text
custom_stop_words <- bind_rows(tibble(word = c("neccessary", "cookies", "site", "settings", "website",
                                                                    "example", "manage", "browser",
                                               "session", "accept", "cookie", "clicking", "disable",
                                               "enable", "track", "we'd", "visitors", "optional",
                                               "functionality", "analytics", "banner", 'party',
                                               "security", "management", "accessibility", "spacebar"), 
                                                           lexicon = c("custom")),
                                                    stop_words)

#Removing stop words such as the, of, to
txdf2boe <- txdf1boe %>%
  dplyr::anti_join(custom_stop_words)

rm(txdf1boe)

#count the number of words frequency
boewordcount <- txdf2boe %>% dplyr::count(word, sort = TRUE)
boewordcount
inflationcountboe <- boewordcount %>% filter(word == "inflation")
inflationcountboe

lengthdf2boe <- length(txdf2boe$line)
lengthdf2boe

#How many positive words have been used in boe from LM
boeposlm <- txdf2boe %>%
  inner_join(LMpostable) %>%
  dplyr::count(word, sort = TRUE)

boetotalposlm <- sum(boeposlm$n)
boetotalposlm

#Faction of positive words
boePosFactionlm <- boetotalposlm / lengthdf2boe
boePosFactionlm

#How many negative words have been used
boeneglm <- txdf2boe %>%
  inner_join(LMnegtable) %>%
  dplyr::count(word, sort = TRUE)

boetotalneglm <- sum(boeneglm$n)
boetotalneglm

#Faction of positive words
boeNegFactionlm <- boetotalneglm / lengthdf2boe
boeNegFactionlm

#So More Negative Financial News (2.69%) than Postive News (1.75%)
boeDifferencelm <- boePosFactionlm - boeNegFactionlm
boeDifferencelm

#Bing Negative Words
bingboeneg <- txdf2boe %>%
  inner_join(bingneg) %>%
  dplyr::count(word, sort = TRUE)

totalnegbingboe <- sum(bingboeneg$n)
totalnegbingboe

##Faction of negative words
NegFactionbingboe <- totalnegbingboe / lengthdf2boe
NegFactionbingboe

#Positive Words
bingboepos <- txdf2boe %>%
  inner_join(bingpos) %>%
  dplyr::count(word, sort = TRUE)

totalposbingboe <- sum(bingboepos$n)
totalposbingboe

##Faction of Positive words
PosFactionbingboe <- totalposbingboe / lengthdf2boe
PosFactionbingboe

#So More Positive General Words (5.44%) than Negative General Words (2.95%)
DiffBingboe <- PosFactionbingboe - NegFactionbingboe
DiffBingboe

#Average Difference
AvgDiffboe = ((DiffBingboe + boeDifferencelm) / 2)
AvgDiffboe

#How many times Inflation was used
inflationuseboe = inflationcountboe$n / lengthdf2boe
inflationuseboe

#So Avg Diff is positive more likely entering a strong economy, thus can expect currency to appreciate
# Avg Diff = xxxx
#However inflation made up xxxx of the words used which is a better sign that the currency will remain stable

#Going to try using the AFINN as well 
#Negative Words
afinnboeneg <- txdf2boe %>%
  inner_join(afinnneg)

totalnegafinnboe <- sum(afinnboeneg$value)
totalnegafinnboe

##Faction of negative words
NegFactionafinnboe <- totalnegafinnboe / lengthdf2boe
NegFactionafinnboe

#Positive Words
afinnboepos <- txdf2boe %>%
  inner_join(afinnpos)

totalposafinnboe <- sum(afinnboepos$value)
totalposafinnboe

##Faction of Positive words
PosFactionafinnboe <- totalposafinnboe / lengthdf2boe
PosFactionafinnboe

#So more very Negative than Positive Wprds
Diffafinnboe <- PosFactionafinnboe + NegFactionafinnboe
Diffafinnboe

#PART 3: CURRENCY PAIRS 
#We have USD, AUD, EUR, JPY, GBP

#WHICH CURRENCY TO BUY
#If the value appreciates (or goes up), demand for the currency also rises. 
#In contrast, if a currency depreciates, it loses value against the currency against 
#which it is being traded.
#https://www.investopedia.com/terms/c/currency-appreciation.asp#:~:text=If%20the%20value%20appreciates%20(or,which%20it%20is%20being%20traded.

#So if the currency appreciate, the demand for the currency rises so you want to buy the stronger currency

#Additionally we can check inflation, if inflation is discussed more in one country than other, then that
#means the currency has a chance of instability and depreciating

#PART 3.1 AUD/USD which currency is stronger

#Check if AUD or USD is stronger
if (AvgDiffaus > AvgDiff) {
  print("AUS has more positive news than USD, AUS will most likely Appreciate compared to USD, buy AUS")
} else if ( AvgDiffaus < AvgDiff) {
  print("USD has more positive new than AUS, USD will most likely Appreciate compared to USD, buy USD")
}

if(inflationuseAus > inflationuseFed) {
  print("USD has a lower chance of inflation, greater stability and less chance of depreciation 
        compared to AUD")
} else if (inflationuseAus < inflationuseFed) {
  print("AUS has a lower chance of inflation, greater stability and less chance of depreciation 
        compared to USD")
}

#PART 3.2 EUR/USD which currency is stronger

#Check if EUR or USD is stronger
if (AvgDiffecb > AvgDiff) {
  print("Euro has more positive news than USD, Euro will most likely Appreciate compared to USD, buy Euro")
} else if ( AvgDiffecb < AvgDiff) {
  print("USD has more positive new than Euro, USD will most likely Appreciate compared to Euro, buy USD")
}

if(inflationuseecb > inflationuseFed) {
  print("USD has a lower chance of inflation, greater stability and less chance of depreciation 
        compared to Euro")
} else if (inflationuseecb < inflationuseFed) {
  print("Euro has a lower chance of inflation, greater stability and less chance of depreciation 
        compared to USD")
}


#PART 3.3 USD/JPY which currency is stronger

#Check if JPY or USD is stronger
if (AvgDiffboj > AvgDiff) {
  print("JPY has more positive news than USD, JPY will most likely Appreciate compared to USD, buy JPY")
} else if ( AvgDiffboj < AvgDiff) {
  print("USD has more positive news than JPY, USD will most likely Appreciate compared to JPY, buy USD")
}

if(inflationuseboj > inflationuseFed) {
  print("USD has a lower chance of inflation, greater stability and less chance of depreciation 
        compared to JPY")
} else if (inflationuseboj < inflationuseFed) {
  print("JPY has a lower chance of inflation, greater stability and less chance of depreciation 
        compared to USD")
}

#PART 3.4 GBP/USD which currency is stronger

#Check if GBP or USD is stronger
if (AvgDiffboe > AvgDiff) {
  print("GBP has more positive news than USD, GBP will most likely Appreciate compared to USD, buy GBP")
} else if ( AvgDiffboe < AvgDiff) {
  print("USD has more positive news than GBP, USD will most likely Appreciate compared to JPY, buy USD")
}

if(inflationuseboe > inflationuseFed) {
  print("USD has a lower chance of inflation, greater stability and less chance of depreciation 
        compared to GBP")
} else if (inflationuseboe < inflationuseFed) {
  print("GBP has a lower chance of inflation, greater stability and less chance of depreciation 
        compared to USD")
}

#Part 4: Store the values 

datevalue <- Sys.Date()
#dfvalue = data.frame("AvgDiff" = AvgDiff, "AvgDiffaus" = AvgDiffaus, 
#                    "AvgDiffecb" = AvgDiffecb, "AvgDiffboj" = AvgDiffboj, "AvgDiffboe" = AvgDiffboe,
#                    "Date" = datevalue)

#write.csv(dfvalue, "/Users/johnc.burns/Documents/Documents/PhD Year One/Mockup 1/Rvaluestest.csv", row.names = FALSE)

newvalue = data.frame("AvgDiff" = AvgDiff, "AvgDiffaus" = AvgDiffaus, 
                      "AvgDiffecb" = AvgDiffecb, "AvgDiffboj" = AvgDiffboj, "AvgDiffboe" = AvgDiffboe, 
                      "Date" = datevalue)

write.table(newvalue, file = "/Users/johnc.burns/Documents/Documents/PhD Year One/Mockup 1/Rvaluestest.csv", 
            append = TRUE, col.names = FALSE, row.names = FALSE, sep = ",", eol = "\r")

#Part 5: End Time
end_time <- Sys.time()

timediff <- end_time - start_time
timediff
#4.5 minutes to run 

