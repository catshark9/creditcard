writeLines(sprintf("var url ='https://www.bankofamerica.com/credit-cards/#filter';
var page = new WebPage()
                   var fs = require('fs');
                   
                   
                   page.open(url, function (status) {
                   just_wait();
                   });
                   
                   function just_wait() {
                   setTimeout(function() {
                   fs.write('1.html', page.content, 'w');
                   phantom.exit();
                   }, 2500);
                   }"), con="scrape_boa.js")

system("phantomjs scrape_boa.js")

pg <- read_html("1.html")


boaCardName <- pg %>% 
  html_nodes('[class=cnx-regular]') %>%
  html_text()
boaCardName <- boaCardName[seq(from=2, to=(length(boaCardName)-14))]
boaCardName <- gsub('[^A-z&-+\']', ' ', boaCardName)
boaCardName <- str_trim(gsub('\\s{2,}', ' ', boaCardName))
boaCardName <- boaCardName[5:length(boaCardName)]


boaCardOffer <- pg %>%
  html_nodes(xpath="//*[@class='sub-heading cnx-regular']") %>%
  html_text()
boaCardOffer <- boaCardOffer[5:length(boaCardOffer)]

boaLinks <- pg %>%
  html_nodes(xpath="//*[@class='learn spa-fn-done']") %>%
  html_attr("href")
boaLinks <- unique(paste0('https://www.bankofamerica.com', boaLinks))
boaLinks <- boaLinks[!grepl('campaign', boaLinks)]

boa <- data.frame(CardName = boaCardName,
                  Issuer = 'BOA',
                  Program = 'BOA',
                  Link = boaLinks,
                  IntroOffer = boaCardOffer, stringsAsFactors = FALSE)

boa <- boa[!duplicated(boa$CardName), ]

boa$Cash   <- as.numeric(ifelse(grepl('\\$\\d{3}', boa$IntroOffer), 
                      gsub('.*?\\$(\\d{3}).*', '\\1', boa$IntroOffer), 0))
boa$Points   <- ifelse(grepl('\\d{1,3},\\d{3}', boa$IntroOffer), 
                        gsub('.*?(\\d{1,3},\\d{3}).*', '\\1', boa$IntroOffer), 0)
boa$Points <- as.numeric(gsub(',','', boa$Points))
boa$Nights <- ifelse(grepl('nights', boa$IntroOffer), 2, 0)

boa$Credit <- 0


boaFee <- c()
boaSpend <- c()
#boaStatement <- c()
boaIMG <- c()
i<-1
for (l in boa$Link){
  writeLines(paste0(i))
  writeLines(sprintf("var url ='%s';
  var page = new WebPage()
                     var fs = require('fs');
                     
                     
                     page.open(url, function (status) {
                     just_wait();
                     });
                     
                     function just_wait() {
                     setTimeout(function() {
                     fs.write('1.html', page.content, 'w');
                     phantom.exit();
                     }, 2500);
                     }", l), con="scrape_boa.js")

  system("phantomjs scrape_boa.js")
  
  pg <- read_html("1.html")
  
  fee <- pg %>% 
    html_nodes(xpath='//*[@id="interestRatesFeesCcModule"]/div[1]/div[1]/div/div[3]') %>%
    html_text()
  boaFee <- c(boaFee, fee)
  
  spend1 <- pg %>%
    html_nodes(xpath="//*[@class='small-12 columns']") %>%
    html_text()
  spend1 <- gsub(',', '', spend1)
  
  spend <- spend1[grepl('\\$(\\d{3,4}) [i|o]n purchases', spend1)]
  spend <- as.numeric(ifelse(length(spend)>0,
                  gsub('.*?\\$(\\d{3,4}) [i|o]n purchases.*', '\\1', spend), 0))
  
  spend2 <- spend1[grepl('\\$(\\d{3,4}) or more', spend1)]
  spend2 <- as.numeric(ifelse(length(spend2)>0,
                  gsub('.*?\\$(\\d{3,4}) or more.*', '\\1', spend2), 0))
  spend<- max(spend, spend2)
  boaSpend <- c(boaSpend, spend)
  
  img <- pg %>%
    html_nodes(xpath='//*[@id="productEngagementCcModule"]/div/div/div[1]/img') %>%
    html_attr("src")
  img <- paste0('https://www.bankofamerica.com', img)
  
  
  
  boaIMG <- c(boaIMG, img)
  
  i<-i+1
}

boaFee <- str_trim(gsub('\\s{2,}', ' ', boaFee))

boa$FeeWaived1stYr <- ifelse(grepl('\\$0.*first year', boaFee), 1, 0)

boaFee <- gsub(' \\$0', '', boaFee)
boaFee <- as.numeric(ifelse(grepl('\\$(\\d{2,3})', boaFee),
                 gsub('.*?\\$(\\d{2,3}).*', '\\1', boaFee), 0))
if(length(boaFee) < nrow(boa)){
  boaFee[length(boaFee)+1]<-0
}

boa$Fee <- boaFee


boa$Spend <- boaSpend

boa$img <- boaIMG


for(p in rates[,1]){
  boa$Program <- ifelse(grepl(p, boa$CardName), p, boa$Program)
}
for(p in rates[,1]){
  boa$Program <- ifelse(grepl(p, boa$IntroOffer), p, boa$Program)
}

write.csv(boa, 'boa.csv')



