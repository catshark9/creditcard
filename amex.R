writeLines(sprintf("var url ='https://www.americanexpress.com/us/credit-cards/view-all-personal-cards/';
var page = new WebPage()
                   var fs = require('fs');
                   
                   
                   page.open(url, function (status) {
                   just_wait();
                   });
                   
                   function just_wait() {
                   setTimeout(function() {
                   fs.write('1.html', page.content, 'w');
                   phantom.exit();
                   }, 20000);
                   }"), con="scrape_amex.js")

system("phantomjs scrape_amex.js")

pg <- read_html("1.html") 


amexCardName <- pg %>% 
  html_nodes(xpath='//*[@itemprop="name"]/a') %>%
  html_text()
amexCardName <- gsub('[^A-z&-+\']', ' ', amexCardName) 
amexCardName <- str_trim(gsub('\\s{2,}', ' ', amexCardName))
amexCardName <- amexCardName[2:length(amexCardName)]


amexCardOffer <- pg %>%
  html_nodes("[itemprop=offers]") %>%
  html_text()
amexCardOffer <- gsub('\n', '', amexCardOffer)
amexCardOffer <- str_trim(amexCardOffer)
amexCardOffer <- amexCardOffer[seq(from=1, to=39, by=2)]


amexLinks <- pg %>%
  html_nodes(xpath="//*[@itemprop='url']") %>%
  html_attr("content")


amex <- data.frame(CardName = amexCardName,
                   Issuer = 'Amex',
                   Program = 'American Express Membership Rewards',
                   Link = amexLinks,
                   IntroOffer = amexCardOffer, stringsAsFactors = FALSE)

amex$Cash   <- as.numeric(ifelse(grepl('\\$\\d{3} back after', tolower(amex$IntroOffer)), 
                      gsub('.*?\\$(\\d{3}) back after.*', '\\1', tolower(amex$IntroOffer)), 0))
amex$Cash   <- as.numeric(ifelse(grepl('\\$\\d{2,3} [S|s]tatement', amex$IntroOffer), 
                      gsub('.*?\\$(\\d{2,3}).*', '\\1', amex$IntroOffer), amex$Cash))
amex$Points   <- ifelse(grepl(' \\d{1,3},\\d{3}', amex$IntroOffer), 
                        gsub('.*? (\\d{1,3},\\d{3}).*', '\\1', amex$IntroOffer), 0)
amex$Points   <- ifelse(grepl(' \\d{1,2},\\d{3} bonus', amex$IntroOffer), 
                        gsub('.*? (\\d{1,2},\\d{3}) bonus.*', '\\1', amex$IntroOffer), amex$Points)
amex$Points <- as.numeric(gsub(',','',amex$Points))
amex$Nights <- ifelse(grepl('nights', amex$IntroOffer), 
                      2, 0)

amexCredit <- c()
amexIntro <- c()
amexIMG <- c()
i <- 1
for(l in amex$Link){
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
                     }, 7000);
                     }", l), con="scrape_amex.js")
  
  system("phantomjs scrape_amex.js")
  
  url <- read_html("1.html")
  
  intro <- url %>%
    html_nodes(xpath='//*[@id="overview"]/section[2]/div/div[1]/div[1]/p') %>%
    html_text()
  amexIntro <- c(amexIntro, intro)
  
  credit <- url %>%
    html_nodes(xpath="//*[@class='card-detail__features-list']") %>%
    html_text()
  credit <- credit[grepl('Airline Fee Credit', credit)]
  credit <- ifelse(length(credit)==0, 0, credit)
  credit <- as.numeric(gsub('.*?\\$(\\d{3}).*', '\\1', credit))
  amexCredit <- c(amexCredit, credit)
  
  
  img <- url %>%
    html_nodes(xpath="//*[@itemprop='image']") %>%
    html_attr("content")
  amexIMG <- c(amexIMG, img)
  

  
  i <- i + 1
}


amex$Credit <- amexCredit

amexFee <- pg %>%
  html_nodes('[itemprop=description]') %>%
  html_text()
amexFee <- gsub('\\s{2,}', ' ', amexFee)
amexFee <- str_trim(amexFee)

amexFee <- gsub('.*?Annual Fee(.*)', '\\1', amexFee)

amex$FeeWaived1stYr <- ifelse(grepl('\\$0.*first year', amexFee), 1, 0)

amex$Fee <- as.numeric(ifelse(grepl('\\$\\d{2,3}', amexFee),
                   gsub('.*?\\$(\\d{2,3}).*', '\\1', amexFee), 0))
amex$Fee <- ifelse(grepl('No Annual Fee', amexFee), 0, amex$Fee)
amex$Fee <- ifelse(amex$Fee>450, 0, amex$Fee)

amexSpend <- gsub(',', '', amex$IntroOffer)
amexSpend <- ifelse(grepl('\\$(\\d{3,4}) [i|o]n purchases', amexSpend),
                    gsub('.*?\\$(\\d{3,4}) [i|o]n purchases.*', '\\1', amexSpend), 0)
amex$Spend <- as.numeric(gsub('\\D', '', amexSpend))

amex <- amex[amex$CardName!='Serve from American Express', ]
amex$img <- amexIMG



for(p in rates[,1][rates[,1]!='American']){
  amex$Program <- ifelse(grepl(p, amex$CardName), p, amex$Program)
}
for(p in rates[,1][rates[,1]!='American']){
  amex$Program <- ifelse(grepl(p, amex$IntroOffer), p, amex$Program)
}

write.csv(amex, 'amex.csv')

