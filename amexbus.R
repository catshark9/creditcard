writeLines(sprintf("var url ='https://www.americanexpress.com/us/small-business/credit-cards/see-all-business-credit-cards/44281?linknav=us-open-aj-hp-viewallcards-abc';
var page = new WebPage()
                   var fs = require('fs');
                   
                   
                   page.open(url, function (status) {
                   just_wait();
                   });
                   
                   function just_wait() {
                   setTimeout(function() {
                   fs.write('1.html', page.content, 'w');
                   phantom.exit();
                   }, 10000);
                   }"), con="scrape_amex.js")

system("phantomjs scrape_amex.js")

pg <- read_html("1.html")


# extract the content you need
abusCardName <- pg %>% 
  html_nodes('[class=viewcomparelink]') %>%
  html_text()
abusCardName <- gsub('[^A-z&-+\']', ' ', abusCardName)
abusCardName <- str_trim(gsub('\\s{2,}', ' ', abusCardName))


abusCardOffer <- pg %>%
  html_nodes(xpath="//*[@class='grid-offer-container earn-points']") %>%
  html_text()
abusCardOffer <- gsub('WELCOME OFFER', '', abusCardOffer)
abusCardOffer <- gsub('\\s{2,}', '', abusCardOffer)

abusLinks <- pg %>%
  html_nodes('[class=viewcomparelink]') %>%
  html_attr("href")


abus <- data.frame(CardName = abusCardName,
                   Issuer = 'Amex',
                   Program = 'American Express Membership Rewards',
                   Link = abusLinks,
                   IntroOffer = abusCardOffer, stringsAsFactors = FALSE)


abus$Cash   <- as.numeric(ifelse(grepl('\\$\\d{3} [B|b]ack', abus$IntroOffer), 
                      gsub('.*?\\$(\\d{3}).*', '\\1', abus$IntroOffer), 0))
abus$Cash   <- as.numeric(ifelse(grepl('\\$\\d{2,3} [S|s]tatement', abus$IntroOffer), 
                      gsub('.*?\\$(\\d{2,3}).*', '\\1', abus$IntroOffer), abus$Cash))
abus$Points   <- ifelse(grepl(' \\d{1,3},\\d{3}', abus$IntroOffer), 
                        gsub('.*? (\\d{1,3},\\d{3}).*', '\\1', abus$IntroOffer), 0)
abus$Points   <- ifelse(grepl(' \\d{1,3},\\d{3} bonus', abus$IntroOffer), 
                        gsub('.*? (\\d{1,3},\\d{3}) bonus.*', '\\1', abus$IntroOffer), abus$Points)
abus$Points <- as.numeric(gsub(',','',abus$Points))
abus$Nights <- ifelse(grepl('nights', abus$IntroOffer), 
                      2, 0)

abusCredit <- c()
abusFee <- c()
abusIMG <- c()
i <- 1
for(l in abusLinks){
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
                     }, 10000);
                     }", l), con="scrape_amex.js")
  
  system("phantomjs scrape_amex.js")
  
  url <- read_html("1.html")
  
  credit <- url %>%
    html_nodes(xpath='//*[@id="details"]/div[2]/div[2]/div/ul') %>%
    html_text()
  credit <- credit[grepl('Airline Fee Credit', credit)]
  credit <- ifelse(length(credit)==0, 0, credit)
  credit <- as.numeric(gsub('.*?\\$(\\d{3}) Airline Fee Credit.*', '\\1', credit))
  abusCredit <- c(abusCredit, credit)
  
  fee <- url %>%
    html_nodes(xpath='//*[@id="offer-overview"]/div/div[2]') %>%
    html_text()
  
  abusFee <- c(abusFee, fee)
  
  img <- url %>%
    html_nodes(xpath='//*[@id="offer-overview"]/div/div[3]/div/div/img') %>%
    html_attr("src")
  abusIMG <- c(abusIMG, img)
  
  
  i <- i + 1
}

abus$Credit <- abusCredit

abusFee <- str_trim(gsub('\\s{2,}', ' ', abusFee))

abus$FeeWaived1stYr <- ifelse(grepl('\\$0.*first year', abusFee), 1, 0)

abusFee <- gsub('\\$0 ', '', abusFee)
abusFee <- as.numeric(ifelse(grepl('\\$\\d{2,3}', abusFee),
              gsub('.*?\\$(\\d{2,3}).*', '\\1', abusFee), 0))
abus$Fee <- abusFee

abusSpend <- gsub(',', '', abus$IntroOffer)
abusSpend1 <- as.numeric(ifelse(grepl('\\$(\\d{3,5}) [i|o]n purchases', abusSpend),
                    gsub('.*?\\$(\\d{3,5}) [i|o]n purchases.*', '\\1', abusSpend), 0))
abusSpend2 <- as.numeric(ifelse(grepl('spend \\$\\d{3,5}', abusSpend),
                    gsub('.*?spend \\$(\\d{3,5}).*', '\\1', abusSpend), 0))
abus$Spend <- apply(data.frame(abusSpend1, abusSpend2), 1, max)

abus$img <- abusIMG

for(p in rates[,1][rates[,1]!='American']){
  abus$Program <- ifelse(grepl(p, abus$CardName), p, abus$Program)
}
for(p in rates[,1][rates[,1]!='American']){
  abus$Program <- ifelse(grepl(p, abus$IntroOffer), p, abus$Program)
}

write.csv(abus, 'abus.csv')
