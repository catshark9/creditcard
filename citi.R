writeLines(sprintf("var url ='https://www.citi.com/credit-cards/compare-credit-cards/citi.action?ID=view-all-credit-cards';
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
                   }"), con="scrape2.js")

system("phantomjs scrape2.js")

pg <- read_html("1.html")

citiCardName <- pg %>% 
  html_nodes('[class=cA-DD-cardTitle]') %>%
  html_text()
citiCardName <- gsub('[^A-z&-+\']', ' ', citiCardName) 
citiCardName <- gsub('TM', ' ', citiCardName) 
citiCardName <- str_trim(gsub('\\s{2,}', ' ', citiCardName))

citiLinks <- pg %>%
  html_nodes(xpath="//*[@class='cA-DD-detailLink']") %>%
  html_attr("href")
citiLinks <- paste0('https://www.citi.com', citiLinks)


citi <- data.frame(CardName = citiCardName,
                   Issuer = 'Citi',
                   Program = 'Citi ThankYou Rewards',
                   Link = citiLinks,
                   #IntoOffer = citiCardOffer, 
                   stringsAsFactors = FALSE)


citiIntro <- c()
citiCredit <- c()
citiIMG <- c()

i <- 1
for(l in citiLinks){
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
                     }, 5000);
                     }", l), con="scrape_citi.js")
  
  system("phantomjs scrape_citi.js")
  
  url <- read_html("1.html")
  
  intro <- url %>%
    html_nodes(xpath="//*[@id='heroText']") %>%
    html_text()
  intro <- gsub('\t', '', intro)
  intro <- strsplit(intro, '\n')[[1]]
  intro <- intro[3:length(intro)]
  intro <- gsub('\\s{2,}', ' ', intro)
  intro <- str_trim(paste(intro, collapse = ''))
  

  citiIntro <- c(citiIntro, intro)
  
  credit <- url %>%
    html_nodes(xpath="//*[@class='cA-DD-cards-details-section-left cA-DD-cards-mobile-internal']") %>%
    html_text()
  credit <- credit[grepl('[T|t]ravel [C|c]redit', credit)]
  credit <- ifelse(length(credit)==0, 0, credit)
  credit <- as.numeric(gsub('.*?\\$([0-9]{2,3}).*', '\\1', credit))
  citiCredit <- c(citiCredit, credit)
  
  img <- url %>%
    html_nodes(xpath='//*[@id="cA-DD-cards-details-banner-top-new"]/div/div[1]/div[2]/img') %>%
    html_attr("src")
  img <- paste0('https://www.citi.com', img)
  citiIMG <- c(citiIMG, img)
  
  i <- i + 1
}


citi$IntroOffer <- citiIntro

citi$Cash <- 0
citi$Points <- as.numeric(ifelse(grepl(' [0-9]{1,3},[0-9]{3}', citi$IntroOffer), 
                      gsub('.*? ([0-9]{1,3}),([0-9]{3}).*', '\\1\\2', citi$IntroOffer), 0))
citi$Nights <- ifelse(grepl('nights', citi$IntroOffer), 2, 0)


citiCredit[is.na(citiCredit)] <- 0
citi$Credit <- citiCredit


citiFee <- pg %>%
  html_nodes(xpath = "//*[@class='annual-fee']") %>%
  html_text()
citi$FeeWaived1stYr <- ifelse(grepl('waiv', citiFee), 1, 0)
citi$Fee <- as.numeric(ifelse(grepl('\\$[0-9]{2,3}', citiFee),
                   gsub('.*?\\$([0-9]{2,3}).*', '\\1', citiFee), 0))


citiSpend <- gsub(',', '', citiIntro)
citiSpend <- ifelse(grepl('[spending|making|after] \\$([0-9]{3,4})', citiSpend),
                     gsub('.*?[spending|making|after] \\$([0-9]{3,4}).*', '\\1', citiSpend), 0)
citiSpend <- as.numeric(gsub('[^0-9]', '', citiSpend))
citi$Spend <- citiSpend

citi$img <- citiIMG




for(p in rates[,1]){
  citi$Program <- ifelse(grepl(p, citi$CardName), p, citi$Program)
}
for(p in rates[,1]){
  citi$Program <- ifelse(grepl(p, citi$IntroOffer), p, citi$Program)
}

write.csv(citi, 'citi.csv')

