airlines <- read.xlsx("C:/Users/Jon Kelley/Desktop/CreditCards/Rewards.xlsx", sheetName="Airlines")
hotels <- read.xlsx("C:/Users/Jon Kelley/Desktop/CreditCards/Rewards.xlsx", sheetName="Hotels")
categories <- read.xlsx("C:/Users/Jon Kelley/Desktop/CreditCards/Rewards.xlsx", sheetName="Categories")
programs <- data.frame(Company = c(airlines$Company, hotels$Company),
                       Program = c(airlines$Program_1, hotels$Program),
                       Tag = c(airlines$Company, hotels$Parent),
                       Type = c(airlines$Type, hotels$Type))


#banks <- c('MR', 'BC', 'UR', 'ThankYou')


url <- read_html('https://thepointsguy.com/category/points-valuations/')
url <- url %>%
  html_nodes(xpath='/html/body/div[1]/div[3]/section[2]/div/div[1]/div[2]/a') %>%
  html_attr("href")


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
                     }", url), con="scrape_boa.js")

system("phantomjs scrape_boa.js")

url <- read_html("1.html")

rates <- url %>%
  html_nodes(xpath='/html/body/div[1]/div[4]/section/div/div[2]/section[1]/table') %>%
  html_table(header=TRUE)
rates<-rates[[1]]
names(rates)[1] <- 'Call'
rates[c(2,3)] <-NULL

rates$Company <- ''
rates$Program <- ''
programs$Call <- ''

# TEST <- data.frame()
# for(i in unique(rates$Call)){
#   int <- row.names(programs[grepl(i, programs$Program), ])
#   
#   add <- cbind(rates[rates$Call==i,], int)
#   TEST <- rbind(TEST, add)
# }


rates[,2] <- str_trim(rates[,2])
rates[,2] <- as.numeric(ifelse(grepl('-', rates[,2]),
       (as.numeric(gsub('(.*?)-.*', '\\1', rates[,2])) + as.numeric(gsub('.*?-(.*)', '\\1', rates[,2])))/2,
       rates[,2]))
names(rates)[1] <- 'Program'
rates[,1] <- gsub('Miles', '', rates[, 1])
rates[,1] <- gsub(' & More', 'Miles & More', rates[, 1])
rates[,1] <- gsub('-', ' ', rates[, 1])
rates[,1] <- gsub('AAdvantage', '', rates[,1])
rates[,1] <- gsub('Guest Rewards', '', rates[,1])
rates[,1] <- gsub('Avios', '', rates[,1])
rates[,1] <- gsub('SkyMiles', '', rates[,1])
rates[,1] <- gsub('Sky', '', rates[,1])
rates[,1] <- gsub('Starpoints', '', rates[,1])
rates[,1] <- gsub('Korean Air', 'SKYPASS', rates[,1])
rates[,1] <- str_trim(rates[,1])
rates <- rates[!is.na(rates[,2]), ]

rates2 <- read_csv("C:/Users/Jon Kelley/Desktop/CreditCards/rates.csv")
rates2<-as.data.frame(rates2)

names(rates) <- names(rates2)
rates <- rbind(rates, rates2)
rates <- rates[!duplicated(rates$Program), ]

write.csv(rates, 'rates.csv', row.names = FALSE)
