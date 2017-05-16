#download.file('https://home.barclaycardus.com/cards.list.html', destfile = '1.html')
#bcURL <- read_html('1.html')

bcURL <- read_html('https://home.barclaycardus.com/cards.list.html')

bcCardName <- bcURL %>%
  html_nodes("[class=bcus-card-results__list-card-name]") %>%
  html_text()
bcCardName <- gsub('[^A-z&-+\']', ' ', bcCardName)
bcCardName <- str_trim(gsub('\\s{2,}', ' ', bcCardName))


bcCardOffer <- bcURL %>%
  html_nodes("[class=bcus-card-results__list-highlights]") %>%
  html_text()

offer <- c()
for(o in bcCardOffer){
  o <- strsplit(o, '\r\n')[[1]]
  o[!grepl('\n', o)] <- '~'
  o <- paste(o, sep = '~', collapse = '')
  o <- gsub('\\s{2,}', ' ', o)
  o <- strsplit(o, '\n')[[1]]
  o <- gsub('~*', '', o)
  o <- str_trim(o)
  o <- o[grepl('[B|b]onus', o)][1]
  offer <- c(offer, o)
}

bcCardOffer <- gsub('\\s{2,}', ' ', bcCardOffer)
bcCardOffer <- str_trim(bcCardOffer)

feeWaived <- ifelse(grepl('waiv', bcCardOffer), 1, 0)

bcFee <- as.numeric(gsub('.*?\\$(\\d{1,2}) annual fee.*', '\\1', bcCardOffer))

bcCardOffer <- offer

bcLinks <- bcURL %>%
  html_nodes('[class=bcus-card-results__list-card-link]') %>%
  html_attr("href")
bcLinks <- paste0('https://home.barclaycardus.com', bcLinks)

bcIMG <- bcURL %>%
  html_nodes('[class="bcus-card-results__list-card-image"]') %>%
  html_attr("src")
bcIMG <- paste0('https://home.barclaycardus.com', bcIMG)

bc <- data.frame(CardName = bcCardName,
                 Issuer = 'Barclay',
                 Program = 'Barclaycard Arrival Miles',
                 Link = bcLinks,
                 IntroOffer = bcCardOffer, stringsAsFactors = FALSE)


bcSpend <- c()
bcCash <- c()
bcPoints <- c()
i <- 1
for(l in bcLinks){
  writeLines(paste0(i))
  
  #download.file(paste0(l), destfile = "1.html", quiet=TRUE)
  #content <- read_html("1.html")
  
  url <- read_html(paste0(l))
  offer <- url %>%
    html_nodes(xpath="//*[@class='bcus-card-details__rewards-benefits-list bcus-grid__two-column bcus-card-details__rewards-benefits-list--left']") %>%
    html_text()
  offer <- gsub(',', '', offer)

  
  spend <- offer[grepl('[spend|spending] \\$(\\d{3,4})', offer)|grepl('\\$(\\d{3,4}) in purchases', offer)][1]
  spend1 <- as.numeric(ifelse(!is.na(spend) & grepl('[spend|spending] \\$(\\d{3,4})', offer),
                              gsub('.*?[spend|spending] \\$(\\d{3,4}).*', '\\1', spend), 0))
  spend2 <- as.numeric(ifelse(!is.na(spend) & grepl('\\$(\\d{3,4}) in purchases', offer) ,
                              gsub('.*?\\$(\\d{3,4}) in purchases.*', '\\1', spend), 0))
  spend <- max(spend1, spend2)
  bcSpend <- c(bcSpend, spend)
  
  cash   <- offer[grepl('cash', offer)][1]
  cash   <- as.numeric(ifelse(!is.na(cash), 
                              gsub('.*?\\$(\\d{3}).*', '\\1', offer), 0))
  cash <- ifelse(cash == spend, 0, cash)
  
  points <- offer[grepl('\\s\\d{4,6}\\s', offer)][1]
  points <- as.numeric(ifelse(!is.na(points), 
                              gsub('.*?\\s(\\d{4,6})\\s.*', '\\1', offer), 0))
  
  
  bcCash <- c(bcCash, cash)
  bcPoints <- c(bcPoints, points)
  i <- i + 1
}

bc$Cash   <- bcCash
bc$Points   <- bcPoints
bc$Nights <- 0

bc$Credit <- 0

bc$FeeWaived1stYr <- feeWaived

bc$Fee <- bcFee

bc$Spend <- bcSpend

bc$img <- bcIMG


for(p in rates[,1]){
  bc$Program <- ifelse(grepl(p, bc$IntroOffer), p, bc$Program)
}
for(p in rates[,1]){
  bc$Program <- ifelse(grepl(p, bc$CardName), p, bc$Program)
}

write.csv(bc, 'bc.csv')


