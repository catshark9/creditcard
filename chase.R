chaseURL <- read_html('https://creditcards.chase.com/credit-cards/browse-all?iCELL=61FY&jp_ltg=chsecate_allcards')

# Card Names
chaseCardName <- chaseURL %>%
  html_nodes("[id=LearnMoreLinkCardName]") %>%
  html_text()
chaseCardName <- gsub('SM', '', chaseCardName)
chaseCardName <- gsub('credit card', '', chaseCardName)
chaseCardName <- gsub('[^A-z&-+\']', ' ', chaseCardName)
chaseCardName <- str_trim(gsub('\\s{2,}', ' ', chaseCardName))

# Card Offer
chaseCardIntro <- chaseURL %>%
  html_nodes("[class=cardmember-offer]") %>%
  html_text()
chaseCardIntro <- gsub('NEW CARDMEMBER OFFER', '', chaseCardIntro)
chaseCardIntro <- gsub('\\s{2,}', ' ', chaseCardIntro)

# Card Links
chaseLinks <- chaseURL %>%
  html_nodes('[id=LearnMoreLinkCardName]') %>%
  html_attr("href")

# Create Data Frame
chase <- data.frame(CardName = chaseCardName,
                    Issuer = 'Chase',
                    Program = 'Chase Ultimate Rewards',
                    Link = chaseLinks,
                    IntroOffer = chaseCardIntro, stringsAsFactors = FALSE)

# Intro Reward Types
chase$Cash   <- as.numeric(ifelse(grepl('\\$\\d{2,3} .*?after', chaseCardIntro), 
                       gsub('( \\$)(\\d{2,3})( .*$)', '\\2', chaseCardIntro), 0))
chase$Points <- as.numeric(ifelse(grepl(' \\d{1,3},\\d{3} ', chaseCardIntro), 
                       gsub('(\\d{1,3}),(\\d{3})( .*$)', '\\1\\2', chaseCardIntro), 0))
chase$Nights <- ifelse(grepl('nights', chaseCardIntro), 
                       2, 0)
  # some say two instead of two

# loops through links to get statement credit
chaseCredit <- c()
i <- 1
for(l in chaseLinks){
  writeLines(paste0(i))
  url <- read_html(paste0(l))
  credit <- url %>%
    html_nodes("[class=primary-item-title]") %>%
    html_text()
  credit <- credit[grepl('[T|t]ravel [C|c]redit', credit)]
  credit <- ifelse(length(credit)==0, 0, credit)
  credit <- as.numeric(gsub('\\D', '', credit))
  chaseCredit <- c(chaseCredit, credit)
  
  i <- i + 1
}

chase$Credit <- chaseCredit

chaseCardFee <- chaseURL %>%
  html_nodes("[class=annual-fee]") %>%
  html_text()
chaseCardFee <- gsub('\\s{2,}', ' ', chaseCardFee)

chase$FeeWaived1stYr <- ifelse(grepl('\\$0.*first year', chaseCardFee), 1, 0)

chaseCardFee <- gsub('\\$0', '', chaseCardFee)
chaseCardFee <- gsub('(;)(.*$)', '\\1', chaseCardFee)


chase$Fee <- 0
chase$Fee <- as.numeric(ifelse(grepl('\\d', chaseCardFee), 
                    gsub('\\D', '', chaseCardFee), chase$Fee))


chaseSpend <- gsub(',', '', chaseCardIntro)
chase$Spend <- as.numeric(ifelse(grepl('[spend|make] \\$(\\d{3,4})', chaseSpend),
                     gsub('.*?[spend|make] \\$(\\d{3,4}).*', '\\1', chaseSpend), 0))

chase$img <- chaseURL %>%
  html_nodes('[class="img-responsive card"]') %>%
  html_attr("src")


for(p in rates[,1]){
  chase$Program <- ifelse(grepl(p, chase$CardName), p, chase$Program)
}

write.csv(chase, 'chase.csv')





