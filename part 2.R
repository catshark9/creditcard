cards <- rbind(chase, bc, citi, amex, abus, boa, us)
cards$Credit <- ifelse(cards$Credit==1, 0, cards$Credit)


cards <- merge(x = cards, y = rates[1:2], by = "Program", all.x = TRUE)
names(cards)[length(cards)] <- 'Rate'
cards$Rate <- cards$Rate/100
cards$Rate[is.na(cards$Rate)] <- 0.01

# manual redepton rates
#cards[cards$CardName=='Chase Sapphire Preferred® credit card', 'Rate'] <- 0.0125
#cards[cards$CardName=='Chase Sapphire ReserveSM', 'Rate'] <- 0.015


#cards[cards$Nights!=0, 'Points'] <- 0

# targeted

cards[cards$CardName=='Citi Hilton HHonors Visa Signature Card', 'Points'] <- 75000
cards[cards$CardName=='United MileagePlus Explorer card', 'Points'] <- ifelse(Sys.Date() < as.Date('03/15/2017', "%m/%d/%Y"),
                                                                              70000,
                                                                              cards[cards$CardName=='United MileagePlus Explorer card', 'Points'])
cards[cards$CardName=='United MileagePlus Explorer card', 'Credit'] <- ifelse(Sys.Date() < as.Date('03/15/2017', "%m/%d/%Y"),
                                                                              50,
                                                                              cards[cards$CardName=='United MileagePlus Explorer card', 'Credit'])

cards[cards$CardName=='Citi / AAdvantage Platinum Select World Elite MasterCard', 'Points'] <- ifelse(Sys.Date() < as.Date('04/15/2017', "%m/%d/%Y"),
                                                                               60000,
                                                                               cards[cards$CardName=='Citi / AAdvantage Platinum Select World Elite MasterCard', 'Points'])





cards$Value <- cards$Points*cards$Rate + cards$Cash + cards$Nights*150 + cards$Credit - abs(cards$FeeWaived1stYr-1)*cards$Fee


names(cards)[length(cards)] <- paste0('x', Sys.Date())

write.csv(cards, file = 'cards2.csv', row.names=FALSE)



cards <- read_csv("C:/Users/Jon Kelley/Desktop/CreditCards/cards2.csv")
cards2 <- read_csv("C:/Users/Jon Kelley/Desktop/CreditCards/currentCards.csv")

cards3 <- cards2[!(cards2$CardName%in%cards$CardName), ]

cards <- merge(x = cards, y = cards2[c(1, 18:length(cards2))], by = "CardName", all.x = TRUE)
cards$MaxValue <- apply(cards[,c(15:length(cards))], 1, max, na.rm=TRUE)
cards$isMax <- cards$MaxValue <= cards[15]
cards$newMax <- ifelse(cards$isMax==TRUE & cards[15]>cards[16], TRUE, FALSE)


cards <- bind_rows(cards, cards3)
cards <- data.frame(cards[1:14], cards[(length(cards)-2):length(cards)], cards[15:(length(cards)-3)])

Encoding(cards$IntroOffer) <- "UTF-8"

cards <- cards[!duplicated(cards$CardName), ]

write.csv(cards, file = 'currentCards.csv', row.names=FALSE)
write.csv(cards, file = paste0('cards', Sys.Date(), '.csv'), row.names=FALSE)

