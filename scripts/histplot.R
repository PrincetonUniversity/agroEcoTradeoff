maize <- data.frame(c_prob$maize)
maize$crop <- 'maize'
names(maize) <- c("conversion_probabilities", "crop")

cassava <- data.frame(c_prob$cassava)
cassava$crop <- 'cassava'
names(cassava) <- c("conversion_probabilities", "crop")

ground <- data.frame(c_prob$ground)
ground$crop <- 'ground'
names(ground) <- c("conversion_probabilities", "crop")

cotton <- data.frame(c_prob$cotton)
cotton$crop <- 'cotton'
names(cotton) <- c("conversion_probabilities", "crop")

soy <- data.frame(c_prob$soy)
soy$crop <- 'soy'
names(soy) <- c("conversion_probabilities", "crop")

pulse <- data.frame(c_prob$soy)
pulse$crop <- 'pulse'
names(pulse) <- c("conversion_probabilities", "crop")

sunflower <- data.frame(c_prob$sunflower)
sunflower$crop <- 'sunflower'
names(sunflower) <- c("conversion_probabilities", "crop")

sugarcane <- data.frame(c_prob$sugarcane)
sugarcane$crop <- 'sugarcane'
names(sugarcane) <- c("conversion_probabilities", "crop")

wheat <- data.frame(c_prob$wheat)
wheat$crop <- 'wheat'
names(wheat) <- c("conversion_probabilities", "crop")


cropCp <- rbind(maize, cassava, ground, cotton,
                soy, pulse, sunflower, sugarcane, wheat)

ggplot(cropCp, aes(conversion_probabilities, fill = crop)) + geom_density(alpha = 0.1)
