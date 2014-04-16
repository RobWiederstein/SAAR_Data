#Grad Rate 2003-2007 was sent via email from Kentucky Department of Education
#No longer available on website

wd <- getwd()
file <- paste (wd, "data sets", "Grad Rate 2003-2007.csv", sep = "/")
gr <- read.csv (file = file, sep = ",", header = TRUE,
                strip.white = TRUE,
                colClasses = "character")

#pull out columns for 4 year graduates
attach (gr)
gr <- gr [, c(4, 5, grep ("GRADS_4YR", names (gr)))]
gr <- gr [SCHNAME == "---DISTRICT TOTAL---", ]
gr <- gr [, -2]
detach (gr)

#Save as R object to load in later script
gr.new <- paste (wd, "objects", "gr.2003.2007.csv", sep = "/")
write.csv(gr, file = gr.new)

#clear workspace
rm (list = ls())
