
try.df <-read.delim(file = 'data/leafn15/23086.txt', header = TRUE, sep = "\t", dec = ".")

# 
with_year <- unique(try.df$StdValueStr)[117:437]

try.df.sub <- try.df[try.df$StdValueStr %in% with_year,]

unique(try.df$StdValue)

try.df.sub <- try.df.sub[!is.na(try.df.sub$StdValue),]

# 
with_gps <- unique(try.df$StdValue)