# get n depo from
# https://doi.org/10.3334/ORNLDAAC/830
# download.file('https://daac.ornl.gov/orders/9da27da7ef6c170af5c86377c9883a65/1860_1993_2050_NITROGEN/data/N-deposition1993.tfw',
#               ,destfile = 'data/nDepo/nDepo.tfw')
# 
# download.file('https://daac.ornl.gov/orders/9da27da7ef6c170af5c86377c9883a65/1860_1993_2050_NITROGEN/data/N-deposition1993.tif',
#               ,destfile = 'data/nDepo/nDepo.tif',cacheOK = F)

nDepo.ra <- rast('data/nDepo/N-deposition1993.tif')
# plot(nDepo.ra)
