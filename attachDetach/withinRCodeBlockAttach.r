
# to save time and make life easier

attach(datStata14)

dat <- data.frame(Score,sa)
pairs(dat)

detach()

# rather than

dat <- data.frame(datStata14$Score,datStata14$sa)
pairs(dat)


