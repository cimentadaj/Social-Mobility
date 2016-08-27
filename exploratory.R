

countries3 <- list(Italy=prgitap1, Czech=prgczep1, Slovakia=prgsvkp1,
                   Japan=prgjpnp1)

attempt <- lapply(countries3, function(x) subset(x, !is.na(lowerclass) & !is.na(pvnum1) &
                      !is.na(non.cognitive) & !is.na(highisced)))

lapply(attempt, function(x) table(x$highisced))


attempt2 <- lapply(countries3, function(x) subset(x, !is.na(serviceclass) & !is.na(pvnum1) &
                                                     !is.na(non.cognitive) & !is.na(lowisced)))

lapply(attempt2, function(x) table(x$lowisced))