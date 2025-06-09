test_that("La fonction nbha fonctionne ok", {
  nb <- nbha(1, 0.04)
  expect_equal(nb, 25)
})

test_that("La fonction nbha fonctionne ok dans un data", {
  fic1 <- data.frame(placette=1,
                     noarbre=1:3,
                     essence='ERS',
                     dhpcm = c(10,11,12),
                     vol = NA,
                     nb = 1:3,
                     sup = 0.04)

  fic1$tige_ha <- nbha(fic1$nb, fic1$sup)
  fic1 <- fic1 %>% mutate(tige_ha2 = nbha(nb, sup))

  expect_equal(fic1$tige_ha, c(25,50,75))
  expect_equal(fic1$tige_ha2, c(25,50,75))

})

########################################################
########################################################

test_that("La fonction stm2ha fonctionne ok", {
  st <- stm2ha(10, 2, 0.25)
  expect_equal(round(st,6), 0.062832)
})

test_that("La fonction stm2ha fonctionne ok dans un data", {
  fic1 <- data.frame(placette=1,
                     noarbre=1:3,
                     essence='ERS',
                     dhpcm = c(10,11,12),
                     vol = NA,
                     nb = 1:3,
                     sup = 0.04)
  fic1$st_ha <- stm2ha(fic1$dhpcm, fic1$nb, fic1$sup)
  fic1 <- fic1 %>% mutate(st_ha2 = stm2ha(dhpcm, nb, sup))
  expect_equal(round(fic1$st_ha,6), c(0.196350, 0.475166, 0.848230))
  expect_equal(round(fic1$st_ha2,6), c(0.196350, 0.475166, 0.848230))

})

########################################################
########################################################

test_that("La fonction volm3ha fonctionne ok", {
  vol <- volm3ha(500, 2, 0.25)
  expect_equal(vol, 4)
})

test_that("La fonction volm3ha fonctionne ok dans un data", {
  fic1 <- data.frame(placette=1,
                     noarbre=1:4,
                     essence=c('ERS','SAB'),
                     dhpcm = c(10,11,12,13),
                     vol = c(100,200,300,400),
                     nb = 1:4,
                     sup = 0.25)
  fic1$vol_ha <- volm3ha(fic1$vol, fic1$nb, fic1$sup)
  fic1 <- fic1 %>% mutate(vol_ha2 = volm3ha(vol, nb, sup))
  expect_equal(round(fic1$vol_ha,6), c(0.40,  1.60, 3.60, 6.40))
  expect_equal(round(fic1$vol_ha2,6), c(0.40,  1.60, 3.60, 6.40))

})

########################################################
########################################################

test_that("La fonction dqm fonctionne ok", {
  dq <- dqm(10, 100)
  expect_equal(round(dq,4), 35.6825)
})

test_that("La fonction dqm fonctionne ok dans un data", {
  fic1 <- data.frame(placette=1,
                     stha = c(1.5197,0.1671,0.2884),
                     nbha = c(150, 16, 24))
  fic1$dq <- dqm(fic1$stha, fic1$nbha)
  fic1 <- fic1 %>% mutate(dq2 = dqm(stha, nbha))
  expect_equal(round(fic1$dq,4), c(11.3576, 11.5314, 12.3694))
  expect_equal(round(fic1$dq2,4), c(11.3576, 11.5314, 12.3694))

})

########################################################
########################################################

test_that("La fonction calcul_var_dendro fonctionne ok", {

  fic1 <- data.frame(placette=1,
                     noarbre=1:3,
                     essence='ERS',
                     dhpcm = c(10,11,12),
                     vol = NA,
                     nb = 1:3,
                     sup = 0.04)
  fic2 <- data.frame(placette=2,
                     noarbre=1:4,
                     essence=c('ERS','SAB'),
                     dhpcm = c(10,11,12,13),
                     vol = c(100,200,300,400),
                     nb = 1:4,
                     sup = 0.25)
  fic <- rbind(fic1,fic2)
  #write_delim(fic,"test_dendro.csv", delim=';')

  obtenu <- calcul_var_dendro(data=fic, dhp=dhpcm, voldm3=vol, nombre=nb, superficie=sup, group_by_vars=c('placette','essence'))


  attendu_Ti_ha <- c(150,16,24)
  attendu_ST_m2ha <- c(1.5197454, 0.1671327, 0.2883982)
  attendu_Vol_m3ha <- c(NA,4,8)
  attendu_DQM_cm <- c(11.35782, 11.53256, 12.36932)

  expect_equal(obtenu$Ti_ha, attendu_Ti_ha)
  expect_equal(round(obtenu$ST_m2ha,7), attendu_ST_m2ha)
  expect_equal(obtenu$Vol_m3ha, attendu_Vol_m3ha)
  expect_equal(round(obtenu$DQM_cm,5), attendu_DQM_cm)

})


test_that("La fonction calcul_var_dendro fonctionne ok sans group_by_vars", {

  fic1 <- data.frame(placette=1,
                     noarbre=1:3,
                     essence='ERS',
                     dhpcm = c(10,11,12),
                     vol = 0,
                     nb = 1:3,
                     sup = 0.04)
  fic2 <- data.frame(placette=2,
                     noarbre=1:4,
                     essence=c('ERS','SAB'),
                     dhpcm = c(10,11,12,13),
                     vol = c(100,200,300,400),
                     nb = 1:4,
                     sup = 0.25)
  fic <- rbind(fic1,fic2)
  #write_delim(fic,"test_dendro.csv", delim=';')

  obtenu <- calcul_var_dendro(data=fic, dhp=dhpcm, voldm3=vol, nombre=nb, superficie=sup)


  expect_equal(obtenu$Ti_ha, 190)
  expect_equal(round(obtenu$ST_m2ha,4), 1.9753)
  expect_equal(obtenu$Vol_m3ha, 12)
  expect_equal(round(obtenu$DQM_cm,4), 11.5051)

})


########################################################
########################################################

test_that("La fonction calcul_hdom fonctionne ok avec group_by_vars", {

  fic1 <- data.frame(placette=1,
                     noarbre=1:5,
                     essence='ERS',
                     haut = c(10,11,12,13,14),
                     nb = 1,
                     sup = 0.04)
  fic2 <- data.frame(placette=2,
                     noarbre=1:5,
                     essence='SAB',
                     haut = c(9,10,11,12,13),
                     nb = c(4,10,8,4,4),
                     sup = 0.25)
  fic <- rbind(fic1,fic2)

  obtenu <- calcul_hdom(data=fic, ht=haut, nombre=nb, superficie=sup, group_by_vars=c("placette", "essence"))

  expect_equal(round(obtenu$HDom_m,2), c(12.50,11.12))

})

test_that("La fonction calcul_hdom fonctionne ok sans group_by_vars", {

  fic <- data.frame(placette=2,
                     noarbre=1:5,
                     essence='SAB',
                     haut = c(9,10,11,12,13),
                     nb = c(4,10,8,4,4),
                     sup = 0.25)

  obtenu <- calcul_hdom(data=fic, ht=haut, nombre=nb, superficie=sup)

  expect_equal(round(obtenu$HDom_m,2), 11.12)

})



