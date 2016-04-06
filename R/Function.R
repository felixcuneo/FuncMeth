#formatR::tidy_dir("R")
################### La fonction
#install.packages("lintr")
#lintr::lint_package()
FMTalltogether = function(DATA, nc = 5, ni = 30) {
    ### nc = nombre de composante qu'on veut retenir ###ni = nombre d'itération qu'on veut faire

    cormat = cor(DATA)

    EGV = eigen(cormat)
    V = EGV$value
    VE = EGV$vector
    N = length(V)
    #### parenthèse 11 février VE=SameSense(VE) ###try pARENTHèSE 11 février nc=5 #nombre de
    #### composantes qu'on veut garder
    SatMat = matrix(NA, N, nc)
    for (i in 1:nc) {
        SatMat[, i] = sqrt(V[i]) * VE[, i]
    }
    ### les itérations de PCA pour que çA converge (hypersphère orthogonale) ni=30
    for (i in 1:ni) {
        NewData = t(apply(SatMat, 1, ToNorm))
        EGV = eigen(cor(NewData))
        V = EGV$value
        VE = EGV$vector
        #### parenthèse 11 février VE=SameSense(VE) ###try pARENTHèSE 11 février
        N = length(V)
        SatMat = scale(NewData) %*% VE[, 1:nc]  ###c'est donc le SatMat qu'on modifie, et on rerun le tout sur lui après -> but que la norme des vecteurs soient la même
    }

    # cor(SatMat) ; apply(SatMat,1,function(x) sqrt(sum(x^2)))
    KZ = SatMat


    coh = apply(DATA, 1, GetCoherence, KZ)
    # list(coh,KZ,DATA) pairs=pairing2(DATA)
    pairs = pairing2(KZ)
    KZA = KZ[pairs[, 1], ]
    KZB = KZ[pairs[, 2], ]
    PARTA = as.matrix(DATA[, pairs[, 1]])
    PARTB = as.matrix(DATA[, pairs[, 2]])

    StratA = matrix(0, dim(DATA)[1], dim(KZ)[2])
    StratB = matrix(0, dim(DATA)[1], dim(KZ)[2])

    for (i in 1:dim(DATA)[1]) {
        #### peut-être qu'il faut voir ça avec KZ normé et produit scalaire
        for (j in 1:dim(KZ)[2]) {
            StratA[i, j] = cor(PARTA[i, ], KZA[, j])
            StratB[i, j] = cor(PARTB[i, ], KZB[, j])

        }
    }


    SAN = t(apply(StratA, 1, ToNorm))
    SBN = t(apply(StratB, 1, ToNorm))
    # on norme les stratégies pour que leur produit scalaire 'corresponde' à une corrélation
    a = rep(0, dim(DATA)[1])
    for (i in 1:dim(DATA)[1]) {
        a[i] = SAN[i, ] %*% SBN[i, ]
    }
    ### response mean et sd : marche seulement avant l'inversion, sinon c useless
    moyvec = apply(DATA, 1, mean)
    sdvec = apply(DATA, 1, sd)


    list(coherence = coh, reliability = a, KZ = KZ, Rmean = moyvec, Rsd = sdvec, StratA = StratA,
        StratB = StratB, KZA = KZA, KZB = KZB, pairs = pairs, PA = PARTA, PB = PARTB)

}


