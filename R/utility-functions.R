InverseData = function(DATA, keys = NULL, likert.min, likert.max) {
    InvDATA = DATA
    InvDATA[, keys] = likert.max + likert.min - DATA[, keys]
    InvDATA
}

# vec=c(1, 9, 12, 14, 15, 16, 18, 19, 23, 24, 27, 28, 30, 31, 33, 39, 42, 44, 45, 46, 48,
# 54, 55, 57, 59) NEOinv=InverseData(NEO,vec,1,5)

SameSense = function(VE) {
    Toinverse = apply(VE, 2, sum) < 0
    for (i in 1:nc) {
        if (Toinverse[i] == TRUE) {
            VE[, i] = -1 * VE[, i]
        }
    }
    VE
}
#########################


ToNorm = function(oneline) {
    oneline/sqrt(sum(oneline^2))
}

GetCoherence = function(OneR, FinalData) {
    sqrt(sum((apply(FinalData, 2, cor, OneR))^2))  ##one R c'est un sujet, on regarde simplement la racine de la somme des carrés des corrélations entre la matrice de saturation(final data, colonnes) et le vec de réponse du sujet
}

pairing2 = function(KZbrut) {
    IProd = KZbrut %*% t(KZbrut)
    IProd[lower.tri(IProd, diag = TRUE)] <- NA
    IPmat = IProd

    pairs = matrix(0, dim(IPmat)[2]/2, 2)
    for (i in 1:(dim(IPmat)[2]/2)) {
        pairs[i, ] = which(IPmat == max(IPmat, na.rm = TRUE), arr.ind = TRUE)[1, ]
        IPmat[, pairs[i, ]] = NA
        IPmat[pairs[i, ], ] = NA
    }
    pairs
}
