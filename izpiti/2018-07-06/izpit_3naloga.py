#===== 3. NALOGA =========
import random
from functools import lru_cache

# a)
def obrni_niz (niz):
    return niz[::-1]

def simetricen(niz):
    return niz == obrni_niz(niz)

# b)

def najdaljsi_simetricni(niz, start=0):
    if simetricen(niz):
        return niz, start
    else:
        odstrani_z = niz[1:len(niz)]
        odstrani_k = niz[0: len(niz) - 1]
        z, start_z = najdaljsi_simetricni(odstrani_z, start + 1)
        k, start_k = najdaljsi_simetricni(odstrani_k, start)
        if len(z) >= len(k):
            return z, start_z
        else:
            return k, start_k

def razdeli(niz):
    najdaljsi, index = najdaljsi_simetricni(niz)
    sez = [najdaljsi]
    if niz == najdaljsi:
        sez = sez.append(niz)
        return sez
    else:
        zacetek, i_z = najdaljsi_simetricni(najdaljsi[0: index])
        razd_z = razdeli(zacetek)
        konec, i_k = najdaljsi_simetricni(najdaljsi[index + 1:])
        razd_k = razdeli(konec)
        return razd_z + razd_k