# === 3. NALOGA ===

def skok_z_najvec_energije(mocvara):
    #vrne indeks elementa, ki iz zacetka mocvare prinese najvec energije
    e = mocvara[0]
    moznosti_za_skok = mocvara[1 : e + 1]
    energije = []
    for i in range(len(moznosti_za_skok)):
        energije.append(e + moznosti_za_skok[i] - (i + 1))
    return energije

def skoci(mocvara, start):
    #naredi skok, če lahko v eni rundi pride ven iz mocvirja....


def iz_mocvirja(mocvara):
    stevec = 0
    for i in range(len(mocvara)):
        del1 = mocvara[0:mocvara[0] + 1]
        indeks = skok_z_najvec_energije(del1)
        stevec += 1
        if indeks > len(mocvara):
            return stevec




        







