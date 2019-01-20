##############################################################################
# Želimo definirati pivotiranje na mestu za tabelo [a]. Ker bi želeli
# pivotirati zgolj dele tabele, se omejimo na del tabele, ki se nahaja med
# indeksoma [start] in [end].
#
# Primer: za [start = 0] in [end = 8] tabelo
#
# [10, 4, 5, 15, 11, 2, 17, 0, 18]
#
# preuredimo v
#
# [0, 2, 5, 4, 10, 11, 17, 15, 18]
#
# (Možnih je več različnih rešitev, pomembno je, da je element 10 pivot.)
#
# Sestavi funkcijo [pivot(a, start, end)], ki preuredi tabelo [a] tako, da bo
# element [ a[start] ] postal pivot za del tabele med indeksoma [start] in
# [end]. Funkcija naj vrne indeks, na katerem je po preurejanju pristal pivot.
# Funkcija naj deluje v času O(n), kjer je n dolžina tabele [a].
#
# Primer:
#
#     >>> a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
#     >>> pivot(a, 1, 7)
#     3
#     >>> a
#     [10, 2, 0, 4, 11, 15, 17, 5, 18]
##############################################################################
def zamenjaj(a, i, j):
    ''' Funkcija zamenja elementa z indeksom i in indeksom j v seznamu a.'''
    a[i], a[j] = a[j], a[i]
    return a

def pivot(a, start, end):
    if len(a[start:end]) == 1:
        return 0
    pivot = a[start]
    indeks = start + 1
    for i in range(start + 1, end):
        if a[i] < pivot:
            a = zamenjaj(a, indeks, i)
            indeks = indeks + 1
    a = zamenjaj(a, start, indeks - 1)
    return indeks - 1

##############################################################################
# Tabelo a želimo urediti z algoritmom hitrega urejanja (quicksort).
#
# Napišite funkcijo [quicksort(a)], ki uredi tabelo [a] s pomočjo pivotiranja.
# Poskrbi, da algoritem deluje 'na mestu', torej ne uporablja novih tabel.
#
# Namig: Definirajte pomožno funkcijo [quicksort_part(a, start, end)], ki
#        uredi zgolj del tabele [a].
#
#   >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#   >>> quicksort(a)
#   [2, 3, 4, 5, 10, 11, 15, 17, 18]
##############################################################################

def quicksort_part(a, start, end):
    indeks_p = pivot(a, start, end) 
    if len(a[start:indeks_p + 1]) == 1:
        return a
    if len(a[indeks_p + 1 : end]) == 1:
        return a
    else:
        manjsi = quicksort_part(a, start, indeks_p)
        vecji = quicksort_part(a, indeks_p + 1, end)
        return a
    

def quicksort(a):
    return quicksort_part(a, 0, len(a))
        

##############################################################################
# V tabeli želimo poiskati vrednost k-tega elementa po velikosti.
#
# Primer: Če je
#
# >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#
# potem je tretji element po velikosti enak 5, ker so od njega manši elementi
#  2, 3 in 4. Pri tem štejemo indekse od 0 naprej, torej je "ničti" element 2.
#
# Sestavite funkcijo [kth_element(a, k)], ki v tabeli [a] poišče [k]-ti
# element po velikosti. Funkcija sme spremeniti tabelo [a]. Cilj naloge je, da
# jo rešite brez da v celoti uredite tabelo [a].
##############################################################################

def kth_element(a, k):
    index = pivot(a,0,len(a))
    if index == k:
        return a[index]
    elif index > k:
        return kth_element(a[0 : index], k)
    else:
        return kth_element(a[index + 1 : len(a)], k - index-1)

        

