import requests
import re
import os
import csv

###############################################################################
# Najprej definirajmo nekaj pomožnih orodij za pridobivanje podatkov s spleta.
###############################################################################

# definiratje URL glavne strani bolhe za oglase z mačkami
cats_frontpage_url = 'http://www.bolha.com/zivali/male-zivali/macke/'
# mapa, v katero bomo shranili podatke
cat_directory = '2-zajem-podatkov/vaje/podatki_macke'
# ime datoteke v katero bomo shranili glavno stran
frontpage_filename = 'macke_glavna_stran.html'
# ime CSV datoteke v katero bomo shranili podatke
csv_filename = 'podatki_macke.csv'

def download_url_to_string(url):
    '''This function takes a URL as argument and tries to download it
    using requests. Upon success, it returns the page contents as string.'''
    try:      
        # del kode, ki morda sproži napako
        r = requests.get(url)
        print("download successful")
    except requests.exceptions.ConnectionError:
        # koda, ki se izvede pri napaki
        # dovolj je če izpišemo opozorilo in prekinemo izvajanje funkcije
        print("Stran ne obstaja")
        return None
    # nadaljujemo s kodo če ni prišlo do napake
    return r.text  #vrne texst v obliki niza


def save_string_to_file(text, directory, filename):
    '''Write "text" to the file "filename" located in directory "directory",
    creating "directory" if necessary. If "directory" is the empty string, use
    the current directory.'''
    os.makedirs(directory, exist_ok=True)    #ustvari direktorij, če je že tam je OK
    path = os.path.join(directory, filename) #naredi pot
    with open(path, 'w', encoding='utf-8') as datoteka:  #odpri pot za writing in kodiranje je utf, dobimo nov dokument
        datoteka.write(text)        #zapiše text v datoteko (ta text je pa prej pobrau iz neta in je niz)
    return None

# Definirajte funkcijo, ki prenese glavno stran in jo shrani v datoteko.


def save_frontpage():               #je brez argumentov, ker je to samo funkcija, ki nam olajša delo
    '''Save "cats_frontpage_url" to the file
    "cat_directory"/"frontpage_filename"'''
    besedilo = download_url_to_string(cats_frontpage_url)
    return save_string_to_file(besedilo, cat_directory, frontpage_filename)

###############################################################################
# Po pridobitvi podatkov jih želimo obdelati.
###############################################################################


def read_file_to_string(directory, filename):  #ta bo delala to, da bo prebrala tisto datoteko, ki smo jo prej naredili
    '''Return the contents of the file "directory"/"filename" as a string.'''
    path = os.path.join(directory, filename)
    with open(path, 'r', encoding='utf-8') as file_in:
        return file_in.read()
    return None
    

# Definirajte funkcijo, ki sprejme niz, ki predstavlja vsebino spletne strani,
# in ga razdeli na dele, kjer vsak del predstavlja en oglas. To storite s
# pomočjo regularnih izrazov, ki označujejo začetek in konec posameznega
# oglasa. Funkcija naj vrne seznam nizov.

vzorec_bloka = re.compile(r'<div class="ad">.*?'     
    r'<div class="clear"></div>', 
    re.DOTALL)

def page_to_ads(page):    #to so tisti bloki s posamezno reklamo
    '''Split "page" to a list of advertisement blocks.'''
    ads = re.findall(vzorec_bloka, page)
    return ads

# Definirajte funkcijo, ki sprejme niz, ki predstavlja oglas, in izlušči
# podatke o imenu, ceni in opisu v oglasu.

vzorec_reklame = re.compile(
    r'<table><tr><td><a title=(?P<ime>.+?)\s*?href=.*?'
    r'</a></h3>\s*?(?P<opis>.+?)\s*?<div class="additionalInfo">.*?'
    r'<div class="price">(?P<cena>.+?)</div>.*?', 
    flags = re.DOTALL
)

def get_dict_from_ad_block(blok):
    '''Build a dictionary containing the name, description and price
    of an ad block.'''
    reklama = vzorec_reklame.search(blok)
    slovar = reklama.groupdict()
    return slovar

# Definirajte funkcijo, ki sprejme ime in lokacijo datoteke, ki vsebuje
# besedilo spletne strani, in vrne seznam slovarjev, ki vsebujejo podatke o
# vseh oglasih strani.


def ads_from_file(directory, filename):
    '''Parse the ads in filename/directory into a dictionary list.'''
    #read the file
    page = read_file_to_string(directory, filename)
    #split the page into blocks
    bloki = page_to_ads(page)
    #for each blok, get out the dict
    seznam = []
    for i in range(len(bloki)):
        seznam.append(get_dict_from_ad_block(bloki[i]))
    #return the list of dict's
    return seznam

###############################################################################
# Obdelane podatke želimo sedaj shraniti.
###############################################################################


def write_csv(fieldnames, rows, directory, filename):
    '''Write a CSV file to directory/filename. The fieldnames must be a list of
    strings, the rows a list of dictionaries each mapping a fieldname to a
    cell-value.'''
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)
    return None

# Definirajte funkcijo, ki sprejme neprazen seznam slovarjev, ki predstavljajo
# podatke iz oglasa mačke, in zapiše vse podatke v csv datoteko. Imena za
# stolpce [fieldnames] pridobite iz slovarjev.


def write_cat_ads_to_csv(ads):
    fieldnames = ads[0].keys()
    write_csv(fieldnames, ads, cat_directory, csv_filename)
    return None

#TODO: popravi vzorec  za opis!
