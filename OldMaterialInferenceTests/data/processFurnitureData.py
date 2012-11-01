import sys
import csv
from collections import namedtuple
from nltk.corpus import wordnet as wn
import re

def separateCollectionsAndItems(filename):
    
    # Read
    f = open(filename, 'r')
    csvreader = csv.DictReader(f)
    rows = []
    for row in csvreader:
        rows.append(row)
    collectionRows = [row for row in rows if row['source'] == 'furniturecom' and row['itemType'] == 'COLLECTION']
    itemRows = [row for row in rows if row['source'] == 'furniturecom' and row['itemType'] == 'SINGLE']
    setRows = [row for row in rows if row['source'] == 'furniturecom' and row['itemType'] == 'SET']
    f.close()
    
    def writeDataRows(filename, datarows):
        f = open(filename, 'wb')
        csvwriter = csv.DictWriter(f, csvreader.fieldnames)
        csvwriter.writeheader()
        csvwriter.writerows(datarows)
        f.close()
    
    # Write
    writeDataRows('furnitureCollections.csv', collectionRows)
    writeDataRows('furnitureItems.csv', itemRows)
    writeDataRows('furnitureSets.csv', setRows)
    
    
materials = ['wood', 'fabric', 'fiber', 'leather', 'stone', 'glass', 'metal', 'plastic', 'foam', 'resin', 'plaster']
mappings = {'polypropylene' : 'fiber', 'polyurethane' : 'foam', 'mdf' : 'wood', 'olefin' : 'fiber', 'hydrocal' : 'plaster', 'denscast' : 'plaster'}
def quantizeMaterial(matString):
    
    lowerString = matString.lower()
    
    # First, see if any of the materials occur as substrings
    # (or if any hardcoded mapped terms exist)
    directMats = filter(lambda mat: lowerString.find(mat) >= 0, materials)
    if len(directMats):
        return directMats[0]
    mappedMats = [mappings[m] for m in filter(lambda m: lowerString.find(m) >= 0, mappings.keys())]
    if len(mappedMats):
        return mappedMats[0]
    
    # If not, check if any of the words in the string have materials
    # as their direct hypernyms
    
    def materialHypernym(word):
        all_names = []
        synsets = wn.synsets(word, pos=wn.NOUN)
        for synset in synsets:
            hypernyms = synset.hypernyms()
            for hypernym in hypernyms:
                all_names.extend(hypernym.lemma_names)
        all_names = set(all_names)
        # Done this way to enforce a preference for materials that occur
        # earlier in the list (higher priority)
        for mat in materials:
            if mat in all_names:
                return mat
        return None
    
    words = re.split(' |/', lowerString)
    for word in words:
        mhnym = materialHypernym(word)
        if mhnym:
            return mhnym
    
    # At this point, we don't know anything; bail with 'None'
    #return matString
    return None
        

reducedFieldnames = ['collectionId', 'id', 'name', 'collectionName', 'subordinateFurnitureType', 'detailsUrl', 'material', 'furnitureType', 'roomType', 'color']
def generateLivingRoomDataSet():
    
    # First, find collection ids of all 'living room' collections
    f = open('furnitureCollections.csv', 'r')
    reader = csv.DictReader(f)
    livingRoomIds = [row['collectionId'] for row in reader if row['roomType'] == 'LIVING']
    livingRoomIds = set(livingRoomIds)
    f.close()
    
    inf = open('furnitureItems.csv', 'r')
    outf = open('livingRoomItems.csv', 'wb')
    reader = csv.DictReader(inf)
    writer = csv.DictWriter(outf, reducedFieldnames)
    writer.writeheader()
    for row in reader:
        reducedRow = {k:v for k,v in row.iteritems() if k in reducedFieldnames}
        if reducedRow['collectionId'] in livingRoomIds and reducedRow['material'] != '' and reducedRow['furnitureType'] != '':
            quantMat = quantizeMaterial(reducedRow['material'])
            if quantMat:
                reducedRow['material'] = quantMat
                writer.writerow(reducedRow)
    inf.close()
    outf.close()
    
    

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print 'usage: processFurnitureData csvfilename'
        sys.exit(1)
    separateCollectionsAndItems(sys.argv[1])
    generateLivingRoomDataSet()