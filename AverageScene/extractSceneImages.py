import sys
import os
import csv
import shutil

SOURCES = { 'ashley' : lambda row: row['source'] == 'ashleyfurniture' and row['itemType'] == 'SET',
            'furniture.com' : lambda row: row['source'] == 'furniturecom' and row['itemType'] == 'COLLECTION'
        }
def ensureDirExists(dirname):
    if not os.path.isdir(dirname):
        os.mkdir(dirname)

def extractSceneImages(inputDir, outputDir):
    
    # Ensure top-level directory exists
    ensureDirExists(outputDir)
    
    # Get all rows from each source
    f = open('{}/furniture.csv'.format(inputDir), 'r')
    csvreader = csv.DictReader(f)
    rows = []
    for row in csvreader:
        rows.append(row)
    sourceRows = {source: [row for row in rows if pred(row)] for (source, pred) in SOURCES.iteritems()}
    f.close()
    
    for source in SOURCES:
        
        myrows = sourceRows[source]
        
        # Ensure source output subdirectories exists
        sourceOutDir = '{}/{}'.format(outputDir, source)
        ensureDirExists(sourceOutDir)
        
        indir = '{}/{}/images'.format(inputDir, source)
        
        for item in myrows:
            
            # Ensure the directory for this scene type exists
            sceneOutDir = '{}/{}'.format(sourceOutDir, item['roomType'])
            ensureDirExists(sceneOutDir)
            
            # Grab the image associated with the id, copy it over
            # Also grab any _big image, if it exists
            imgname = '{}.jpg'.format(item['itemId'])
            bigImgname = '{}_big.jpg'.format(item['itemId'])
            inPath = '{}/{}'.format(indir, imgname)
            bigInPath = '{}/{}'.format(indir, bigImgname)
            if os.path.exists(inPath):
                shutil.copyfile(inPath, '{}/{}'.format(sceneOutDir, imgname))
            if os.path.exists(bigInPath):
                shutil.copyfile(inPath, '{}/{}'.format(sceneOutDir, bigImgname))

if __name__ == '__main__':
    if len(sys.argv) != 3:
        print 'usage: extractSceneImages furnitureCsvFile outputDirectory'
        sys.exit(1)
    extractSceneImages(sys.argv[1], sys.argv[2])