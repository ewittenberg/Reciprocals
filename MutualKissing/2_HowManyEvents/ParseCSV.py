import csv

# CHANGE FOR NEW BATCH FILES
NUM_QUESTIONS = 47

# Convert a string to an float
def num(x): 
    try: 
        return float(x)
    except: 
        return 0

def findRow(sentData, sent):
    print(sent)
    print()
    for row in sentData:
        print(row['Sentence'])
        if (row['Sentence'].strip() == sent.strip()):
            print("--- Match")
            return row
    return None


outputData = []
with open('Datafiles/HowManyEvents_results.csv') as dataFile,  \
     open('Datafiles/HowManyEvents_stims.csv') as sentFile:
    # Read in the csv files as lists of dictionaries
    inputData = list(csv.DictReader(dataFile))
    sentData  = list(csv.DictReader(sentFile))
    
    # For each response from each user
    for row in inputData:
        for question in range(1,NUM_QUESTIONS + 1):
            # Data for this row to be filled
            rowData = {}
            
            # Turn the question number into a string for concatenation into keys
            i = str(question)
            
            # Grab the information about the sentence corresponding to this
            # response
            # Split to remove html formatting and make the 2 sets of strings 
            # match
            sentRow = findRow(sentData, row['Input.sent'+i]) 
            
            # Fill in the fields 
            rowData['workerId'] = row['WorkerId']
            
            rowData['ID'] = sentRow['ID']
            rowData['pairID'] = sentRow['PairID']
            
            rowData['count'] =  num(row['Answer.count' + i])  

            rowData['event'] = sentRow['Event']
            rowData['eventCat'] = sentRow['EventType']
            rowData['construction'] = sentRow['Construction']
            
            # Add the data to the output data array
            outputData.append(rowData);

# Write the data to file
with open('FormattedHowMany.csv', 'w') as outputFile:
    fieldnames = ['workerId', 'ID', 'pairID', 'count', 'event', 
                  'eventCat', 'construction', "sentence"]
    outWriter = csv.DictWriter(outputFile, fieldnames=fieldnames)
    outWriter.writeheader();
    outWriter.writerows(outputData)
