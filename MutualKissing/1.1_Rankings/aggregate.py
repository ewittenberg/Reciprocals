import csv

with open("MutualKissingRankings.csv", "r") as rankings_file, open("MutualKissingRankingsAggregate.csv", "w") as out_file:
    rankings = csv.DictReader(rankings_file)
   
    rankings_list = list(rankings)
    
    transitive_counts = dict(zip(rankings.fieldnames, [0 for x in rankings.fieldnames]))
    transitive_counts["total"] = 0

    for row in rankings_list:
        for key in rankings.fieldnames:
            if row[key] == "TRANSITIVE":
                transitive_counts[key] += 1
                transitive_counts["total"] += 1
   
    to_del = []
    for key in rankings.fieldnames:
        if rankings_list[0][key] == "Filler":
            to_del.append(key)

    for i in to_del:
        del transitive_counts[i]
    
    percents = {}

    for key in transitive_counts.keys():
        percents[key] = transitive_counts[key]/15.0

    percents["total"] = transitive_counts["total"]/(15.0 * len(transitive_counts.keys()))

    out_list = [transitive_counts, percents]

    fieldnames = [x for x in percents.keys() if x != "total"] + ["total"]
    out_writer = csv.DictWriter(out_file, fieldnames=fieldnames)
    out_writer.writeheader()
    out_writer.writerows(out_list)

