# Exploring individual variation in Turkish heritage speakers’ complex linguistic productions

This repository contains all the code and data necessary to replicate the findings from our study.

## Models

The models were to big in size in order to be included in this GitHub Release. You can download them from OSF: [https://www.doi.org/10.17605/OSF.IO/6ZCXU](https://www.doi.org/10.17605/OSF.IO/6ZCXU)

## Short guide to replication

You can either access the data from the folder `scripts/data/`, or download them as explained in the next section. In the next step, open the folder `scripts/` and proceed to run the scripts in the indicated order.

## How to download the original data

In a first step, please open the following link:

[https://korpling.german.hu-berlin.de/annis3/#c=rueg](https://korpling.german.hu-berlin.de/annis3/#c=rueg)

Then, select the RUEG-TR_1.0-SNAPSHOT corpus.

In the search form, you can type the following:

```
norm _o_ pos_lang _o_ cu
```

And then export the data with the following additional settings:

```
Annotation Keys: norm
Parameters: metakeys=doc
```

You should now have a CSV-file with all the data we have used in our study.
