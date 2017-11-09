# icml-recommender

# Install

Requirements:

- git
- g++
- libgsl-dev libssl-dev libcurl4-openssl-dev libxml2-dev
- python + pip + some modules
- R + some packages

Install everything with

```sh
git clone --recursive git@github.com:ICMLRecommender/icml-recommender.git
cd icml-recommender
sudo make su_require
make require
```

# Configuration

Create a `.env` file with the following content and edit the `COUCHDB_USER` and `COUCHDB_PWD` variables.

```sh
DATA_ROOT=data
DATA_DIR=nips2017

#PDF_DIR=papers
#PDF_TXT_DIR=papers_txt
ABSTRACTS_TXT_DIR=abstracts_txt
TXT_DIR=abstracts_txt
#PDF_CONVERSION_PATH=icml-pdf-conversion/pdfconversion.py
#MULT_FILE=mult.dat
#FILES_FILE=files.dat
#VOCAB_FILE=vocab.dat
PDF_CONVERSION_PATH=icml-pdf-conversion/pdfconversion_stopwords_2grams.py
MULT_FILE=mult_stop.dat
FILES_FILE=files_stop.dat
VOCAB_FILE=vocab_stop.dat

# Uncomment to enable download of the data from url instead of scraping
#DATA_ZIP_URL=https://www.dropbox.com/sh/l98rpjwrr4giove/AACRIDSlVJs1mFdDyOr5cVIda?dl=1

SCRAPE_R_FILE=scrape_nips2017.r
PROCESS_R_FILE=process_nips2017.r
SCRAPE_DL_PDF=0
SCRAPE_DL_SUPP_PDF=0

SESSION_LABELS_CSV_URL=https://docs.google.com/spreadsheets/d/1vwiSxPpaVcGfl-MtwUJEZm4nPkKp3IN7aHzSHkrIjFk/export?format=csv&id=1vwiSxPpaVcGfl-MtwUJEZm4nPkKp3IN7aHzSHkrIjFk&gid=7384290

LDA_SETTINGS_PATH=lda_settings_fixed.txt
LDA_OUTPUT_DIR=lda_output
LDA_N_TOPICS=30
LDA_ALPHA=1

TOPICS_THRES_TOPIC=0.05
TOPICS_THRES_WORD=3e-3
TOPICS_LABELS_CSV_URL=https://docs.google.com/spreadsheets/d/11EarYD7_y-j8wEl5oeLXHzPaADjq0v31IbOU5wY8UaI/export?format=csv&id=11EarYD7_y-j8wEl5oeLXHzPaADjq0v31IbOU5wY8UaI&gid=0

CTR_OUTPUT_DIR=ctr_output
CTR_A=1 
CTR_B=0.01
CTR_ALPHA_U_SMOOTH=0.01
CTR_ALPHA_V_SMOOTH=0
CTR_LAMBDA_U=0.01
CTR_LAMBDA_V=0.01
CTR_MAX_ITER=1000

COUCHDB_HOST=icml.papro.org.uk
COUCHDB_PATH=couchdb
COUCHDB_PORT=NULL
COUCHDB_TRANSPORT=https
COUCHDB_USER=<USER>
COUCHDB_PWD=<PWD>
COUCHDB_REVS_LIMIT=10
  
# Uncomment for simulating random likes
#SIMU_SEED=2017
#SIMU_N_LIKE=100

RECO_N_TOP=20

TRENDING_DBNAME=schedule
TRENDING_DOCID=trending_2017
TRENDING_FIELD=trendingids
TRENDING_BOOKMARK_WEIGHT=1
TRENDING_LIKE_WEIGHT=2
TRENDING_N_TOP=7
```

# Usage with nips2017

1. scrape nips2017

    ```sh
    make scrape
    ```

Alternatively, download the data by setting the `DATA_ZIP_URL` environment variable and running

    ```sh
    make dl_data_zip
    ```
    
2. process the data

    ```sh
    make process
    ```

3. convert abstracts to mult.dat
    
    ```sh
    make abstracts_txt
    make txt2dat
    ```
    
4. compile lda
    
    ```sh
    make lda-c
    ```
    
5. run lda

    ```sh
    make run_lda
    ```

-------------------------------------------------------------------

**WIP: DO NOT RUN BELOW**

6. make topics data

    ```sh
    make topics
    ```

7. init couchdb

    ```sh
    make init_couchdb
    ```

8. compile ctr2
    
    ```sh
    make ctr2
    ```

9. update recommendations: read likes from couchdb, run ctr2 and write recommendations to couchdb

    ```sh
    make
    ```

### Automate recommendations updates every 2 minutes

Edit your crontab with `crontab -e` and add the following line:

```
*/2 * * * * cd icml-recommender; make > cron.log; cd -
```
