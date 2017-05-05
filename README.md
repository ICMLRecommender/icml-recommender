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

Edit the following variables in the `Makefile`.

```make
export DATA_PATH = data/icml2016
export PDF_PATH = $(DATA_PATH)/papers
export TXT_PATH = $(DATA_PATH)/papers_txt
export LDA_OUTPUT_PATH = $(DATA_PATH)/lda_output
export CTR_OUTPUT_PATH = output/icml2016
export N_TOPICS = 50
export LDA_ALPHA = 0.005
export ALPHA_U_SMOOTH = 1
export LAMBDA_U = 0.01
export LAMBDA_V = 0.01
```

Create a `config.yml.in` file with the following content and edit the `couchdb:user` and `couchdb:pwd` fields.

```yaml
data: 
  path: "${DATA_PATH}"
  pdf_path: "${PDF_PATH}"
  txt_path: "${TXT_PATH}"

scrape:
  dl_pdf: TRUE
  dl_supp_pdf: FALSE
  dl_review: FALSE
  dl_rebuttal: FALSE

lda:
  alpha: ${LDA_ALPHA}
  n_topics: ${N_TOPICS}
  output_path: "${LDA_OUTPUT_PATH}"

couchdb:
  # transport: http
  host: localhost
  # port: NULL
  # path: NULL
  user: <COUCHDB_USER>
  pwd: <COUCHDB_PASSWORD>

ctr:
  output_path: "${CTR_OUTPUT_PATH}"
  lambda_u: ${LAMBDA_U}
  lambda_v: ${LAMBDA_V}
  a: 1
  b: 0.01
  alpha_u_smooth: ${ALPHA_U_SMOOTH}
  alpha_v_smooth: 0
  max_iter: 200

# # Uncomment for simulating random likes
# simu:
#   seed: 2017
#   n_likes: 100
  
reco:
  n_top: 20
```

# Usage with icml2016

1. scrape icml2016

    ```sh
    make scrape
    ```

2. convert pdf to mult.dat
    
    ```sh
    make pdf2txt
    make txt2dat
    ```

3. compile lda
    
    ```sh
    make lda-c
    ```
    
4. run lda

    ```sh
    make run_lda
    ```

5. init couchdb

    ```sh
    make init_couchdb
    ```

6. compile ctr2
    
    ```sh
    make ctr2
    ```

7. update recommendations: read likes from couchdb, run ctr2 and write recommendations to couchdb

    ```sh
    make
    ```

### Automate recommendations updates every 5 minutes

Edit your crontab with `crontab -e` and add the following line:

```
*/5 * * * * cd icml-recommender; make > cron.log; cd -
```
