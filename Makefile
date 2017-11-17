#!make
SHELL:=/bin/bash
include .env
export $(shell sed 's/[=\#].*//' .env)

HOSTNAME:=$(shell hostname)
DATA_PATH:=$(DATA_ROOT)/$(DATA_DIR)
RAW_PATH:=$(DATA_PATH)/raw
PDF_PATH:=$(DATA_PATH)/$(PDF_DIR)
PDF_TXT_PATH:=$(DATA_PATH)/$(PDF_TXT_DIR)
ABSTRACTS_TXT_PATH:=$(DATA_PATH)/$(ABSTRACTS_TXT_DIR)
TXT_PATH:=$(DATA_PATH)/$(TXT_DIR)
LDA_OUTPUT_PATH:=$(DATA_PATH)/$(LDA_OUTPUT_DIR)
CTR_OUTPUT_PATH:=$(DATA_PATH)/$(CTR_OUTPUT_DIR)

# .env (environment variables)
.env: 
	if [ -f $(HOSTNAME).env ]; then cp -f $(HOSTNAME).env .env; fi
	
info:
	@echo HOSTNAME=$(HOSTNAME)
	@echo DATA_PATH=$(DATA_PATH)
	@echo PDF_PATH=$(PDF_PATH)
	@echo PDF_TXT_PATH=$(PDF_TXT_PATH)
	@echo ABSTRACTS_TXT_PATH=$(ABSTRACTS_TXT_PATH)
	@echo TXT_PATH=$(TXT_PATH)
	@echo LDA_OUTPUT_PATH=$(LDA_OUTPUT_PATH)
	@echo CTR_OUTPUT_PATH=$(CTR_OUTPUT_PATH)

# disable implicit suffix rules
.SUFFIXES:

# default make target
all: clean_db write_db

# install requirements
su_require: su_ctr_require su_py_require su_r_require

require: r_require

su_ctr_require:
	apt-get install libgsl-dev 

su_py_require:
	apt-get install python-pip
	pip install --upgrade pip
	pip install -r icml-pdf-conversion/requirements.txt
	
su_r_require: 
	apt-get install r-base libssl-dev libcurl4-openssl-dev libxml2-dev libv8-3.14-dev libudunits2-dev 

	
r_require: 
	./requirements.r
	
# make lda-c
lda-c/lda: 
	cd lda-c; $(MAKE); cd ..
	
lda-c: lda-c/lda
	
# make ctr2
ctr2/ctr:
	cd ctr2; $(MAKE); cd ..
	
ctr2: ctr2/ctr

# scrape data
SCRAPE_FILES = $(RAW_PATH)/schedule.json $(RAW_PATH)/events.json $(RAW_PATH)/authors.json

$(SCRAPE_FILES):
	./$(SCRAPE_R_FILE)
	
scrape: $(SCRAPE_FILES)

clean_scrape:
	rm -rf $(SCRAPE_FILES)
	
# process data
PROCESS_FILES = $(DATA_PATH)/papers.json $(DATA_PATH)/authors.json $(DATA_PATH)/schedule.json

$(PROCESS_FILES):
	./$(PROCESS_R_FILE)
	
process: $(PROCESS_FILES)

clean_process:
	rm -rf $(PROCESS_FILES)
		
# convert papers pdf to txt
pdf2txt: 
	python $(PDF_CONVERSION_PATH) -p '$(PDF_PATH)/*.pdf' -t $(PDF_TXT_PATH) -m pdf2txt
	
clean_pdf2txt:
	rm -rf $(PDF_TXT_PATH)/*.txt
	
# write abstracts txt
abstracts_txt: $(DATA_PATH)/papers.json
	./abstracts_txt.r
	
clean_abstracts_txt:
	rm -rf $(ABSTRACTS_TXT_PATH)/*.txt
	
# convert txt files to dat
TXT2DAT_FILES = $(TXT_PATH)/$(MULT_FILE) $(TXT_PATH)/$(FILES_FILE) $(TXT_PATH)/$(VOCAB_FILE)

$(TXT2DAT_FILES):
	python $(PDF_CONVERSION_PATH) -t $(TXT_PATH) -m txt2dat
	
txt2dat: $(TXT2DAT_FILES)

clean_txt2dat:
	rm -f $(TXT2DAT_FILES)

# compute lda word distributions and documents topic distributions
LDA_FILES = $(LDA_OUTPUT_PATH)/final.gamma $(LDA_OUTPUT_PATH)/final.beta

$(LDA_FILES): $(TXT_PATH)/$(MULT_FILE)
	lda-c/lda est $(LDA_ALPHA) $(LDA_N_TOPICS) $(LDA_SETTINGS_PATH) $(TXT_PATH)/$(MULT_FILE) random $(LDA_OUTPUT_PATH)
	
run_lda: $(LDA_FILES)

clean_run_lda:
	rm -rf $(LDA_OUTPUT_PATH)

# download data
dl_data_zip: 
	mkdir -p $(DATA_PATH)
	cd $(DATA_PATH)
	wget $(DATA_ZIP_URL) -O .data.zip
	unzip .data.zip
	rm -f .data.zip
	cd -
	
# make topics json
TOPICS_FILES = $(DATA_PATH)/topics.json $(DATA_PATH)/topic_clusters.json $(DATA_PATH)/papers_topics.json
$(TOPICS_FILES): $(TXT_PATH)/$(FILES_FILE) $(TXT_PATH)/$(VOCAB_FILE) $(LDA_OUTPUT_PATH)/final.gamma $(LDA_OUTPUT_PATH)/final.beta $(DATA_PATH)/papers.json 
	./topics.r

topics: $(TOPICS_FILES)

clean_topics: 
	rm -f $(TOPICS_FILES)
   
# initialize couchdb
init_db: $(DATA_PATH)/papers_topics.json $(DATA_PATH)/authors.json $(DATA_PATH)/schedule.json $(DATA_PATH)/topics.json $(DATA_PATH)/topic_clusters.json
	./init_couchdb.r
	
# read user likes and topic preferences from couchdb	
READ_DB_FILES = $(DATA_PATH)/userids.dat $(DATA_PATH)/users.dat $(DATA_PATH)/items.dat $(DATA_PATH)/theta_u.dat

$(READ_DB_FILES): $(TXT_PATH)/$(FILES_FILE)
	./read_couchdb.r
	
read_db: $(READ_DB_FILES)

clean_db: 
	rm -f $(READ_DB_FILES)
		
# compute ctr latent features
CTR_FILES = $(CTR_OUTPUT_PATH)/final-U.dat $(CTR_OUTPUT_PATH)/final-V.dat

$(CTR_FILES): $(DATA_PATH)/users.dat $(DATA_PATH)/items.dat $(DATA_PATH)/theta_u.dat $(LDA_OUTPUT_PATH)/final.gamma
	mkdir -p $(CTR_OUTPUT_PATH)
	ctr2/ctr --directory $(CTR_OUTPUT_PATH) --user $(DATA_PATH)/users.dat --item $(DATA_PATH)/items.dat --theta_v_init $(LDA_OUTPUT_PATH)/final.gamma --theta_u_init $(DATA_PATH)/theta_u.dat --num_factors $(LDA_N_TOPICS) --a ${CTR_A} --b ${CTR_B} --alpha_u_smooth $(CTR_ALPHA_U_SMOOTH) --alpha_v_smooth $(CTR_ALPHA_V_SMOOTH) --lambda_u $(CTR_LAMBDA_U) --lambda_v $(CTR_LAMBDA_V) --max_iter ${CTR_MAX_ITER}
	
run_ctr: $(CTR_FILES)

clean_run_ctr:
	rm -rf $(CTR_OUTPUT_PATH)

# write recommendations to couchdb
write_db: $(DATA_PATH)/userids.dat $(TXT_PATH)/$(TXT_PATH)/$(FILES_FILE) $(CTR_OUTPUT_PATH)/final-U.dat $(CTR_OUTPUT_PATH)/final-V.dat
	./write_couchdb.r

# clean
CLEAN_TARGETS = clean_scrape clean_pdf2txt clean_abstracts_txt clean_txt2dat clean_run_lda clean_topics clean_db clean_run_ctr

clean: $(CLEAN_TARGETS)

.PHONY: .env $(CLEAN_TARGETS) clean
