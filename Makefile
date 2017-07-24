export LABEL := icml2017
export DATA_PATH := data/$(LABEL)
export PDF_PATH := $(DATA_PATH)/papers
export TXT_PATH := $(DATA_PATH)/papers_txt
export LDA_OUTPUT_PATH := $(DATA_PATH)/lda_output
export CTR_OUTPUT_PATH := output/$(LABEL)
ifeq ($(LABEL), icml2016)
	export PDF_CONVERSION_PATH := icml-pdf-conversion/pdfconversion.py
  export N_TOPICS := 50
  export LDA_ALPHA := 0.005
  export LDA_SETTINGS_PATH := lda-c/settings.txt
  export ALPHA_U_SMOOTH := 1
  export LAMBDA_U := 0.01
  export LAMBDA_V := 0.01
else ifeq ($(LABEL), icml2017)
	export PDF_CONVERSION_PATH := icml-pdf-conversion/pdfconversion_stopwords_2grams.py
	export PDF_CONVERSION_SUFFIX := _stop
  export N_TOPICS := 15
  export LDA_ALPHA := 0.5
  export LDA_SETTINGS_PATH := lda_settings_fixed.txt
  export ALPHA_U_SMOOTH := 1
  export LAMBDA_U := 0.01
  export LAMBDA_V := 0.01
endif

# disable implicit suffix rules
.SUFFIXES:

all: clean_db write_db

# install requirements
su_require: su_ctr_require su_py_require su_r_require

require: r_require

su_ctr_require:
	apt install libgsl-dev

su_py_require:
	apt install python-pip
	pip install --upgrade pip
	pip install -r icml-pdf-conversion/requirements.txt
	
su_r_require: 
	apt install r-base libssl-dev libcurl4-openssl-dev libxml2-dev
	
r_require: 
	./requirements.r
	
# make lda-c
lda-c/lda: 
	cd lda-c; $(MAKE); cd ..
	
lda-c: lda-c/lda
	
# make ctr2c
ctr2/ctr:
	cd ctr2; $(MAKE); cd ..
	
ctr2: ctr2/ctr

# configuration file
config.yml: config_$(LABEL).yml.in
	envsubst < config_$(LABEL).yml.in > config.yml
	
# scrape data
$(DATA_PATH)/papers.json $(DATA_PATH)/authors.json $(DATA_PATH)/schedule.json: config.yml
	./scrape_$(LABEL).r
	
scrape: $(DATA_PATH)/papers.json $(DATA_PATH)/authors.json $(DATA_PATH)/schedule.json
	
# convert papers pdf to txt
pdf2txt: 
	python $(PDF_CONVERSION_PATH) -p '$(PDF_PATH)/*.pdf' -t $(TXT_PATH) -m pdf2txt
	
clean_txt:
	rm -rf $(TXT_PATH)
	
# convert txt files to dat
$(TXT_PATH)/mult$(PDF_CONVERSION_SUFFIX).dat $(TXT_PATH)/files$(PDF_CONVERSION_SUFFIX).dat $(TXT_PATH)/vocab$(PDF_CONVERSION_SUFFIX).dat:
	python $(PDF_CONVERSION_PATH) -t $(TXT_PATH) -m txt2dat
	
txt2dat: $(TXT_PATH)/mult$(PDF_CONVERSION_SUFFIX).dat $(TXT_PATH)/files$(PDF_CONVERSION_SUFFIX).dat $(TXT_PATH)/vocab$(PDF_CONVERSION_SUFFIX).dat

clean_dat:
	rm -f $(TXT_PATH)/mult$(PDF_CONVERSION_SUFFIX).dat $(TXT_PATH)/files$(PDF_CONVERSION_SUFFIX).dat $(TXT_PATH)/vocab$(PDF_CONVERSION_SUFFIX).dat

# compute lda word distributions and documents topic distributions
$(LDA_OUTPUT_PATH)/final.gamma $(LDA_OUTPUT_PATH)/final.beta: $(TXT_PATH)/mult$(PDF_CONVERSION_SUFFIX).dat
	lda-c/lda est $(LDA_ALPHA) $(N_TOPICS) $(LDA_SETTINGS_PATH) $(TXT_PATH)/mult$(PDF_CONVERSION_SUFFIX).dat random $(LDA_OUTPUT_PATH)
	
run_lda: $(LDA_OUTPUT_PATH)/final.gamma $(LDA_OUTPUT_PATH)/final.beta

clean_run_lda:
	rm -rf $(LDA_OUTPUT_PATH)

# make topics json
$(DATA_PATH)/topics.json $(DATA_PATH)/topic_clusters.json $(DATA_PATH)/papers_topics.json: config.yml $(TXT_PATH)/files$(PDF_CONVERSION_SUFFIX).dat $(TXT_PATH)/vocab$(PDF_CONVERSION_SUFFIX).dat $(LDA_OUTPUT_PATH)/final.gamma $(LDA_OUTPUT_PATH)/final.beta $(DATA_PATH)/papers.json 
	./topics.r

topics: $(DATA_PATH)/topics.json $(DATA_PATH)/topic_clusters.json $(DATA_PATH)/papers_topics.json

clean_topics: 
	rm -f $(DATA_PATH)/topics.json $(DATA_PATH)/topic_clusters.json $(DATA_PATH)/papers_topics.json
   
# initialize couchdb
init_db: config.yml $(DATA_PATH)/papers_topics.json $(DATA_PATH)/authors.json $(DATA_PATH)/schedule.json $(DATA_PATH)/topics.json $(DATA_PATH)/topic_clusters.json
	./init_couchdb.r
	
# read user likes and topic preferences from couchdb	
$(DATA_PATH)/userids.dat $(DATA_PATH)/users.dat $(DATA_PATH)/items.dat $(DATA_PATH)/theta_u.dat: $(TXT_PATH)/files$(PDF_CONVERSION_SUFFIX).dat
	./read_couchdb.r
	
read_db: $(DATA_PATH)/users.dat $(DATA_PATH)/items.dat $(DATA_PATH)/theta_u.dat

clean_db: 
	rm -f $(DATA_PATH)/users.dat $(DATA_PATH)/items.dat $(DATA_PATH)/theta_u.dat
		
# compute ctr latent features
$(CTR_OUTPUT_PATH)/final-U.dat $(CTR_OUTPUT_PATH)/final-V.dat: $(DATA_PATH)/users.dat $(DATA_PATH)/items.dat $(DATA_PATH)/theta_u.dat $(LDA_OUTPUT_PATH)/final.gamma
	mkdir -p $(CTR_OUTPUT_PATH)
	ctr2/ctr --directory $(CTR_OUTPUT_PATH) --user $(DATA_PATH)/users.dat --item $(DATA_PATH)/items.dat --theta_v_init $(LDA_OUTPUT_PATH)/final.gamma --theta_u_init $(DATA_PATH)/theta_u.dat --num_factors $(N_TOPICS) --alpha_u_smooth $(ALPHA_U_SMOOTH) --lambda_u $(LAMBDA_U) --lambda_v $(LAMBDA_V)
	
run_ctr: $(CTR_OUTPUT_PATH)/final-U.dat $(CTR_OUTPUT_PATH)/final-V.dat

clean_run_ctr:
	rm -rf $(CTR_OUTPUT_PATH)

# write recommendations to couchdb
write_db: config.yml $(DATA_PATH)/userids.dat $(TXT_PATH)/files$(PDF_CONVERSION_SUFFIX).dat $(CTR_OUTPUT_PATH)/final-U.dat $(CTR_OUTPUT_PATH)/final-V.dat
	./write_couchdb.r

# clean
clean: clean_scrape clean_txt clean_dat clean_run_lda clean_topics clean_db clean_run_ctr

.PHONY: clean
