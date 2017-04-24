export DATA_PATH = data/icml2016
export PDF_PATH = data/icml2016/papers
export TXT_PATH = data/icml2016/papers_txt
export LDA_OUTPUT_PATH = data/icml2016/lda_output
export CTR_OUTPUT_PATH = output/icml2016
export N_TOPICS = 50
export LDA_ALPHA = 0.005
export ALPHA_U_SMOOTH = 1
export LAMBDA_U = 0.01
export LAMBDA_V = 0.01

all: clean_db write_db

require: ctr_require py_require r_require

ctr_require:
	apt install libgsl-dev

py_require:
	apt install python-pip
	pip install --upgrade pip
	pip install -r icml-pdf-conversion/requirements.txt
	
r_require: 
	apt install r-base libssl-dev libcurl4-openssl-dev libxml2-dev
	Rscript requirements.r
	
lda-c/lda: lda-c/lda
	cd lda-c; make; cd ..
	
lda-c: lda-c/lda
	
ctr2/ctr:
	cd ctr2; make; cd ..
	
ctr2: ctr2/ctr

config.yml: config.yml.in
	envsubst < config.yml.in > config.yml
		
$(DATA_PATH)/papers.json $(DATA_PATH)/authors.json $(DATA_PATH)/sessions.json: scrape_icml.r config.yml
	./scrape_icml.r
	
scrape: $(DATA_PATH)/papers.json $(DATA_PATH)/authors.json $(DATA_PATH)/sessions.json

clean_scrape:
	rm -f $(DATA_PATH)/papers.json $(DATA_PATH)/authors.json $(DATA_PATH)/sessions.json
		
$(TXT_PATH)/mult.dat $(TXT_PATH)/files.dat $(TXT_PATH)/vocab.dat: scrape icml-pdf-conversion/pdfconversion.py
	python icml-pdf-conversion/pdfconversion.py -p '$(PDF_PATH)/*.pdf' -t $(TXT_PATH) -m full
	
pdfconversion: $(TXT_PATH)/mult.dat $(TXT_PATH)/files.dat $(TXT_PATH)/vocab.dat

clean_pdfconversion:
	rm -rf $(TXT_PATH)

$(LDA_OUTPUT_PATH)/final.gamma $(LDA_OUTPUT_PATH)/final.beta: lda-c $(TXT_PATH)/mult.dat
	lda-c/lda est $(LDA_ALPHA) $(N_TOPICS) lda-c/settings.txt $(TXT_PATH)/mult.dat random $(LDA_OUTPUT_PATH)
	
run_lda: $(LDA_OUTPUT_PATH)/final.gamma $(LDA_OUTPUT_PATH)/final.beta

clean_run_lda:
	rm -rf $(LDA_OUTPUT_PATH)

$(DATA_PATH)/topics.json $(DATA_PATH)/papers_topics.json: topics.r config.yml $(TXT_PATH)/files.dat $(TXT_PATH)/vocab.dat run_lda $(DATA_PATH)/papers.json 
	./topics.r

topics: $(DATA_PATH)/topics.json $(DATA_PATH)/papers_topics.json

clean_topics: 
	rm -f $(DATA_PATH)/topics.json $(DATA_PATH)/papers_topics.json
    
init_db: init_couchdb.r config.yml $(DATA_PATH)/papers_topics.json $(DATA_PATH)/authors.json $(DATA_PATH)/sessions.json $(DATA_PATH)/topics.json 
	./init_couchdb.r
		
$(DATA_PATH)/userids.dat $(DATA_PATH)/users.dat $(DATA_PATH)/items.dat $(DATA_PATH)/theta_u.dat: read_couchdb.r config.yml $(TXT_PATH)/files.dat
	./read_couchdb.r
	
read_db: $(DATA_PATH)/users.dat $(DATA_PATH)/items.dat $(DATA_PATH)/theta_u.dat

clean_db: 
	rm -f $(DATA_PATH)/users.dat $(DATA_PATH)/items.dat $(DATA_PATH)/theta_u.dat
		
$(CTR_OUTPUT_PATH)/final-U.dat $(CTR_OUTPUT_PATH)/final-V.dat: ctr2 read_db $(LDA_OUTPUT_PATH)/final.gamma
	ctr2/ctr --directory $(CTR_OUTPUT_PATH) --user $(DATA_PATH)/users.dat --item $(DATA_PATH)/items.dat --theta_v_init $(LDA_OUTPUT_PATH)/final.gamma --theta_u_init $(DATA_PATH)/theta_u.dat --num_factors $(N_TOPICS) --ALPHA_U_SMOOTH $(ALPHA_U_SMOOTH) --LAMBDA_U $(LAMBDA_U) --LAMBDA_V $(LAMBDA_V)
	
run_ctr: $(CTR_OUTPUT_PATH)/final-U.dat $(CTR_OUTPUT_PATH)/final-V.dat

clean_run_ctr:
	rm -rf $(CTR_OUTPUT_PATH)

write_db: write_couchdb.r config.yml $(DATA_PATH)/userids.dat $(TXT_PATH)/files.dat run_ctr
	./write_couchdb.r

clean: clean_scrape clean_pdfconversion clean_run_lda clean_topics clean_db clean_run_ctr

.PHONY: require ctr_require py_require r_require lda-c ctr2 scrape clean_scrape pdfconversion clean_pdfconversion run_lda clean_run_lda topics clean_topics init_db read_db clean_db run_ctr clean_run_ctr write_db clean

