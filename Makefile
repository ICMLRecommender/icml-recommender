data_path := data/icml2016
pdf_path := data/icml2016/papers
txt_path := data/icml2016/papers_txt
lda_output_path := data/icml2016/lda_output
ctr_output_path := output/icml2016
n_topics := 50
lda_alpha := 0.005
alpha_u_smooth := 1
lambda_u := 0.01
lambda_v := 0.01

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
		
$(data_path)/papers.json $(data_path)/authors.json $(data_path)/sessions.json: scrape_icml.r
	./scrape_icml.r
	
scrape: $(data_path)/papers.json $(data_path)/authors.json $(data_path)/sessions.json

clean_scrape:
	rm -f $(data_path)/papers.json $(data_path)/authors.json $(data_path)/sessions.json
		
$(txt_path)/mult.dat $(txt_path)/files.dat $(txt_path)/vocab.dat: scrape icml-pdf-conversion/pdfconversion.py
	python icml-pdf-conversion/pdfconversion.py -p '$(pdf_path)/*.pdf' -t $(txt_path) -m full
	
pdfconversion: $(txt_path)/mult.dat $(txt_path)/files.dat $(txt_path)/vocab.dat

clean_pdfconversion:
	rm -rf $(txt_path)

$(lda_output_path)/final.gamma $(lda_output_path)/final.beta: pdfconversion lda-c
	lda-c/lda est $(lda_alpha) $(n_topics) lda-c/settings.txt $(txt_path)/mult.dat random $(lda_output_path)
	
run_lda: $(lda_output_path)/final.gamma $(lda_output_path)/final.beta

clean_run_lda:
	rm -rf $(lda_output_path)

$(data_path)/topics.json $(data_path)/papers_topics.json: topics.r run_lda
	./topics.r

topics: $(data_path)/topics.json $(data_path)/papers_topics.json

clean_topics: 
	rm -f $(data_path)/topics.json $(data_path)/papers_topics.json
    
init_db: init_couchdb.r topics
	./init_couchdb.r
		
$(data_path)/users.dat $(data_path)/items.dat $(data_path)/theta_u.dat:
	./read_couchdb.r
	
read_db: $(data_path)/users.dat $(data_path)/items.dat $(data_path)/theta_u.dat

clean_db: 
	rm -f $(data_path)/users.dat $(data_path)/items.dat $(data_path)/theta_u.dat
		
$(ctr_output_path)/final-U.dat $(ctr_output_path)/final-V.dat: ctr2 read_db $(lda_output_path)/final.gamma
	ctr2/ctr --directory $(ctr_output_path) --user $(data_path)/users.dat --item $(data_path)/items.dat --theta_v_init $(lda_output_path)/final.gamma --theta_u_init $(data_path)/theta_u.dat --num_factors $(n_topics) --alpha_u_smooth $(alpha_u_smooth) --lambda_u $(lambda_u) --lambda_v $(lambda_v)
	
run_ctr: $(ctr_output_path)/final-U.dat $(ctr_output_path)/final-V.dat

clean_run_ctr:
	rm -rf $(ctr_output_path)

write_db: write_couchdb.r run_ctr
	./write_couchdb.r

clean: clean_scrape clean_pdfconversion clean_run_lda clean_topics clean_db clean_run_ctr

.PHONY: require ctr_require py_require r_require lda-c ctr2 scrape clean_scrape pdfconversion clean_pdfconversion run_lda clean_run_lda topics clean_topics init_db read_db clean_db run_ctr clean_run_ctr write_db clean

