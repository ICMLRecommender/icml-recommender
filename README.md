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
sudo make require
```

# Usage with icml2016

1. scrape icml2016

    ```sh
    ./scrape_icml.r
    ```

2. convert pdf to mult.dat
    
    ```sh
    cd icml-pdf-conversion
    sudo pip install -r requirements.txt
    python pdfconversion.py -p '../data/icml2016/papers/*.pdf' -t ../data/icml2016/papers_txt/ -m full
    cd ..
    ```

3. compile lda
    
    ```sh
    cd lda-c
    make  
    cd ..
    ```
    
4. run lda

    ```sh
    lda-c/lda  est 0.005 50 lda-c/settings.txt   data/icml2016/mult.dat   random  data/icml2016/lda_output/
    ./topics.r
    ```

5. init couchdb

    ```sh
    ./init_couchdb.r
    ```

6. compile ctr2
    
    ```sh
    cd ctr2
    make  
    cd ..
    ```

7. run ctr2 (every 5 min)

    ```sh
    ./read_couchdb.r
    
    ctr2/ctr --directory output/icml2016 --user data/icml2016/users.dat --item data/icml2016/items.dat --theta_v_init data/icml2016/lda_output/final.gamma --theta_u_init data/icml2016/theta_u.dat --num_factors 50
    
    ./write_couchdb.r
    ```

# Usage (deprecated)

1. Compile ctr2 using the make file in the ctr2 directory:     
    
    ```sh
    make  
    ```

2. Run ctr2:     
    
    ```sh
    ctr2/ctr --directory output/ --user data/users_like.dat --item data/items_like.dat  --mult_v data/mult_v_like.dat --mult_u data/mult_u_library.dat --theta_v_init lda_output/final_like.gamma_v --theta_u_init lda_output/final_library.gamma_u --beta_init lda_output/final.beta   
    ```

3. Given final-U.dat, final-V.dat, user.dat, saves in output/ up to (e.g.) 20 recommendations for each user. This example is for K = 200.    
    
    ```sh
    ./rating.R  output/  200 20
    ```
