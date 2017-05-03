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

7. read likes, run ctr2 and write recommendations (every 5 min)

    ```sh
    make
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
