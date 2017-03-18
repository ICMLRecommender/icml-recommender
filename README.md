# icml-recommender

This is the CTR code from https://github.com/blei-lab/ctr. 
To be extended to take as input `mult_user.dat` and `final_user.gamma`.

Input:

```
data/users.dat   
data/items.dat   
data/mult.dat    
lda_output/final.gamma    
lda_output/final.beta   
```

Output:

```
output/final-U.dat  
output/final-V.dat   
```

# Usage

1. Compile ctr2 using the make file in the ctr2 directory:     
    
    ```sh
    make  
    ```

2. Run ctr2:     
    
    ```sh
    ctr2/ctr --directory output/ --user data/users_like.dat --item data/items_like.dat  --mult_v data/mult_v_like.dat --mult_u data/mult_u_library.dat --theta_v_init lda_output/final_like.gamma_v --theta_u_init lda_output/final_library.gamma_u --beta_init lda_output/final.beta   
    ```

3. Given final-U.dat, final-V.dat, save in output/ the top 10 recommendations for each user:    
    
    ```sh
    ./rating.R  output/  200 10
    ```