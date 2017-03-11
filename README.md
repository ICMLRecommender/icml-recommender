# icml-recommender

This is the CTR code from https://github.com/blei-lab/ctr. 
To be extended to take as input mult_library.dat and final_library.gamma.

Input:  
data/users.dat   
data/items.dat   
data/mult.dat    
lda-output/final.gamma    
lda_output/final.beta   

Output:  
output/final-U.dat  
output/final-V.dat   


# Usage

1. Compile ctr using the make file in the ctr directory:     
make  

2. Run ctr:     
ctr2/ctr --directory output/ --user data/users_like.dat --item data/items_like.dat  --mult_v data/mult_like.dat --mult_u data/mult_u_library.dat --theta_v_init lda_output/final_like.gamma --theta_u_init lda_output/final_library.gamma_u --beta_init lda_output/final.beta   

3. Given final-U.dat, final-V.dat, save in output/ the top 10 recommendations for each user:    
Rscript rating.R  output/  200 10
