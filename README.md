# icml-recommender

This is the CTR code from https://github.com/blei-lab/ctr. 
To be extended to take as input user_mult.dat and user_final.gamma.

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
ctr/ctr --directory output/ --user data/users.dat --item data/items.dat  --mult data/mult.dat --theta_init lda_output/final.gamma  --beta_init lda_output/final.beta 


