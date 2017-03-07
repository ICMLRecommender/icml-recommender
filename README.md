# icml-recommender

This is the CTR code from (https://github.com/blei-lab/ctr). 
To be extended to take as input mult_user.dat and final_user.gamma.

Input:
data/users.dat, data/items.dat, data/mult.dat 
lda-output/final.gamma, lda-output/final.beta 

Output:
output/final-U.dat, output/final-V.dat 


# Usage

ctr/ctr --directory output/ --user data/users.dat --item data/items.dat  --mult data/mult.dat --theta_init lda-output/final.gamma  --beta_init lda-output/final.beta 
