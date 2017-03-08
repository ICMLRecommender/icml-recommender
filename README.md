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

Get the data:

wget http://www.cs.cmu.edu/%7Echongw/data/citeulike/items.dat -P /data/   

wget http://www.cs.cmu.edu/%7Echongw/data/citeulike/users.dat -P /data/   

wget http://www.cs.cmu.edu/%7Echongw/data/citeulike/mult.dat -P /data/   


Then run ctr:

ctr/ctr --directory output/ --user data/users.dat --item data/items.dat  --mult data/mult.dat --theta_init lda-output/final.gamma  --beta_init lda-output/final.beta 


