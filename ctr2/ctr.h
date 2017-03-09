// class for ctr
//
#ifndef CTR_H
#define CTR_H

#include "utils.h"
#include "corpus.h"
#include "data.h"

struct ctr_hyperparameter {
  double a;
  double b;
  double lambda_u;
  double lambda_v;
  double learning_rate;
  double alpha_u_smooth;
  double alpha_v_smooth;
  int    random_seed;
  int    max_iter;
  int    save_lag;
  int    theta_u_opt;
  int    theta_v_opt;
  int    ctr_run;
  int    lda_regression;
  
  void set(double aa, double bb, 
           double lu, double lv, 
           double lr, double aus, double avs,
           int rs, int mi, int sl,    
		   int tuo, int tvo,
		   int cr, int lda_r) {
    a = aa; b = bb; 
    lambda_u = lu; lambda_v = lv; 
    learning_rate = lr;
    alpha_u_smooth = aus; alpha_v_smooth = avs;
    random_seed = rs; max_iter = mi;
    save_lag = sl;
    theta_u_opt = tuo; theta_v_opt = tvo;
    ctr_run = cr; lda_regression = lda_r;
  }

  void save(char* filename) {
    FILE * file = fopen(filename, "w");
    fprintf(file, "a = %.4f\n", a);
    fprintf(file, "b = %.4f\n", b);
    fprintf(file, "lambda_u = %.4f\n", lambda_u);
    fprintf(file, "lambda_v = %.4f\n", lambda_v);
    fprintf(file, "learning_rate = %.6f\n", learning_rate);
    fprintf(file, "alpha_u_smooth = %.6f\n", alpha_u_smooth);
    fprintf(file, "alpha_v_smooth = %.6f\n", alpha_v_smooth);
    fprintf(file, "random seed = %d\n", (int)random_seed);
    fprintf(file, "max iter = %d\n", max_iter);
    fprintf(file, "save lag = %d\n", save_lag);
    fprintf(file, "theta_u opt = %d\n", theta_u_opt);
    fprintf(file, "theta_v opt = %d\n", theta_v_opt);
    fprintf(file, "ctr run = %d\n", ctr_run);
    fprintf(file, "lda_regression = %d\n", lda_regression);
    fclose(file);
  }
};

class c_ctr {
public:
  c_ctr();
  ~c_ctr();
  void read_init_information(const char* theta_v_init_path, 
                             const char* beta_init_path, 
                             const c_corpus* c, double alpha_v_smooth);

  void read_init_information_lib(const char* theta_u_init_path,
                             const c_corpus* c_lib, double alpha_u_smooth);

  void set_model_parameters(int num_factors, 
                            int num_users, 
                            int num_items,
                            int num_items_lib);

  void learn_map_estimate(const c_data* users, const c_data* items, 
		  	  	  	  	  const c_data* users_lib, const c_data* items_lib,
                          const c_corpus* c, const c_corpus* c_lib,
						  const ctr_hyperparameter* param,
                          const char* directory);

  void stochastic_learn_map_estimate(const c_data* users, const c_data* items, 
                                     const c_corpus* c, const ctr_hyperparameter* param, 
                                     const char* directory);

  void init_model(int ctr_run);

  double doc_inference(const c_document* doc, const gsl_vector* theta_v, 
                       const gsl_matrix* log_beta, gsl_matrix* phi, 
                       gsl_vector* gamma, gsl_matrix* word_ss, 
                       bool update_word_ss);
public:
  gsl_matrix* m_beta;
  gsl_matrix* m_theta_v;
  gsl_matrix* m_theta_u;

  gsl_matrix* m_U;
  gsl_matrix* m_V;

  int m_num_factors; // m_num_topics
  int m_num_items; // m_num_docs
  int m_num_users; // num of users
  int m_num_items_lib; // m_num_docs
};

#endif // CTR_H
