#include "ctr.h"
#include "opt.h"

extern gsl_rng * RANDOM_NUMBER;
//int min_iter = 15;
int min_iter = 2;
double beta_smooth = 0.01;

c_ctr::c_ctr() {
  m_beta = NULL;
  m_theta_v = NULL;
  m_theta_u = NULL;
  m_U = NULL;
  m_V = NULL;

  m_num_factors = 0; // m_num_topics
  m_num_items = 0; // m_num_docs
  m_num_users = 0; // num of users
}

c_ctr::~c_ctr() {
  // free memory
  if (m_beta != NULL) gsl_matrix_free(m_beta);
  if (m_theta_u != NULL) gsl_matrix_free(m_theta_u);
  if (m_theta_v != NULL) gsl_matrix_free(m_theta_v);
  if (m_U != NULL) gsl_matrix_free(m_U);
  if (m_V != NULL) gsl_matrix_free(m_V);
}

void c_ctr::read_init_information_v(const char* theta_v_init_path,
                                  const char* beta_init_path,
                                  const c_corpus* c_v,
                                  double alpha_v_smooth) {
  int num_topics = m_num_factors;
  m_theta_v = gsl_matrix_alloc(c_v->m_num_docs, num_topics);
  printf("\nreading theta initialization from %s\n", theta_v_init_path);
  FILE * f = fopen(theta_v_init_path, "r");
  mtx_fscanf(f, m_theta_v);
  fclose(f);

  //smoothing
  gsl_matrix_add_constant(m_theta_v, alpha_v_smooth);

  //normalize m_theta, in case it's not
  for (size_t j = 0; j < m_theta_v->size1; j ++) {
    gsl_vector_view theta_v = gsl_matrix_row(m_theta_v, j);
    vnormalize(&theta_v.vector);
  }

  m_beta = gsl_matrix_alloc(num_topics, c_v->m_size_vocab);
  printf("reading beta initialization from %s\n", beta_init_path);
  f = fopen(beta_init_path, "r");
  mtx_fscanf(f, m_beta);
  fclose(f);

  // exponentiate if it's not
  if (mget(m_beta, 0, 0) < 0) {
    mtx_exp(m_beta);
  }
  else {
    gsl_matrix_add_constant(m_beta, beta_smooth);
    for (size_t j = 0; j < m_beta->size1; j ++) {
      gsl_vector_view beta_v = gsl_matrix_row(m_beta, j);
      vnormalize(&beta_v.vector);
    }
  }
}

void c_ctr::read_init_information_u(const char* theta_u_init_path,
                                  const c_corpus* c_u,
                                  double alpha_u_smooth) {
  int num_topics = m_num_factors;
  m_theta_u = gsl_matrix_alloc(c_u->m_num_docs, num_topics);
  printf("\nreading theta_u initialization from %s\n", theta_u_init_path);
  FILE * f = fopen(theta_u_init_path, "r");
  mtx_fscanf(f, m_theta_u);
  fclose(f);

  //smoothing
  gsl_matrix_add_constant(m_theta_u, alpha_u_smooth);

  //normalize m_theta_u, in case it's not
  for (size_t j = 0; j < m_theta_u->size1; j ++) {
    gsl_vector_view theta_u_v = gsl_matrix_row(m_theta_u, j);
    vnormalize(&theta_u_v.vector);
  }

}

void c_ctr::set_model_parameters(int num_factors, 
                                 int num_users, 
                                 int num_items) {
  m_num_factors = num_factors;
  m_num_users = num_users;
  m_num_items = num_items;
}

void c_ctr::init_model(int ctr_run) {

  m_U = gsl_matrix_calloc(m_num_users, m_num_factors);
  m_V = gsl_matrix_calloc(m_num_items, m_num_factors);

  if (ctr_run) {
	gsl_matrix_memcpy(m_U, m_theta_u);
    gsl_matrix_memcpy(m_V, m_theta_v);
  }
  else {
    // this is for convenience, so that updates are similar.
    m_theta_v = gsl_matrix_calloc(m_num_items, m_num_factors);

    for (size_t i = 0; i < m_V->size1; i ++) 
      for (size_t j = 0; j < m_V->size2; j ++) 
        mset(m_V, i, j, runiform());
  }
}


void c_ctr::learn_map_estimate(const c_data* users, const c_data* items, 
							   const c_corpus* c_u, const c_corpus* c_v,
							   const ctr_hyperparameter* param,
                               const char* directory) {
  // init model parameters
  printf("\ninitializing the model ...\n");
  init_model(param->ctr_run);

  // filename
  char name[500];

  // start time
  time_t start, current;
  time(&start);
  int elapsed = 0;

  int iter = 0;
  double log_likelihood = -exp(50), log_likelihood_old;
  double converge = 1.0;

  /// create the state log file 
  sprintf(name, "%s/state.log", directory);
  FILE* file = fopen(name, "w");
  fprintf(file, "iter time log-likelihood converge\n");


  /* alloc auxiliary variables */
  gsl_matrix* XX = gsl_matrix_alloc(m_num_factors, m_num_factors);
  gsl_matrix* A  = gsl_matrix_alloc(m_num_factors, m_num_factors);
  gsl_matrix* B  = gsl_matrix_alloc(m_num_factors, m_num_factors);
  gsl_vector* x  = gsl_vector_alloc(m_num_factors);

  gsl_matrix* phi_u = NULL;
  gsl_matrix* phi_v = NULL;
  gsl_matrix* word_ss_u = NULL;
  gsl_matrix* word_ss_v = NULL;
  gsl_matrix* log_beta = NULL;
  gsl_vector* gamma_u = NULL;
  gsl_vector* gamma_v = NULL;

  if (param->ctr_run && param->theta_u_opt) {
    int max_len_u = c_u->max_corpus_length();
    phi_u = gsl_matrix_calloc(max_len_u, m_num_factors);
    word_ss_u = gsl_matrix_calloc(m_num_factors, c_u->m_size_vocab);
    gamma_u = gsl_vector_alloc(m_num_factors);
  }
  if (param->ctr_run && param->theta_v_opt) {
    int max_len_v = c_v->max_corpus_length();
    phi_v = gsl_matrix_calloc(max_len_v, m_num_factors);
    word_ss_v = gsl_matrix_calloc(m_num_factors, c_v->m_size_vocab);
    gamma_v = gsl_vector_alloc(m_num_factors);
  }
  if (param->ctr_run && (param->theta_u_opt || param->theta_v_opt)) {
	log_beta = gsl_matrix_calloc(m_num_factors, c_u->m_size_vocab);
	gsl_matrix_memcpy(log_beta, m_beta);
	mtx_log(log_beta);
  }

  /* tmp variables for indexes */
  int i, j, m, n, l, k;
  int* item_ids; 
  int* user_ids;

  double result;

  /// confidence parameters
  double a_minus_b = param->a - param->b;

  while ((iter < param->max_iter and converge > 1e-4 ) or iter < min_iter) {

    log_likelihood_old = log_likelihood;
    log_likelihood = 0.0;

    // update U
    if (param->ctr_run && param->theta_u_opt) gsl_matrix_set_zero(word_ss_u);

    gsl_matrix_set_zero(XX);
    for (j = 0; j < m_num_items; j ++) {
      m = items->m_vec_len[j];
      if (m>0) {
        gsl_vector_const_view v = gsl_matrix_const_row(m_V, i);
        gsl_blas_dger(1.0, &v.vector, &v.vector, XX);
      }
    }
    gsl_matrix_scale(XX, param->b);

    for (i = 0; i < m_num_users; i ++) {
      gsl_vector_view u = gsl_matrix_row(m_U, i);
      gsl_vector_view theta_u = gsl_matrix_row(m_theta_u, i);

      item_ids = users->m_vec_data[i];
      n = users->m_vec_len[i];
      if (n>0) {
        // n > 0, this user has rated some articles
        gsl_matrix_memcpy(A, XX);
        gsl_vector_set_zero(x);
        for (l = 0; l < n; l ++) {
          j = item_ids[l];
          gsl_vector_const_view v = gsl_matrix_const_row(m_V, j);
          gsl_blas_dger(a_minus_b, &v.vector, &v.vector, A);
          gsl_blas_daxpy(param->a, &v.vector, x);
        }

        // adding the topic vector
        // even when ctr_run=0, m_theta=0
        gsl_blas_daxpy(param->lambda_u, &theta_u.vector, x);

        gsl_matrix_memcpy(B, A); // save for computing likelihood

        gsl_matrix_add_diagonal(A, param->lambda_u);
        matrix_vector_solve(A, x, &u.vector);

        // likelihood part of theta, even when theta=0, which is a
        // special case
        gsl_vector_memcpy(x, &u.vector);
        gsl_vector_sub(x, &theta_u.vector);
        gsl_blas_ddot(x, x, &result);
        log_likelihood += -0.5 * param->lambda_u * result;

//        if (param->ctr_run && param->theta_u_opt) {
//          const c_document* doc =  c_u->m_docs[i];
//          likelihood += doc_inference(doc, &theta_u.vector, log_beta, phi_u, gamma_u, word_ss_u, true);
//          optimize_simplex(gamma_u, &u.vector, param->lambda_u, &theta_u.vector);
//        }
      }
//      else {
//      // n=0, this user has no rating
//        if (param->ctr_run && param->theta_u_opt) {
//          const c_document* doc =  c_u->m_docs[i];
//          doc_inference(doc, &theta_u.vector, log_beta, phi_u, gamma_u, word_ss_u, false);
//          vnormalize(gamma_u);
//          gsl_vector_memcpy(&theta_u.vector, gamma_u);
//        }
//      }
    }
    
    if (param->lda_regression) break; // one iteration is enough for lda-regression

    // update V
    if (param->ctr_run && param->theta_v_opt) gsl_matrix_set_zero(word_ss_v);

    gsl_matrix_set_zero(XX);
    for (i = 0; i < m_num_users; i ++) {
      n = users->m_vec_len[i]; 
      if (n>0) {
        gsl_vector_const_view u = gsl_matrix_const_row(m_U, i);
        gsl_blas_dger(1.0, &u.vector, &u.vector, XX);
      }
    }
    gsl_matrix_scale(XX, param->b);

    for (j = 0; j < m_num_items; j ++) {
      gsl_vector_view v = gsl_matrix_row(m_V, j);
      gsl_vector_view theta_v = gsl_matrix_row(m_theta_v, j);

      user_ids = items->m_vec_data[j];
      m = items->m_vec_len[j];
      if (m>0) {
        // m > 0, some users have rated this article
        gsl_matrix_memcpy(A, XX);
        gsl_vector_set_zero(x);
        for (l = 0; l < m; l ++) {
          i = user_ids[l];
          gsl_vector_const_view u = gsl_matrix_const_row(m_U, i);  
          gsl_blas_dger(a_minus_b, &u.vector, &u.vector, A);
          gsl_blas_daxpy(param->a, &u.vector, x);
        }

        // adding the topic vector
        // even when ctr_run=0, m_theta=0
        gsl_blas_daxpy(param->lambda_v, &theta_v.vector, x);
        
        gsl_matrix_memcpy(B, A); // save for computing likelihood 

        gsl_matrix_add_diagonal(A, param->lambda_v);  
        matrix_vector_solve(A, x, &v.vector);

        // update the likelihood for the relevant part
        log_likelihood += -0.5 * m * param->a;
        for (l = 0; l < m; l ++) {
          i = user_ids[l];
          gsl_vector_const_view u = gsl_matrix_const_row(m_U, i);  
          gsl_blas_ddot(&u.vector, &v.vector, &result);
          log_likelihood += param->a * result;
        }
        log_likelihood += -0.5 * mahalanobis_prod(B, &v.vector, &v.vector);
        // likelihood part of theta, even when theta=0, which is a
        // special case
        gsl_vector_memcpy(x, &v.vector);
        gsl_vector_sub(x, &theta_v.vector);
        gsl_blas_ddot(x, x, &result);
        log_likelihood += -0.5 * param->lambda_v * result;
        
//        if (param->ctr_run && param->theta_v_opt) {
//          const c_document* doc =  c_v->m_docs[j];
//          likelihood += doc_inference(doc, &theta_v.vector, log_beta, phi_v, gamma_v, word_ss_v, true);
//          optimize_simplex(gamma_v, &v.vector, param->lambda_v, &theta_v.vector);
//        }
      }
//      else {
//      // m=0, this article has never been rated
//        if (param->ctr_run && param->theta_v_opt) {
//          const c_document* doc =  c_v->m_docs[j];
//          doc_inference(doc, &theta_v.vector, log_beta, phi_v, gamma_v, word_ss_v, false);
//          vnormalize(gamma_v);
//          gsl_vector_memcpy(&theta_v.vector, gamma_v);
//        }
//      }
    }

//    // update beta if needed
//    if (param->ctr_run && param->theta_v_opt) {
//        gsl_matrix_memcpy(m_beta, word_ss_v);
//        for (k = 0; k < m_num_factors; k ++) {
//          gsl_vector_view row = gsl_matrix_row(m_beta, k);
//          vnormalize(&row.vector);
//        }
//        gsl_matrix_memcpy(log_beta, m_beta);
//        mtx_log(log_beta);
//    }

    time(&current);
    elapsed = (int)difftime(current, start);

    iter++;
    converge = fabs((log_likelihood-log_likelihood_old)/log_likelihood_old);

    if (log_likelihood < log_likelihood_old) printf("likelihood is decreasing!\n");

    fprintf(file, "%04d %06d %13.5f %.10f\n", iter, elapsed, log_likelihood, converge);
    fflush(file);
    printf("iter=%04d, time=%06d, log-likelihood=%.5f, converge=%.10f\n", iter, elapsed, log_likelihood, converge);

    // save intermediate results
    if (iter % param->save_lag == 0) {

      sprintf(name, "%s/%04d-U.dat", directory, iter);
      FILE * file_U = fopen(name, "w");
      mtx_fprintf(file_U, m_U);
      fclose(file_U);

      sprintf(name, "%s/%04d-V.dat", directory, iter);
      FILE * file_V = fopen(name, "w");
      mtx_fprintf(file_V, m_V);
      fclose(file_V);

      if (param->ctr_run) { 
  		if (param->theta_u_opt) {
			sprintf(name, "%s/%04d-theta_u.dat", directory, iter);
			FILE * file_theta_u = fopen(name, "w");
			mtx_fprintf(file_theta_u, m_theta_u);
			fclose(file_theta_u);
  		}

		if (param->theta_v_opt) {
	        sprintf(name, "%s/%04d-theta_v.dat", directory, iter);
	        FILE * file_theta_v = fopen(name, "w");
	        mtx_fprintf(file_theta_v, m_theta_v);
	        fclose(file_theta_v);

			sprintf(name, "%s/%04d-beta.dat", directory, iter);
			FILE * file_beta = fopen(name, "w");
			mtx_fprintf(file_beta, m_beta);
			fclose(file_beta);
		}
      }
    }
  }

  // save final results
  sprintf(name, "%s/final-U.dat", directory);
  FILE * file_U = fopen(name, "w");
  mtx_fprintf(file_U, m_U);
  fclose(file_U);

  sprintf(name, "%s/final-V.dat", directory);
  FILE * file_V = fopen(name, "w");
  mtx_fprintf(file_V, m_V);
  fclose(file_V);

  if (param->ctr_run) { 
	if (param->theta_u_opt) {
		sprintf(name, "%s/final-theta_u.dat", directory);
		FILE * file_theta_u = fopen(name, "w");
		mtx_fprintf(file_theta_u, m_theta_u);
		fclose(file_theta_u);
	}

	if (param->theta_v_opt) {
		sprintf(name, "%s/final-theta_v.dat", directory);
		FILE * file_theta_v = fopen(name, "w");
		mtx_fprintf(file_theta_v, m_theta_v);
		fclose(file_theta_v);

		sprintf(name, "%s/final-beta.dat", directory);
		FILE * file_beta = fopen(name, "w");
		mtx_fprintf(file_beta, m_beta);
		fclose(file_beta);
	}
  }

  // free memory
  gsl_matrix_free(XX);
  gsl_matrix_free(A);
  gsl_matrix_free(B);
  gsl_vector_free(x);

  if (param->ctr_run && param->theta_u_opt) {
    gsl_matrix_free(phi_u);
    gsl_matrix_free(word_ss_u);
    gsl_vector_free(gamma_u);
  }
  if (param->ctr_run && param->theta_v_opt) {
    gsl_matrix_free(phi_v);
    gsl_matrix_free(word_ss_v);
    gsl_vector_free(gamma_v);
  }
  if (param->ctr_run && (param->theta_u_opt || param->theta_v_opt)) {
    gsl_matrix_free(log_beta);
  }
}

double c_ctr::doc_inference(const c_document* doc, const gsl_vector* theta,
                            const gsl_matrix* log_beta, gsl_matrix* phi,
                            gsl_vector* gamma, gsl_matrix* word_ss, 
                            bool update_word_ss) {

  double pseudo_count = 1.0;
  double likelihood = 0;
  gsl_vector* log_theta = gsl_vector_alloc(theta->size);
  gsl_vector_memcpy(log_theta, theta);
  vct_log(log_theta);

  int n, k, w;
  double x;
  for (n = 0; n < doc->m_length; n ++) {
    w = doc->m_words[n]; 
    for (k = 0; k < m_num_factors; k ++)
      mset(phi, n, k, vget(theta, k) * mget(m_beta, k, w));

    gsl_vector_view row =  gsl_matrix_row(phi, n);
    vnormalize(&row.vector);

    for (k = 0; k < m_num_factors; k ++) {
      x = mget(phi, n, k);
      if (x > 0) 
        likelihood += x*(vget(log_theta, k) + mget(log_beta, k, w) - log(x));
    }
  }

  if (pseudo_count > 0) {
    likelihood += pseudo_count * vsum(log_theta);
  }

  gsl_vector_set_all(gamma, pseudo_count); // smoothing with small pseudo counts
  for (n = 0; n < doc->m_length; n ++) {
    for (k = 0; k < m_num_factors; k ++) {
      x = doc->m_counts[n] * mget(phi, n, k);
      vinc(gamma, k, x);      
      if (update_word_ss) minc(word_ss, k, doc->m_words[n], x);
    }
  }

  gsl_vector_free(log_theta);
  return likelihood;
}
