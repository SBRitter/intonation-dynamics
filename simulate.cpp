#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <random>
#include <math.h>

/* Simulates two attractor dynamic system
 *
 * Code inspired by code by A. I. Gafos
 * http://www.ling.uni-potsdam.de/~gafos/code/inc_neut.m
 *
 * Author: Simon Ritter
 * Date: September 26 2018
 *
 * Arguments:
 * 1. control parameter k
 * 2. noise scaler
 * 3. range of x values for simulation
 */
int main(int argc, char *argv[]) {
  double k, s;
  double xlim;
  double t = 10;
  int N = 10000;
  int n = 1000;
  double dt = t / (double)N;
  double dX[N];
  double X[n];
  double sols[n];

  sscanf(argv[1],"%lf",&k); // control parameter
  sscanf(argv[2],"%lf",&s); // noise scaler
  sscanf(argv[3],"%lf",&xlim); // range of x

  std::random_device rd;
  std::mt19937 gen(rd());
  std::normal_distribution<double> d(0.0, 1);
  std::uniform_real_distribution<> uni(-xlim, xlim);

  int i;
  int j;
  for (i = 0; i < n; i++) {
    X[0] = uni(gen);
    for (j = 1; j < N; j++) {
      dX[j-1] = dt * (-4 * 18 * pow(X[j-1], 3) + k * 3 * pow(X[j-1], 2) + 2 * 7.5 * X[j-1]) + s * d(gen) * sqrt(dt);
      X[j] = X[j-1] + dX[j-1];
    }
    sols[i] = X[N-1];
    printf("%lf\n", sols[i]);
  }
  return 0;
}
