
#include <trial/Fitting/FitGaussian.h>
#include <aips/Functionals/Gaussian1D.h>
#include <aips/Functionals/Gaussian2D.h>
#include <trial/Functionals/Gaussian3D.h>

void printfparameters(Function<Double> &f);
void printparameters(Matrix<Double> &m);
void createdata(Matrix<Double> &pos, Vector<Double> &f, Float range, uInt n,
                Matrix<Double> &components);
Int ipow(Int base, uInt power);


int main()
{
  Matrix<Double> pos;
  Vector<Double> f;

  Matrix<Double> components;
  Matrix<Double> estimate;
  Matrix<Double> retryfactors;
  Matrix<Double> solution;

  FitGaussian<Double> fitgauss;

  
  cout << "TEST 1:  1 Gaussian in 1 Dimension." << endl;

  fitgauss.setDimensions(1);
  fitgauss.setNumGaussians(1);

  components.resize(1,3);
  components(0,0) = 5;  
  components(0,1) = 2; 
  components(0,2) = 4;
  createdata(pos, f, 10.0, 11, components);

  estimate.resize(1, 3);
  estimate(0,0) = 1; estimate(0,1) = 1; estimate(0,2) = 1;
  fitgauss.setFirstEstimate(estimate);  

  try {
    solution = fitgauss.fit(pos, f);
  } catch (AipsError err) {
    cout << "ERROR: ";
    string errormesg = err.getMesg();
    cout << errormesg << endl;
    return 1;
  }

  cout << "   Given Parameters:"; printparameters(components);
  cout << "Solution Parameters:"; printparameters(solution); 





  cout << endl << "TEST 2:  1 Gaussian in 2 Dimensions" << endl;

  fitgauss.setDimensions(2);
  fitgauss.setNumGaussians(1);

  components.resize(1,2*3); 
  components(0,0) = 3;   components(0,1) = -1;  components(0,2) = 1;
  components(0,3) = 3;   components(0,4) = 0.5; components(0,5) = 1;
  createdata(pos, f, 4.0, 9, components);

  estimate.resize(1,6);
  estimate(0,0) = 1; estimate(0,1) = 0;   estimate(0,2) = 0;
  estimate(0,3) = 1; estimate(0,4) = 0.5; estimate(0,5) = 1;
  fitgauss.setFirstEstimate(estimate);

  solution.resize();
  solution = fitgauss.fit(pos, f);

  cout << "   Given Parameters:"; printparameters(components);
  cout << "Solution Parameters:"; printparameters(solution);




 
  cout << endl << "TEST 3:  2 Gaussians in 2 Dimensions" << endl;

  fitgauss.setDimensions(2);
  fitgauss.setNumGaussians(2);

  components.resize(2,6); 
  components(0,0) = 3;   components(0,1) = 1;  components(0,2) = 1;
  components(0,3) = 2.2; components(0,4) = 0.85; components(0,5) = 0.25;
  components(1,0) = 3;   components(1,1) = -2;   components(1,2) = -2;
  components(1,3) = 2.5; components(1,4) = 0.75; components(1,5) = 2.9;
  createdata(pos, f, 4, 9, components);

  estimate.resize(2,6);
  estimate(0,0) = 1; estimate(0,1) = 1;   estimate(0,2) = 1;
  estimate(0,3) = 1; estimate(0,4) = 0.5; estimate(0,5) = 1;
  estimate(1,0) = 1; estimate(1,1) = -2;   estimate(1,2) = -2;
  estimate(1,3) = 1; estimate(1,4) = 0.5; estimate(1,5) = 1;
  fitgauss.setFirstEstimate(estimate);

  retryfactors.resize(2,6);
  retryfactors(0,0) = 2;   retryfactors(0,1) = 0;   retryfactors(0,2) = 2;
  retryfactors(0,3) = 2;   retryfactors(0,4) = 1.1; retryfactors(0,5) = 0;
  retryfactors(1,0) = 1.5; retryfactors(1,1) = 0;   retryfactors(1,2) = 1.5;
  retryfactors(1,3) = 1.5; retryfactors(1,4) = 1.2; retryfactors(1,5) = 0;
  fitgauss.setRetryFactors(retryfactors);

  solution.resize();
  solution = fitgauss.fit(pos, f);

  cout << "   Given Parameters:"; printparameters(components);
  cout << "Solution Parameters:"; printparameters(solution);






  cout << endl << "TEST 4:  1 Gaussian in 3 Dimensions" << endl;

  components.resize(1,9); 
  components(0,0) = 3;   components(0,1) = -1;  components(0,2) = 1;
  components(0,3) = 1;   components(0,4) = 0.5; components(0,5) = 1;
  components(0,6) = 1.5; components(0,7) = 0.5; components(0,8) = -0.5;
  createdata(pos, f, 2.0, 5, components);

  fitgauss.setDimensions(3);
  fitgauss.setNumGaussians(1);

  estimate.resize(1,9);
  estimate(0,0) = 1;  estimate(0,1) = 0; estimate(0,2) = 0;
  estimate(0,3) = 0;  estimate(0,4) = 1.0; estimate(0,5) = 0.5;
  estimate(0,6) = 1.4;estimate(0,7) = -0.8; estimate(0,8) = 0;   
  fitgauss.setFirstEstimate(estimate);

  solution.resize();
  solution = fitgauss.fit(pos, f);

  cout << "   Given Parameters:"; printparameters(components);
  cout << "Solution Parameters:"; printparameters(solution);



  cout << endl << "TEST 5:  3 Gaussians in 3 Dimensions" << endl;

  fitgauss.setDimensions(3);
  fitgauss.setNumGaussians(3);

  components.resize(3,9); 
  components(0,0) = 3;   components(0,1) = 0;   components(0,2) = 0;
  components(0,3) = 1;   components(0,4) = 1.5; components(0,5) = 1;
  components(0,6) = 2;   components(0,7) = 0.3; components(0,8) = 0.1;
  components(1,0) = 2.5; components(1,1) = -2;  components(1,2) = -2;
  components(1,3) = -1;  components(1,4) = 2;   components(1,5) = 1.7;
  components(1,6) = 1.1; components(1,7) = 0.5; components(1,8) = -0.5;
  components(2,0) = 2.1; components(2,1) = 2;   components(2,2)  = 2;
  components(2,3) = -2;  components(2,4) = 2;   components(2,5) = 2.1;
  components(2,6) = 1.2; components(2,7) = 0;   components(2,8) = 0;
  createdata(pos, f, 3.0, 7, components);

  estimate.resize(3,9);
  estimate(0,0) = 3;   estimate(0,1) = 0;   estimate(0,2) = 0;
  estimate(0,3) = 1;   estimate(0,4) = 0.8; estimate(0,5) = 1.5;
  estimate(0,6) = 2.2; estimate(0,7) = 0;   estimate(0,8) = 0;
  estimate(1,0) = 2.5; estimate(1,1) = -2;  estimate(1,2) = -2;
  estimate(1,3) = -1;  estimate(1,4) = 0.8; estimate(1,5) = 1.5;
  estimate(1,6) = 2.2; estimate(1,7) = 0;   estimate(1,8) = 0;
  estimate(2,0) = 2.1; estimate(2,1) = 2;   estimate(2,2)  = 2;
  estimate(2,3) = -2;  estimate(2,4) = 0.8; estimate(2,5) = 1.5;
  estimate(2,6) = 2.2; estimate(2,7) = 0;   estimate(2,8) = 0;
  fitgauss.setFirstEstimate(estimate);

  retryfactors.resize(1,9);
  retryfactors(0,0) = 1;   retryfactors(0,1) = 0;   retryfactors(0,2) = 0;
  retryfactors(0,3) = 0;   retryfactors(0,4) = 3;   retryfactors(0,5) = 1;
  retryfactors(0,6) = 2;   retryfactors(0,7) = 0;   retryfactors(0,8) = 0;
  fitgauss.setRetryFactors(retryfactors);

  solution.resize();
  solution = fitgauss.fit(pos, f);

  cout << "   Given Parameters:"; printparameters(components);
  cout << "Solution Parameters:"; printparameters(solution);


  return 0;
}


void createdata(Matrix<Double> &pos, Vector<Double> &f, Float range, uInt n,
                Matrix<Double> &components)
{
  uInt i = 0;
  uInt dim = components.ncolumn() / 3;
  uInt imax = ipow(n,dim);

  pos.resize(imax,dim);
  f.resize(imax);

  //set up functions
  Block<Gaussian1D<Double> > datagauss1d((dim==1) * components.nrow());
  Block<Gaussian2D<Double> > datagauss2d((dim==2) * components.nrow());
  Block<Gaussian3D<Double> > datagauss3d((dim==3) * components.nrow());
  for (uInt g = 0; g < components.nrow(); g++)
    for (uInt p = 0; p < components.ncolumn(); p++)
    {
      if (dim==1) datagauss1d[g][p] = components(g,p);
      if (dim==2) datagauss2d[g][p] = components(g,p);
      if (dim==3) datagauss3d[g][p] = components(g,p);
    }

  //create the data
  Vector<Double> curpos(dim);
  curpos = -range;
  Float inc = 2.0 * range / (n-1);
  while(i < imax)
  {    
    f(i) = 0;
    for (uInt g = 0; g < components.nrow(); g++)
    {
      if (dim==1) f(i) += datagauss1d[g](curpos);
      if (dim==2) f(i) += datagauss2d[g](curpos);
      if (dim==3) f(i) += datagauss3d[g](curpos(0), curpos(1), curpos(2)); //!
    }
    pos.row(i) = curpos;

    //cout << i << ") " << curpos << " = " << f(i) << endl;  

    curpos(dim-1) += inc;
    for (uInt a = dim-1; a > 0; a--)
      if (curpos(a) >= range + inc*0.1) 
        {curpos(a) = -range; curpos(a-1) += inc;}

    i++;
  }

}


void printfparameters(Function<Double> &f)
{
  uInt p;
  for (p = 0; p < f.nparameters() - 1; p++) cout << f[p] << ", ";
  cout << f[p] << endl;
}

void printparameters(Matrix<Double> &m)
{
  uInt g,p;
  for (g = 0; g < m.nrow(); g++)
  {
    for (p = 0; p < m.ncolumn() - 1; p++) cout << m(g,p) << ", ";
    cout << m(g,p) << endl;
    if (g < m.nrow() - 1) cout << "                    ";
  }

}

Int ipow(Int base, uInt power)
{
  Int ans = 1;
  while (power--) ans *= base; 
  return ans;
}
