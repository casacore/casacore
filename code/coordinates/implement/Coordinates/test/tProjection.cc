#include <aips/Arrays/Vector.h>
#include <aips/Arrays/ArrayLogical.h>
#include <trial/Coordinates/Projection.h>
#include <aips/Exceptions/Error.h>

#include <iostream.h>


int main()
{
   try {

// Test parameter setting and recovery

   for (Int i=0; i<Projection::N_PROJ; i++) {
      cout << "Testing " << Projection::name(Projection::Type(i)) << endl;
      uInt nP = Projection::nParameters(Projection::Type(i));
      Vector<Double> pars(nP);
      for (uInt j=0; j<nP; j++) {
        pars(j) = Double(j);
      }
      Projection::Type type = (Projection::Type)i;
      Projection proj(type, pars);
//
      if (proj.type() != type) {
        throw(AipsError("Type recovery inconsistent"));
      }
      if (!allEQ(proj.parameters().ac(),pars.ac())) {
        throw(AipsError("Parameters recovery inconsistent"));
      }
      if (proj.name() != Projection::name(type)) {
        throw(AipsError("Name recovery inconsistent"));
      }
   }
      
// Test near function

   cout << "Test near function" << endl;
   Vector<Double> pars(2);
   pars(0) = 0.1; pars(1) = 0.2;
   {
      Projection proj(Projection::SIN, pars);
      Projection proj2(Projection::SIN, pars);
      if (!proj.near(proj2,1e-6)) {
        throw(AipsError("Near function fails"));
      }
   }

// Test assignment (and zero par constructor)

   {
      cout << "Test assignment" << endl;
      Projection proj(Projection::SIN, pars);
      Projection proj2(Projection::TAN);
      proj2 = proj;
//
      if (proj2.name() != proj.name() ||
          !allEQ(proj2.parameters().ac(),proj.parameters().ac()) ||
          proj2.type() != proj.type() ||
          !proj2.near(proj, 1e-6)) {
        throw(AipsError("Assignment fails"));
      }
   }

// Test copy constructor

   {
      cout << "Test copy constructor" << endl;
      Projection proj(Projection::SIN, pars);
      Projection proj2(proj);
      if (proj2.name() != proj.name() ||
          !allEQ(proj2.parameters().ac(),proj.parameters().ac()) ||
          proj2.type() != proj2.type() ||
          !proj2.near(proj, 1e-6)) {
        throw(AipsError("Copy constructor fails"));
      }
   }

   
   } catch (AipsError x) {
      cerr << "aipserror: error " << x.getMesg() << endl;
      return (1);
   }end_try;

   cout << "ok" << endl; 
   return (0);
}
