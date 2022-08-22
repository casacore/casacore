#include <sys/types.h>
#include <unistd.h>
#include <pthread.h>

#ifndef CASACORE_UTILITY_INTERFACETHREADUNSAFE
#define CASACORE_UTILITY_INTERFACETHREADUNSAFE
namespace casacore {
    // <summary>
    // Object interface for thread unsafe objects
    // Contribution by Benjamin Hugo, Radio Astronomy Research Group, SARAO
    // </summary>
    //
    // <use visibility=export>
    //
    // <reviewed reviewer="" date="" tests="" demos="">
    // </reviewed>
    //
    // <prerequisite>
    // <li>
    // </prerequisite>
    //
    // <synopsis>
    // Reusable pattern to ensure inheriting object is not passed between threads 
    // </synopsis>    
    //
    // <motivation>
    // A substantial portion of the casacore codebase is thread unsafe (asof 2022/02/23)
    // This interface provides reusable code and includes to check whether a calling thread
    // is the thread that originally instantiated the object
    // </motivation>
    // <todo asof="2022/02/23">
    // </todo>
    class InterfaceThreadUnsafe {
    public:
        // sets this object's pid and tid ids
        // inheriting objects must call this constructor upon constructor
        // can override to also check the caller process pid although
        // objects with MPI support probably don't want this
        InterfaceThreadUnsafe(bool checkPid=false, bool checkTid=true);
    protected:
        // verifies this object's pid and tid ids with the current thread
        // raises an exception if they are different
        // inheriting and friend objects should call this method before making changes to the object
        void verifyProcessIdentifier() const;
        // verifies this object's pid and tid ids with the current thread
        // similar to verifyProcessIdentifier() but does not throw
        bool testProcessIdentifier() const;
        // callback method specifying what to do when this object was passed to another thread
        // probably want a nice error message here. Must be overwriten by inheriting classes
        virtual void onMultithreadedAccess() const = 0;
    private:
        pid_t constructorPid;
        pthread_t constructorTid;
        bool checkPid;
        bool checkTid;
    };
} //cc
#endif //CASACORE_UTILITY_INTERFACETHREADUNSAFE