#include<casacore/casa/Utilities/InterfaceThreadUnsafe.h>

using namespace casacore;

InterfaceThreadUnsafe::InterfaceThreadUnsafe(bool checkPid, bool checkTid) :
    constructorPid(getpid()),
    constructorTid(pthread_self()),
    checkPid(checkPid),
    checkTid(checkTid)
{ 

}

bool InterfaceThreadUnsafe::testProcessIdentifier() const {
    pid_t fromPid = getpid();
    pthread_t fromTid = pthread_self();
    bool isSafe = true;
    if (checkTid && (constructorTid != fromTid)) {
        if (constructorPid != fromPid) {
            // inside a different thread, but in another (MPI?) process
            // --allowed -- OS will ensure memory pages are copied on touch
            // bomb only if expressly marked not subprocess safe
            if (checkPid) {
                isSafe = false;  
            }
        } else {
            // inside a different thread on the same process -- not allowed
            isSafe = false;
        }   
    }
    if (checkPid && (constructorPid != fromPid)) {
        // user expressly marked this object not to be used inside another process
        isSafe = false;
    }
    return isSafe;
}

void InterfaceThreadUnsafe::verifyProcessIdentifier() const {
   if (!testProcessIdentifier()) {
        onMultithreadedAccess();    
   }
}