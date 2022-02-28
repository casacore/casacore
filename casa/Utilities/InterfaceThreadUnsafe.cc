#include<casacore/casa/Utilities/InterfaceThreadUnsafe.h>

using namespace casacore;

InterfaceThreadUnsafe::InterfaceThreadUnsafe(bool checkPid, bool checkTid) :
    constructorPid(getpid()),
    constructorTid(pthread_self()),
    checkPid(checkPid),
    checkTid(checkTid)
{ 

}

void InterfaceThreadUnsafe::verifyProcessIdentifier() const {
    pid_t fromPid = getpid();
    pthread_t fromTid = pthread_self();
    if (checkTid && (constructorTid != fromTid)) {
        if (constructorPid != fromPid) {
            // inside a different thread, but in another (MPI?) process
            // --allowed -- OS will ensure memory pages are copied on touch
            // bomb only if expressly marked not subprocess safe
            if (checkPid) {
                onMultithreadedAccess();    
            }
        } else {
            // inside a different thread on the same process -- not allowed
            onMultithreadedAccess();
        }   
    }
    if (checkPid && (constructorPid != fromPid)) {
        // user expressly marked this object not to be used inside another process
        onMultithreadedAccess();
    }
}