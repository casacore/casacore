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
    if ((checkPid && (constructorPid != fromPid)) ||
        (checkTid && (constructorTid != fromTid))) {
        onMultithreadedAccess();
    }
}