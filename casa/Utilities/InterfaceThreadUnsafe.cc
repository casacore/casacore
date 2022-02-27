#include<casacore/casa/Utilities/InterfaceThreadUnsafe.h>

using namespace casacore;

InterfaceThreadUnsafe::InterfaceThreadUnsafe() :
    constructorPid(getpid()),
    constructorTid(pthread_self()) { }

void InterfaceThreadUnsafe::verifyProcessIdentifier() const {
    pid_t fromPid = getpid();
    pthread_t fromTid = pthread_self();
    if (constructorPid != fromPid || constructorTid != fromTid) {
        onMultithreadedAccess();
    }
}