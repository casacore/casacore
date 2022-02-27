#ifndef CONTAINER_WEAKPTRPROCESSGUARD_H
#define CONTAINER_WEAKPTRPROCESSGUARD_H
#include <string>
#include <sys/types.h>
#include <unistd.h>
#include <pthread.h>
#include <stdexcept>

#include <iostream>
namespace casacore {
    //forward declarations
    template<typename T>
    class WeakptrProcessGuard;
    // <summary>
    // Generic weak pointer guard based on process id
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
    // Generic (weak) pointer guard that checks that a pointer is being dereferenced
    // by the thread that created it. 
    // </synopsis>
    //
    // <motivation>
    // The motivation behind this class is to ensure
    // threadsafety on the various raw pointers of objects that are not threadsafe.
    // Raises an error message if dereferenced within a fork or thread.
    // </motivation>
    // <todo asof="2022/02/23">
    // </todo>
    template <typename T>
    class WeakptrProcessGuard {
    public:
        WeakptrProcessGuard() :
            ptr(nullptr), pid(0), tid(0), Ttypename("T") {}
        WeakptrProcessGuard(T* ptr, std::string Ttypename = "T") :
            ptr(ptr), pid(getpid()), tid(pthread_self()), Ttypename(Ttypename) {}
        WeakptrProcessGuard(const WeakptrProcessGuard<T> &other) {
            pid = other.pid;
            tid = other.tid;
            ptr = other.ptr;
            Ttypename = other.Ttypename;
        }
        WeakptrProcessGuard<T>& operator=(const WeakptrProcessGuard<T> &other) {
            pid = other.pid;
            tid = other.tid;
            ptr = other.ptr;
            Ttypename = other.Ttypename;
            return *this;
        }
        WeakptrProcessGuard<T>& operator=(T* other) {
            pid = getpid();
            tid = pthread_self();
            ptr = other;
            Ttypename = "T";
            return *this;
        }
        ~WeakptrProcessGuard() {}
        //smart pointer supporting dereferencing syntax
        T& operator *(void) {
            this->check_caller_pid();
            if (ptr == nullptr) {
                throw std::runtime_error("Nullptr dereferenced");
            }
            return *ptr;
        }
        T* operator->(void) const {
            this->check_caller_pid();
            if (ptr == nullptr) {
                throw std::runtime_error("Nullptr dereferenced");
            }
            return ptr;
        }
        T& operator[](std::size_t t) {
            this->check_caller_pid();
            if (ptr == nullptr) {
                throw std::runtime_error("Nullptr dereferenced");
            }
            return *(ptr + t);
        }
        T* rawptr() const{
            this->check_caller_pid();
            return this->ptr;
        }
    private:
        // Checks that the caller pid and tid matches the pid and tid of the process that
        // constructed this guard
        void check_caller_pid() const {
            pid_t mypid = getpid();
            pthread_t mytid = pthread_self();
            if (!(mypid == this->pid && mytid == this->tid)) {
                throw std::runtime_error(std::string("Object ") + Ttypename + 
                                        std::string(" is not allowed to be passed to children"
                                                    " processes or threads"));
            }
        }
        T* ptr;
        pid_t pid;
        pthread_t tid;
        std::string Ttypename;
    };
} //cc
#endif //CONTAINER_WEAKPTRPROCESSGUARD_H