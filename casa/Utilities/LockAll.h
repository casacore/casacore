#ifndef CASA_UTILITY_LOCKALL
#define CASA_UTILITY_LOCKALL
#include <mutex>
#include <vector>
#include <algorithm>
#include <unistd.h>

namespace casacore {
    // forward declare
    template <typename T> class LockAll;

    class LockAllAttemptsExceeded : public std::exception {
        virtual const char * what () const throw (){
            return "Maximum locking attempts exceeded on object";
        }
    };
    class LockAllNotAllUnique : public std::exception {
        virtual const char * what () const throw (){
            return "Objects being locked are not uniquely identifyable -- must call base constructor";
        }
    };

    // <summary>
    // Object interface for objects that need simultaneous locking to ensure thread safety
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
    // Reusable pattern to ensure thread safety when 1..N objects need locking
    // std::lock is of limited use because it does not take a vector of locks
    // and can generally only easily be used when the number of locks is known at compiletime
    // a vector of LockableObject can be passed to a LockAll class which will act as a lock
    // guard for all the locks (RAII) for the lifetime of its scope
    // The algorithm follows the general pattern that requires locks to be picked up
    // all successfully or none at all. They also have to be picked up in a global (total) order
    // by all threads using this pattern. Therefore we construct a unique identifier per implementing object
    // </synopsis>    
    //
    // <motivation>
    // A substantial portion of the casacore codebase is thread unsafe (asof 2022/02/23)
    // This interface provides reusable code to address the simultaneous locking behaviour needed
    // in e.g. TableProxy
    // </motivation>
    // <todo asof="2022/02/23">
    // </todo>
    template <class MutexType>
    class LockableObject {
    public:
        // Note:: Inheriting object ****!!MUST!!**** call the base constructor
        LockableObject() :
            __object_uniq_id__(LockableObject::__num_allocated_objects__++) {}
    protected:
        virtual MutexType& object_mutex() const {
            return itsmutex;
        }
    private:
        mutable MutexType itsmutex;
        size_t __object_uniq_id__;
        static size_t __num_allocated_objects__;
        friend class LockAll<MutexType>;
    };
    template <typename MutexType>
    size_t LockableObject<MutexType>::__num_allocated_objects__ = 0;

    // convenience type declarations
    class NonRecursiveLockableObject : public LockableObject<std::mutex> {};
    class RecursiveLockableObject : public LockableObject<std::recursive_mutex> {};
    // <summary>
    // LockAll simultaneous locking pattern
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
    // Reusable pattern to ensure thread safety when 1..N objects need locking
    // std::lock is of limited use because it does not take a vector of locks
    // and can generally only easily be used when the number of locks is known at compiletime
    // a vector of LockableObject can be passed to a LockAll class which will act as a lock
    // guard for all the locks (RAII) for the lifetime of its scope
    // The algorithm follows the general pattern that requires locks to be picked up
    // all successfully or none at all. They also have to be picked up in a global (total) order
    // by all threads using this pattern. Therefore we construct a unique identifier per implementing object
    // </synopsis>    
    //
    // <motivation>
    // A substantial portion of the casacore codebase is thread unsafe (asof 2022/02/23)
    // This interface provides reusable code to address the simultaneous locking behaviour needed
    // in e.g. TableProxy
    // </motivation>
    // <todo asof="2022/02/23">
    // </todo>
    template <typename MutexType>
    class LockAll {
    public:
        LockAll() = delete;
        LockAll(const LockAll& rhs) = delete;
        // on construction implement a deadlock-avoiding algorithm to aquire all locks
        // if cannot aquire all locks retry indefinitely if retry == 0
        LockAll(const std::vector<const LockableObject<MutexType>*>& listLockableObject, size_t retry=0) :
            myLockedObjects(listLockableObject.size()) {
            // the algorithm only works if a total order can be established on the set of objects
            std::vector<const LockableObject<MutexType>*> obs = LockAll::giveTotalOrder(listLockableObject);
            // check total order all uniquely labelled
            for (size_t i = 1; i < obs.size(); ++i) {
                if (obs[i-1] == obs[i]) {
                    throw LockAllNotAllUnique(); // should never happen unless the base constructor was not called...
                }
            }
            // pickup ascending - if we cannot pick up we release all we picked up thus far
            auto pickupAll = [&listLockableObject,&retry,&obs]() -> bool {
                bool obtainedAllThusFar = true;
                for (size_t it = 0; it != obs.size(); ) {
                    // try acquire successful => continue upwards
                    if (obtainedAllThusFar && obs[it]->object_mutex().try_lock()) {
                        ++it;
                    } else { // cannot acquire this one... start backtracking
                        obtainedAllThusFar = false;
                        // if we backtracked to the start of the list then stop
                        if (it == 0) break;
                        --it;
                    }
                }
                return obtainedAllThusFar;
            };
            bool acquireSuccess = false;
            if (retry != 0) {
                for (size_t i = 0; i < retry; ++i){
                    acquireSuccess = pickupAll();
                    if (acquireSuccess) break;
                }
            } else {
                while (!acquireSuccess) {
                    acquireSuccess = pickupAll();
                    if (!acquireSuccess) sleep(1);
                }
            }
            // if not obtained after N attempts fail
            if (!acquireSuccess) {
                throw LockAllAttemptsExceeded();
            }
            // aquired all -- need to keep them handy for releasing them later on RAII scope end
            std::copy(obs.begin(), obs.end(), myLockedObjects.begin());
        }
        // Construct lock guard on single object
        LockAll(const LockableObject<MutexType>& obj, size_t retry=0) :
            LockAll(std::vector<const LockableObject<MutexType>*>{&obj}, retry){
        }
        // Construct lock guard two objects
        LockAll(const LockableObject<MutexType>& obj1,
                const LockableObject<MutexType>& obj2,
                size_t retry=0) :
            LockAll(std::vector<const LockableObject<MutexType>*>{&obj1, &obj2}, retry){
        }
        // at scope end implement a deadlock-avoiding algorithm to release all locks
        ~LockAll() {
            releaseLockedResources();
        }
        LockAll& operator=(const LockAll& rhs) = delete;
        // Move assignment operator hands control of the locked resources to the lhs
        // the pool will have no locks after this and essentially goes defunct
        LockAll& operator=(LockAll&& rhs) {
            if (this != &rhs) {
                std::unique_lock<std::mutex> _me(this->lockpoolMutex, std::defer_lock),
                                             _other(rhs.lockpoolMutex, std::defer_lock);
                std::lock(_me, _other);
                this->myLockedObjects.resize(rhs.myLockedObjects.size());
                for (size_t i = 0; i < rhs.myLockedObjects.size(); ++i) {
                    this->myLockedObjects[i] = rhs.myLockedObjects[i];
                }
                // clear without releaseing the other pool -- its destructor will have no effect
                rhs.myLockedObjects.clear();
            }
            return *this;
        }
        // Move constructr hands control of the locked resources to the lhs
        // the pool will have no locks after this and essentially goes defunct
        LockAll(LockAll&& rhs) {
            std::lock_guard<std::mutex> lg(rhs.lockpoolMutex);
            this->myLockedObjects.resize(rhs.myLockedObjects.size());
            for (size_t i = 0; i < rhs.myLockedObjects.size(); ++i) {
                this->myLockedObjects[i] = rhs.myLockedObjects[i];
            }
            // clear without releaseing the other pool -- its destructor will have no effect
            rhs.myLockedObjects.clear();
        }
    private:
        std::mutex lockpoolMutex;
        std::vector<const LockableObject<MutexType>*> myLockedObjects;
        void releaseLockedResources() {
            // guarenteed that myLockedObjects are already a total order by the constructor sort
            // guarenteed that myLockedObjects are all locked
            // we always unlock from the inverse order that was used in the constructor (ie. desc)
            for (auto it = myLockedObjects.rbegin(); it != myLockedObjects.rend(); ++it) {
                (*it)->object_mutex().unlock();
            }
            myLockedObjects.clear();
        }
        // give a global (total) order (ascending default)
        static std::vector<const LockableObject<MutexType>*> giveTotalOrder(const std::vector<const LockableObject<MutexType>*>& unsortedLockableObject, 
                                                                            bool asnd=true) {
            std::vector<const LockableObject<MutexType>*> result(unsortedLockableObject.size());
            std::copy(unsortedLockableObject.begin(), unsortedLockableObject.end(), result.begin());
            std::sort(result.begin(), result.end(), 
                        [&asnd](const LockableObject<MutexType>* a, const LockableObject<MutexType>* b) -> bool {
                        return asnd ? a->__object_uniq_id__ < b->__object_uniq_id__ : \
                                      a->__object_uniq_id__ > b->__object_uniq_id__;
                        });
            return result;
        }
    };
} // cc
#endif //CASA_UTILITY_LOCKALL