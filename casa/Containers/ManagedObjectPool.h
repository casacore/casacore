#ifndef CASA_MANAGEDOBJECTPOOL_H
#define CASA_MANAGEDOBJECTPOOL_H

#include <map>
#include <exception>
#include <casacore/casa/Utilities/LockAll.h>
#include <mutex>

namespace casacore {
    //# Forward declarations

    // <summary>
    // Generic Managed Resource Pool
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
    // Generic threadsafe managed resource used to construct a instance pool of objects
    // keyed on a user specifiable key type
    // symantics 
    // </synopsis>
    //
    // <example>
    // <srcblock>
    // class ThreadPool
    // {
    // public:
    // 	ThreadPool(int nthreads) : nthreads(nthreads) {}
    // 	int nthreads;
    // };
    // int main()
    // {
    // 	ManagedObjectPool<std::string,ThreadPool> pool;
    //  pool.constructObject("key", 5);
    //  pool.constructObject("key2", 3);
    //  AlwaysAssert(pool["key"].nthreads == 5, AipsError);
    //  AlwaysAssert(pool["key2"].nthreads == 3, AipsError);
    // }
    // </srcblock>
    // </example>
    //
    // <motivation>
    // Constructs and manages the lifetime of a pool objects / resources.
    // </motivation>
    // <todo asof="2022/02/23">
    // </todo>
    template <typename KeyType, typename ValueType>
    class ManagedObjectPool : public RecursiveLockableObject {
    public:
        using TemplateType = ManagedObjectPool<KeyType, ValueType>;
        using MapType = std::map<KeyType, ValueType*>;

        ManagedObjectPool() {}
        ~ManagedObjectPool() {
            LockAll<std::recursive_mutex> lg(*this);
            for(auto i = objects.begin();  i != objects.end(); ++i) {
                if (i->second != nullptr) {
                    delete i->second;
                    i->second = nullptr;
                }
            }
        }
        // Pool resources is managed, cannot be copied
        ManagedObjectPool(const TemplateType& other) = delete;
        // Pool resources is managed, cannot be copied
        TemplateType& operator=(const TemplateType& other) = delete;

        // Pool resources rvalue move
        ManagedObjectPool(TemplateType&& other) {
            LockAll<std::recursive_mutex> lg(other);
            for (auto i = other.objects.begin(); i != other.objects.end(); ++i) {
                objects[i->first] = i->second;
            }
            other.objects.clear();
        }
            
        TemplateType& operator=(TemplateType&& other) {
            if (this != &other) {
                LockAll<std::recursive_mutex>(*this, other);
                for (auto i = other.objects.begin(); i != other.objects.end(); ++i) {
                    objects[i->first] = i->second;
                }
                other.objects.clear();
            }
            return *this;
        }
        // Adds an object to the pool under a unique user-defined key
        template <typename... Args>
        ValueType& constructObject(KeyType key, const Args&... constructorArgs) {
            LockAll<std::recursive_mutex> lg(*this);
            if (objects.find(key) != objects.end()) {
                throw std::invalid_argument("Cannot create object on pool - key already exists");
            }
            ValueType* newobj = new ValueType(constructorArgs...);
            objects[key] = newobj;
            return *newobj; 
        }
        // Adds an object to the pool under a unique user-defined key
        // this is the non throwing version of constructObject which only
        // constructs if an object with the same key is not yet created
        template <typename... Args>
        ValueType& checkConstructObject(KeyType key, const Args&... constructorArgs) {
            LockAll<std::recursive_mutex> lg(*this);
            ValueType* newobj = nullptr;
            auto it = objects.find(key);
            if (it == objects.end()) {
                newobj = new ValueType(constructorArgs...);
                objects[key] = newobj;
            } else {
                newobj = it->second;
            }
            return *newobj;
        }
        // accesses an object on the pool under given key
        ValueType& operator[](const KeyType& key) {
            LockAll<std::recursive_mutex> lg(*this);
            if (objects.find(key) == objects.end()) {
                throw std::invalid_argument("Cannot retrieve object on pool - key not found");
            }
            return *objects[key];
        }
        // delete managed object for given key
        void erase(const KeyType& key) {
            LockAll<std::recursive_mutex> lg(*this);
            auto it = objects.find(key); 
            if (it == objects.end()) {
                throw std::invalid_argument("Cannot delete object on pool - key not found");
            }
            if (it->second != nullptr) {
                    delete it->second;
                    it->second = nullptr;
                }
            objects.erase(it);
        }
        // delete all managed objects
        void clear() {
            LockAll<std::recursive_mutex> lg(*this);
            for (auto i = objects.begin(); i != objects.end(); ++i) {
                if (i->second != nullptr) {
                    delete i->second;
                    i->second = nullptr;
                }
            }
            objects.clear();
        }
        // Checks whether the pool is empty
        bool empty() {
            LockAll<std::recursive_mutex> lg(*this);
            return objects.empty();
        }
        // Gets number of objects in pool
        size_t size() {
            LockAll<std::recursive_mutex> lg(*this);
            return objects.size();
        }
        // Checks wether the pool contains a specified key
        bool contains(const KeyType & key) {
            LockAll<std::recursive_mutex> lg(*this);
            return objects.find(key) != objects.end();
        }
        // Applies a function to all objects the managed pool
        // first argument of the function must take a reference
        // to a object instance of template type ValueType
        template <class fnT, class... Args>
        void applyOp(fnT&& fn, Args&&... args) {
            LockAll<std::recursive_mutex> lg(*this);
            for(auto it = objects.begin(); it != objects.end(); ++it) {
                fn(*(it->second), std::forward<Args>(args)...);
            }
        }
    private:
        MapType objects;
    };
} //cc
#endif