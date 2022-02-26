#ifndef CASA_MANAGEDOBJECTPOOL_H
#define CASA_MANAGEDOBJECTPOOL_H

#include <map>
#include <exception>
#include <mutex>
namespace {
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
    class ManagedObjectPool{
    public:
        using TemplateType = ManagedObjectPool<KeyType, ValueType>;
        using MapType = std::map<KeyType, ValueType*>;

        ManagedObjectPool() {}
        ~ManagedObjectPool() {
            std::lock_guard<std::recursive_mutex> lg(poolmutex);
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
            std::lock_guard<std::recursive_mutex> lg(other.poolmutex);
            for (auto i = other.objects.begin(); i != other.objects.end(); ++i) {
                objects[i->first] = i->second;
            }
            other.objects.clear();
        }
            
        TemplateType& operator=(TemplateType&& other) {
            if (this != &other) {
                std::lock_guard<std::recursive_mutex> lg(ManagedObjectPool::classmutex);
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
            std::lock_guard<std::recursive_mutex> lg(poolmutex);
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
            std::lock_guard<std::recursive_mutex> lg(poolmutex);
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
            std::lock_guard<std::recursive_mutex> lg(poolmutex);
            if (objects.find(key) == objects.end()) {
                throw std::invalid_argument("Cannot retrieve object on pool - key not found");
            }
            return *objects[key];
        }
        // delete managed object for given key
        void erase(const KeyType& key) {
            std::lock_guard<std::recursive_mutex> lg(poolmutex);
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
            std::lock_guard<std::recursive_mutex> lg(poolmutex);
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
            std::lock_guard<std::recursive_mutex> lg(poolmutex);
            return objects.empty();
        }
        // Gets number of objects in pool
        size_t size() {
            std::lock_guard<std::recursive_mutex> lg(poolmutex);
            return objects.size();
        }
        // Checks wether the pool contains a specified key
        bool contains(const KeyType & key) {
            std::lock_guard<std::recursive_mutex> lg(poolmutex);
            return objects.find(key) != objects.end();
        }
    private:
        mutable std::recursive_mutex poolmutex;
        static std::recursive_mutex classmutex;
        MapType objects;
    };
    std::recursive_mutex classmutex;
} //cc
#endif