#include <iostream>
#include <cassert>

#include <memory>
#include <mutex>
#include <tuple>
#include <unordered_map>

#ifndef CASA_MULTITON_H
#define CASA_MULTITON_H

namespace casacore {
    //# Forward declarations

    // <summary>
    // Generic Multiton pattern
    // Contribution by Simon Perkins, Radio Astronomy Research Group, SARAO
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
    // Generic multiton pattern implementation for wrapping objects where 
    // allocations are restricted to single objects for a given set of arguments.
    //
    // The class is fully thread-safe, thus the same object can be used safely
    // in multiple threads.
    //
    // Note:: Constructor arguments must be defined rvalue reference that support CXX11 std::move
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
    // 	auto p1 = multiton_get<double>(5.0);
    // 	auto p2 = multiton_get<double>(5.0);
    //
    // 	std::cout << p1 << std::endl;
    // 	std::cout << p2 << std::endl;
    //
    // 	assert(p1 == p2);
    //
    // 	auto p3 = multiton_get<ThreadPool>(5);
    // 	auto p4 = multiton_get<ThreadPool>(5);
    //
    // 	std::cout << p3 << std::endl;
    // 	std::cout << p4 << std::endl;
    //
    // 	assert(p3 == p4);
    //
    // }
    // </srcblock>
    // </example>
    //
    // <motivation>
    // Could be useful to simplify allocation of objects with large buffers for reuse system wide,
    // or instances where caches should only be invalidated, by e.g. different threads
    // </motivation>
    // <todo asof="2022/02/23">
    // </todo>
    template <typename T>
    std::size_t hash_value(const T & value)
    {
        std::hash<T> hasher;
        return hasher(value);
    }

    template <typename ... Args>
    std::size_t hash_value(const std::tuple<Args ...> & value)
    {
        return tuple_hash(value, std::make_index_sequence<sizeof ...(Args)>());
    }

    template<typename Tuple, std::size_t ... ids>
    std::size_t tuple_hash(const Tuple & tuple, const std::index_sequence<ids...> &)
    {
        // Not an expert in hashing.
        // Not sure 0 is a good seed (or this algorithm is good) solely here for demo purpose.
        std::size_t result = 0;

        for(auto const & hash: {hash_value(std::get<ids>(tuple))...})
        {
            result ^= hash + 0x9e3779b9 + (result << 6) + (result >> 2);
        }

        return result;
    }


    template <typename ... Args>
    class ArgHasher
    {
    public:
        std::size_t operator()(const std::shared_ptr<std::tuple<Args...>> & value) const
        {
            return tuple_hash(*value, std::make_index_sequence<sizeof ...(Args)>());
        }
    };

    template <typename KeyPair>
    class KeyEqual
    {
    public:
        bool operator()(const KeyPair & v1,	const KeyPair & v2) const
            { return *v1 == *v2; }
    };

    template <typename T, typename ... Args>
    class Multiton
    {
    public:
        // Public Types
        using Key = std::tuple<Args ...>;
        using KeyPtr = std::shared_ptr<std::tuple<Args ...>>;
        using Value = std::weak_ptr<T>;
        using Map = std::unordered_map<
                        KeyPtr,
                        Value,
                        ArgHasher<Args ...>,
                        KeyEqual<KeyPtr>>;
    private:
        // Private Variables
        static std::mutex lock;
        static Map store;

    public:
        // Public methods
        static std::shared_ptr<T> get(Args && ... args)
        {
            const std::lock_guard<std::mutex> guard(lock);

            // Create a shared_ptr to the key
            // so that we can capture it by value
            // in the value deleter
            auto key = KeyPtr(new Key(args...));
            auto it = store.find(key);

            if(it != store.end()){
                // Promote weak_ptr to shared_ptr
                auto result = it->second.lock();

                // The promoted shared_ptr is valid, return it
                if(result)
                    { return result; }

                // This is a stale entry, remove it from the map
                // before recreating the entry
                store.erase(it);
            }

            auto result = std::shared_ptr<T>(
                new T(std::move(args)...),
                [key](T * ptr) {
                    delete ptr;
                    const std::lock_guard<std::mutex> guard(lock);
                    // Erase entry in store
                    auto it = store.find(key);

                    if(it != store.end())
                        { store.erase(it); }
                });

            std::weak_ptr<T> data(result);
            auto insert_it = store.insert({key, data});

            return result;
        }
    };

    template <typename T, typename ... Args>
    typename Multiton<T, Args...>::Map Multiton<T, Args...>::store;

    template <typename T, typename ... Args>
    std::mutex Multiton<T, Args...>::lock;


    template <typename T, typename ... Args>
    std::shared_ptr<T> multiton_get(Args && ... args)
    {
        return Multiton<T, Args...>::get(std::forward<Args>(args)...);
    }
}
#endif