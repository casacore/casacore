#include <casacore/python/Converters/PyNotThreadSafe.h>

// Stack Overflow https://stackoverflow.com/questions/2261858/boostpython-export-custom-exception
static PyObject* casacore::python::createExceptionClass(const char* name, PyObject* baseTypeObj)
{
    using std::string;
    namespace bp = boost::python;

    const string scopeName = bp::extract<string>(bp::scope().attr("__name__"));
    const string qualifiedName0 = scopeName + "." + name;
    PyObject* typeObj = PyErr_NewException(qualifiedName0.c_str(), baseTypeObj, 0);
    if (!typeObj) bp::throw_error_already_set();
    bp::scope().attr(name) = bp::handle<>(bp::borrowed(typeObj));
    return typeObj;
}

static void casacore::python::translateNotThreadSafeError(casacore::NotThreadSafeError const &e)
{
    boost::python::object exc_t(boost::python::handle<>(boost::python::borrowed(casacore::python::NotThreadSafeErrorType)));
    PyErr_SetString(NotThreadSafeErrorType, 
                    "casacore operation not threadsafe - object may not be shared between threads/processes"); // the string is used by print(exception) in python
}

void casacore::python::registerNotThreadSafeException() {
  boost::python::class_<casacore::NotThreadSafeError>
          myCPPExceptionClass("NotThreadSafeError",
                              boost::python::init<>());
  casacore::python::NotThreadSafeErrorType = createExceptionClass("NotThreadSafeError");
  boost::python::register_exception_translator<casacore::NotThreadSafeError>(&translateNotThreadSafeError);
}