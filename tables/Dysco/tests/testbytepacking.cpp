#include "../bytepacker.h"
#include "../uvector.h"

#include <boost/test/unit_test.hpp>

#include <sstream>

using namespace dyscostman;

BOOST_AUTO_TEST_SUITE(bytepacking)

template<typename T>
void assertEqualArray(const T *expected, const T *actual, size_t size, const std::string& msg)
{
	for(size_t i=0; i!=size; ++i)
	{
		if(expected[i] != actual[i])
		{
			std::ostringstream str;
			str << "Failed assertEqualArray() for test " << msg << ": Expected: {" << (int) expected[0];
			for(size_t j=1; j!=size; ++j)
				str << ", " << (int) expected[j];
			str << "} Actual: {" << (int) actual[0];
			for(size_t j=1; j!=size; ++j)
				str << ", " << (int) actual[j];
			str << "}";
			BOOST_REQUIRE_MESSAGE(expected[i] == actual[i], str.str());
		}
	}
	BOOST_CHECK(expected[0] == actual[0]);
}

BOOST_AUTO_TEST_CASE( under_and_overflow )
{
	const size_t NBITSIZES=8;
	size_t bitSizes[NBITSIZES] = {2, 3, 4, 6, 8, 10, 12, 16};
	for(size_t i=0; i!=NBITSIZES; ++i)
	{
		for(size_t s=0; s!=12; ++s)
		{
			unsigned arr[12] = { 1, 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31 };
			unsigned expected[13] = {37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37};
			for(size_t x=0;x!=12;++x)
				arr[x] &= (1<<bitSizes[i]) - 1;
	
			unsigned result[13] = { 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37};
			unsigned char packed[24], packedOr[24];
			memset(packed, 39, 24);
			memset(packedOr, 39, 24);
			BytePacker::pack(bitSizes[i], packed, arr, s);
			BytePacker::unpack(bitSizes[i], result, packed, s);
			for(size_t x=0; x!=s; ++x)
				expected[x] = arr[x];
			std::stringstream msg;
			msg<< "result of pack+unpack (l=" << s << ",bits=" << bitSizes[i] << ')';
			assertEqualArray(expected, result, 13, msg.str());
			size_t packedSize = BytePacker::bufferSize(s, bitSizes[i]);
			assertEqualArray(packedOr, &packed[packedSize], 24-packedSize, "packed not overwritten past length");
		}
		
		unsigned arr2[15];
		for(size_t x=0; x!=15; ++x)
			arr2[x] = (1<<bitSizes[i]) - 1;
		unsigned char packed[30];
		memset(packed, 0, 30);
		unsigned result[15];
		memset(result, 0, 15*sizeof(unsigned));
		BytePacker::pack(bitSizes[i], packed, arr2, 15);
		BytePacker::unpack(bitSizes[i], result, packed, 15);
		std::stringstream msg;
		msg<< "all bits set (bits=" << bitSizes[i] << ')';
		assertEqualArray(arr2, result, 15, msg.str());
	}
	
}


void testSingle(const ao::uvector<unsigned int>& data, int bitCount)
{
	int limit = (1<<bitCount);
	ao::uvector<unsigned int> trimmedData(data.size()), restoredData(data.size());
	for(size_t i=0; i!=data.size(); ++i)
		trimmedData[i] = data[i] % limit;
	ao::uvector<unsigned char> buffer(BytePacker::bufferSize(trimmedData.size(), bitCount), 0);
	
// 	std::cout << "Tested array: [" << trimmedData[0];
// 	for(size_t i=1; i!=std::min<size_t>(trimmedData.size(), 32); ++i)
// 		std::cout << ", " << trimmedData[i];
// 	if(trimmedData.size() > 32) std::cout << ", ...";
// 	std::cout << "]\n";
	
	BytePacker::pack(bitCount, buffer.data(), trimmedData.data(), trimmedData.size());
	BytePacker::unpack(bitCount, restoredData.data(), buffer.data(), restoredData.size());
	
	for(size_t i=0; i!=trimmedData.size(); ++i)
	{
		BOOST_REQUIRE_MESSAGE(restoredData[i] == trimmedData[i], "data[" << i << "] was incorrectly unpacked: was " << restoredData[i] << ", should be " << trimmedData[i]);
	}
}

void testCombinations(const ao::uvector<unsigned int>& data, int bitCount)
{
	for(size_t dataSize=1; dataSize!=std::min<size_t>(32u, data.size()); ++dataSize)
	{
		ao::uvector<unsigned int> resizedData(data.begin(), data.begin()+dataSize);
		testSingle(resizedData, bitCount);
	}
	testSingle(data, bitCount);
	for(size_t dataSize=1; dataSize!=std::min<size_t>(32u, data.size()-1); ++dataSize)
	{
		ao::uvector<unsigned int> resizedData(data.begin()+1, data.begin()+dataSize+1);
		testSingle(resizedData, bitCount);
	}
	ao::uvector<unsigned int> resizedData(data.begin()+1, data.end());
	testSingle(resizedData, bitCount);
}

const int bitrates[] = {2, 3, 4, 6, 8, 10, 12, 16};

BOOST_AUTO_TEST_CASE( pack_unpack )
{
	for(int sample : bitrates)
	{
		ao::uvector<unsigned int> testArray{1337, 2, 100, 0};
		for(int i=0; i!=1000; ++i)
		{
			testArray.push_back(i);
			testArray.push_back(i*37);
			testArray.push_back(i*20000);
		}
		testCombinations(testArray, sample);
	}
}

BOOST_AUTO_TEST_SUITE_END()
