#include <cmath>
#include <iostream>
#include <fstream>
#include <random>

#include "../stochasticencoder.h"

#include <boost/test/unit_test.hpp>

using namespace dyscostman;

BOOST_AUTO_TEST_SUITE(dithering)

std::unique_ptr<StochasticEncoder<float>> MakeEncoder()
{
	//GausEncoder<float> encoder(256, 1.0, !uniform);
	return std::unique_ptr<StochasticEncoder<float>>(new StochasticEncoder<float>(StochasticEncoder<float>::StudentTEncoder(256, 1.0, 1.0)));
}

BOOST_AUTO_TEST_CASE( avg_deviation )
{
	std::unique_ptr<StochasticEncoder<float>> encoder = MakeEncoder();
	
	constexpr size_t N=12;
	const double values[] = { -(M_PI), -(M_E), -(M_SQRT2), -1.0, -0.01, 0.0, 0.01, 1.0, M_SQRT2, M_E, M_PI, std::numeric_limits<double>::infinity() };
	long double sumsUndithered[N];
	long double sumsDithered[N];
	for(size_t j=0; j!=N; ++j) {
		sumsUndithered[j] = 0.0;
		sumsDithered[j] = 0.0;
	}
	std::mt19937 mt;
	std::uniform_int_distribution<unsigned> distribution = encoder->GetDitherDistribution();
// 	std::cout << "Input: ";
// 	for(size_t j=0; j!=N; ++j) std::cout << ' ' << values[j];
// 	std::cout << "\nDecoded without dithering: ";
// 	for(size_t j=0; j!=N; ++j) std::cout << ' ' << encoder.Decode(encoder.Encode(values[j]));
// 	std::cout << "\nDecoded with dithering: ";
// 	for(size_t j=0; j!=N; ++j) std::cout << ' ' << encoder.Decode(encoder.EncodeWithDithering(values[j], distribution(mt)));
// 	std::cout << '\n';
	ao::uvector<double> previousError(N, 10.0);
	for(size_t i=0; i!=1000000; ++i)
	{
		for(size_t j=0; j!=N; ++j)
		{
			sumsUndithered[j] += encoder->Decode(encoder->Encode(values[j]));
			sumsDithered[j] += encoder->Decode(encoder->EncodeWithDithering(values[j], distribution(mt)));
		}
		if(i==99 || i==9999 || i==999999)
		{
			for(size_t j=0; j!=N; ++j)
			{
				BOOST_CHECK_EQUAL(std::isfinite(values[j]), std::isfinite(sumsUndithered[j]));
				
				BOOST_CHECK_EQUAL(std::isfinite(values[j]), std::isfinite(sumsDithered[j]));
				
				if(std::isfinite(values[j]))
				{
					double unditheredError = std::fabs((sumsUndithered[j]/double(i))-values[j]);
					BOOST_CHECK_LE(unditheredError, std::fabs(values[j]));
					double ditheredError = std::fabs((sumsDithered[j]/double(i))-values[j]);
					if(values[j] == 0.0)
					{
						BOOST_CHECK_LE(ditheredError, 0.0);
					}
					else {
						BOOST_CHECK_LT(ditheredError, unditheredError);
						BOOST_CHECK_LT(ditheredError, previousError[j]);
					}
					previousError[j] = ditheredError;
				}
			}
		}
	}
}

BOOST_AUTO_TEST_SUITE_END()
