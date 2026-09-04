#include <cmath>
#include <algorithm>

// =======================================================================
// PHID: Normal distribution CDF (ported from Fortran)
// =======================================================================
double phid(double z)
{
    static const double P0 = 220.2068679123761,
                        P1 = 221.2135961699311,
                        P2 = 112.0792914978709,
                        P3 = 33.9128660788300,
                        P4 = 6.37396220353165,
                        P5 = 0.700383064436881,
                        P6 = 0.03526249659989109;

    static const double Q0 = 440.4137358247522,
                        Q1 = 793.8265125199484,
                        Q2 = 637.3336333788311,
                        Q3 = 296.5642487796737,
                        Q4 = 86.78073220294608,
                        Q5 = 16.06417757920695,
                        Q6 = 1.755667163182642,
                        Q7 = 0.08838834764831844;

    static const double ROOTPI = 2.506628274631001;
    static const double CUTOFF = 7.071067811865475;

    double zabs = std::fabs(z);
    double p;

    if (zabs > 37.0) {
        p = 0.0;
    } else {
        double expntl = std::exp(-zabs * zabs / 2.0);

        if (zabs < CUTOFF) {
            p = expntl * ((((((P6*zabs + P5)*zabs + P4)*zabs +
                             P3)*zabs + P2)*zabs + P1)*zabs + P0) /
                (((((((Q7*zabs + Q6)*zabs + Q5)*zabs + Q4)*zabs +
                    Q3)*zabs + Q2)*zabs + Q1)*zabs + Q0);
        } else {
            p = expntl / (zabs + 1.0/(zabs + 2.0/(zabs + 3.0/(zabs +
                          4.0/(zabs + 0.65))))) / ROOTPI;
        }
    }
    if (z > 0.0) p = 1.0 - p;
    return p;
}

// =======================================================================
// BVND: Bivariate normal probability (Alan Genz)
// =======================================================================
double bvnd(double dh, double dk, double r)
{
    static const double ZERO  = 0.0;
    static const double TWOPI = 6.283185307179586;

    // Gauss-Legendre abscissas and weights
    static const double X[3][10] = {
        {-0.9324695142031522, -0.6612093864662647, -0.2386191860831970},
        {-0.9815606342467191, -0.9041172563704750, -0.7699026741943050,
         -0.5873179542866171, -0.3678314989981802, -0.1252334085114692},
        {-0.9931285991850949, -0.9639719272779138, -0.9122344282513259,
         -0.8391169718222188, -0.7463319064601508, -0.6360536807265150,
         -0.5108670019508271, -0.3737060887154196, -0.2277858511416451,
         -0.07652652113349733}
    };

    static const double W[3][10] = {
        {0.1713244923791705, 0.3607615730481384, 0.4679139345726904},
        {0.04717533638651177, 0.1069393259953183, 0.1600783285433464,
         0.2031674267230659, 0.2334925365383547, 0.2491470458134029},
        {0.01761400713915212, 0.04060142980038694, 0.06267204833410906,
         0.08327674157670475, 0.1019301198172404, 0.1181945319615184,
         0.1316886384491766, 0.1420961093183821, 0.1491729864726037,
         0.1527533871307259}
    };

    int ng, lg;
    if (std::fabs(r) < 0.3)      { ng = 0; lg = 3; }
    else if (std::fabs(r) < 0.75){ ng = 1; lg = 6; }
    else                         { ng = 2; lg = 10; }

    double h = dh, k = dk;
    double hk = h * k;
    double bvn = 0.0;

    if (std::fabs(r) < 0.925) {
        double hs = (h*h + k*k) / 2.0;
        double asr = std::asin(r);
        for (int i = 0; i < lg; ++i) {
            for (int is = -1; is <= 1; is += 2) {
                double sn = std::sin(asr * (is * X[ng][i] + 1.0) / 2.0);
                bvn += W[ng][i] * std::exp((sn * hk - hs) / (1.0 - sn * sn));
            }
        }
        bvn = bvn * asr / (2.0 * TWOPI) + phid(-h) * phid(-k);
    } else {
        if (r < 0.0) {
            k = -k;
            hk = -hk;
        }
        if (std::fabs(r) < 1.0) {
            double as_ = (1.0 - r)*(1.0 + r);
            double a = std::sqrt(as_);
            double bs = (h - k)*(h - k);
            double c = (4.0 - hk)/8.0;
            double d = (12.0 - hk)/16.0;
            double asr_val = -(bs/as_ + hk)/2.0;

            if (asr_val > -100.0)
                bvn = a * std::exp(asr_val) *
                      (1.0 - c*(bs - as_)*(1.0 - d*bs/5.0)/3.0 + c*d*as_*as_/5.0);

            if (-hk < 100.0) {
                double b = std::sqrt(bs);
                bvn -= std::exp(-hk/2.0) * std::sqrt(TWOPI) * phid(-b/a) * b *
                       (1.0 - c*bs*(1.0 - d*bs/5.0)/3.0);
            }

            a /= 2.0;
            for (int i = 0; i < lg; ++i) {
                for (int is = -1; is <= 1; is += 2) {
                    double xs = a * (is * X[ng][i] + 1.0);
                    xs *= xs;
                    double rs = std::sqrt(1.0 - xs);
                    asr_val = -(bs/xs + hk)/2.0;
                    if (asr_val > -100.0) {
                        bvn += a * W[ng][i] * std::exp(asr_val) *
                               (std::exp(-hk*(1.0-rs)/(2.0*(1.0+rs)))/rs -
                                (1.0 + c*xs*(1.0 + d*xs)));
                    }
                }
            }
            bvn = -bvn / TWOPI;
        }
        if (r > 0.0)
            bvn += phid(-std::max(h, k));
        else
            bvn = -bvn + std::max(ZERO, phid(-h) - phid(-k));
    }
    return bvn;
}

// =======================================================================
// 3-level Van Vleck functions
// =======================================================================
double vvr3(double mux, double muy, double v1x, double v1y, double rho)
{
    double rt2 = std::sqrt(2.0);
    auto L = [&](double h, double k, double r){ return bvnd(h, k, r); };

    return 0.5 * (std::erf((-mux + v1x)/rt2) - std::erf((mux + v1x)/rt2) +
                  std::erf((-muy + v1y)/rt2) - std::erf((muy + v1y)/rt2)) +
           L(-mux-v1x, -muy-v1y, rho) + L(-mux-v1x, -muy+v1y, rho) +
           L(-mux+v1x, -muy-v1y, rho) + L(-mux+v1x, -muy+v1y, rho) - 1.0;
}

double vvr3auto(double mux, double v1x, double rho)
{
    double rt2 = std::sqrt(2.0);
    auto L = [&](double h, double k, double r){ return bvnd(h, k, r); };

    return std::erf((-mux + v1x)/rt2) - std::erf((mux + v1x)/rt2) +
           L(-mux-v1x, -mux-v1x, rho) +
           2.0 * L(-mux-v1x, -mux+v1x, rho) +
           L(-mux+v1x, -mux+v1x, rho) - 1.0;
}

double vvr3zmean(double v1x, double v1y, double rho)
{
    auto L = [&](double h, double k, double r){ return bvnd(h, k, r); };
    return L(-v1x,-v1y,rho) + L(-v1x, v1y,rho) +
           L( v1x,-v1y,rho) + L( v1x, v1y,rho) - 1.0;
}

double vvr3zauto(double v, double rho)
{
    auto L = [&](double h, double k, double r){ return bvnd(h, k, r); };
    return L(-v,-v,rho) + L(-v, v,rho) +
           L( v,-v,rho) + L( v, v,rho) - 1.0;
}

// =======================================================================
// 9-level Van Vleck functions (fully expanded)
// =======================================================================
double vvr9(double mux, double muy, double v1x, double v1y, double rho)
{
    double rt2 = std::sqrt(2.0);
    auto L = [&](double h, double k, double r){ return bvnd(h, k, r); };

    double erf_part = -16.0 + 2.0 * (
        -std::erf((mux-7*v1x)/rt2) -std::erf((mux-5*v1x)/rt2) -
        std::erf((mux-3*v1x)/rt2) + std::erf((-mux+v1x)/rt2) -
        std::erf((mux+v1x)/rt2) -std::erf((mux+3*v1x)/rt2) -
        std::erf((mux+5*v1x)/rt2) -std::erf((mux+7*v1x)/rt2) -
        std::erf((muy-7*v1y)/rt2) -std::erf((muy-5*v1y)/rt2) -
        std::erf((muy-3*v1y)/rt2) + std::erf((-muy+v1y)/rt2) -
        std::erf((muy+v1y)/rt2) -std::erf((muy+3*v1y)/rt2) -
        std::erf((muy+5*v1y)/rt2) -std::erf((muy+7*v1y)/rt2));

    double lsum = 0.0;
    int levels[8] = {-7,-5,-3,-1,1,3,5,7};

    for (int i = 0; i < 8; ++i) {
        double hx = -mux + levels[i] * v1x;
        for (int j = 0; j < 8; ++j) {
            double ky = -muy + levels[j] * v1y;
            lsum += L(hx, ky, rho);
        }
    }

    return erf_part + lsum;
}

double vvr9auto(double mux, double v1x, double rho)
{
    double rt2 = std::sqrt(2.0);
    auto L = [&](double h, double k, double r){ return bvnd(h, k, r); };

    double erf_part = -16.0 + 4.0 * (
        -std::erf((mux-7*v1x)/rt2) -std::erf((mux-5*v1x)/rt2) -
        std::erf((mux-3*v1x)/rt2) + std::erf((-mux+v1x)/rt2) -
        std::erf((mux+v1x)/rt2) -std::erf((mux+3*v1x)/rt2) -
        std::erf((mux+5*v1x)/rt2) -std::erf((mux+7*v1x)/rt2));

    double lsum =
        L(-mux-7*v1x, -mux-7*v1x, rho) + L(-mux-5*v1x, -mux-5*v1x, rho) +
        L(-mux-3*v1x, -mux-3*v1x, rho) + L(-mux-v1x, -mux-v1x, rho) +
        L(-mux+v1x, -mux+v1x, rho) + L(-mux+3*v1x, -mux+3*v1x, rho) +
        L(-mux+5*v1x, -mux+5*v1x, rho) + L(-mux+7*v1x, -mux+7*v1x, rho);

    lsum += 2.0 * (
        L(-mux-7*v1x,-mux-5*v1x,rho) + L(-mux-7*v1x,-mux-3*v1x,rho) +
        L(-mux-7*v1x,-mux-v1x,rho) + L(-mux-7*v1x,-mux+v1x,rho) +
        L(-mux-7*v1x,-mux+3*v1x,rho) + L(-mux-7*v1x,-mux+5*v1x,rho) +
        L(-mux-7*v1x,-mux+7*v1x,rho) + L(-mux-5*v1x,-mux-3*v1x,rho) +
        L(-mux-5*v1x,-mux-v1x,rho) + L(-mux-5*v1x,-mux+v1x,rho) +
        L(-mux-5*v1x,-mux+3*v1x,rho) + L(-mux-5*v1x,-mux+5*v1x,rho) +
        L(-mux-5*v1x,-mux+7*v1x,rho) + L(-mux-3*v1x,-mux-v1x,rho) +
        L(-mux-3*v1x,-mux+v1x,rho) + L(-mux-3*v1x,-mux+3*v1x,rho) +
        L(-mux-3*v1x,-mux+5*v1x,rho) + L(-mux-3*v1x,-mux+7*v1x,rho) +
        L(-mux-v1x,-mux+v1x,rho) + L(-mux-v1x,-mux+3*v1x,rho) +
        L(-mux-v1x,-mux+5*v1x,rho) + L(-mux-v1x,-mux+7*v1x,rho) +
        L(-mux+v1x,-mux+3*v1x,rho) + L(-mux+v1x,-mux+5*v1x,rho) +
        L(-mux+v1x,-mux+7*v1x,rho) + L(-mux+3*v1x,-mux+5*v1x,rho) +
        L(-mux+3*v1x,-mux+7*v1x,rho) + L(-mux+7*v1x,-mux+5*v1x,rho));

    return erf_part + lsum;
}

double vvr9zmean(double v1x, double v1y, double rho)
{
    double rt2 = std::sqrt(2.0);
    auto L = [&](double h, double k, double r){ return bvnd(h, k, r); };

    double term = -12.0 - std::erfc(v1x / rt2) / 2.0;

    double lsum =
        L(-7*v1x,-7*v1y,rho) + L(-7*v1x,-5*v1y,rho) + L(-7*v1x,-3*v1y,rho) +
        L(-7*v1x,-v1y,rho) + L(-7*v1x, v1y,rho) + L(-7*v1x, 3*v1y,rho) +
        L(-7*v1x, 5*v1y,rho) + L(-7*v1x, 7*v1y,rho) +

        L(-5*v1x,-7*v1y,rho) + L(-5*v1x,-5*v1y,rho) + L(-5*v1x,-3*v1y,rho) +
        L(-5*v1x,-v1y,rho) + L(-5*v1x, v1y,rho) + L(-5*v1x, 3*v1y,rho) +
        L(-5*v1x, 5*v1y,rho) + L(-5*v1x, 7*v1y,rho) +

        L(-3*v1x,-7*v1y,rho) + L(-3*v1x,-5*v1y,rho) + L(-3*v1x,-3*v1y,rho) +
        L(-3*v1x,-v1y,rho) + L(-3*v1x, v1y,rho) + L(-3*v1x, 3*v1y,rho) +
        L(-3*v1x, 5*v1y,rho) + L(-3*v1x, 7*v1y,rho) +

        L(3*v1x,-7*v1y,rho) + L(3*v1x,-5*v1y,rho) + L(3*v1x,-3*v1y,rho) +
        L(3*v1x,-v1y,rho) + L(3*v1x, v1y,rho) + L(3*v1x, 3*v1y,rho) +
        L(3*v1x, 5*v1y,rho) + L(3*v1x, 7*v1y,rho) +

        L(5*v1x,-7*v1y,rho) + L(5*v1x,-5*v1y,rho) + L(5*v1x,-3*v1y,rho) +
        L(5*v1x,-v1y,rho) + L(5*v1x, v1y,rho) + L(5*v1x, 3*v1y,rho) +
        L(5*v1x, 5*v1y,rho) + L(5*v1x, 7*v1y,rho) +

        L(7*v1x,-7*v1y,rho) + L(7*v1x,-5*v1y,rho) + L(7*v1x,-3*v1y,rho) +
        L(7*v1x,-v1y,rho) + L(7*v1x, v1y,rho) + L(7*v1x, 3*v1y,rho) +
        L(7*v1x, 5*v1y,rho) + L(7*v1x, 7*v1y,rho);

    // Special cross terms around zero
    lsum += -L(v1x,-7*v1y,-rho) + L(v1x,-7*v1y,rho)
          - L(v1x,-5*v1y,-rho) + L(v1x,-5*v1y,rho)
          - L(v1x,-3*v1y,-rho) + L(v1x,-3*v1y,rho)
          + L(v1x,-v1y,rho) - L(v1x, v1y,-rho)
          + 2.0 * L(v1x, v1y,rho)
          - L(v1x, 3*v1y,-rho) + L(v1x, 3*v1y,rho)
          - L(v1x, 5*v1y,-rho) + L(v1x, 5*v1y,rho)
          - L(v1x, 7*v1y,-rho) + L(v1x, 7*v1y,rho);

    return term + lsum;
}

double vvr9zauto(double v, double rho)
{
    double rt2 = std::sqrt(2.0);
    auto L = [&](double h, double k, double r){ return bvnd(h, k, r); };

    double term = -15.0 + std::erfc(3.0*v/rt2) + std::erfc(5.0*v/rt2) + std::erfc(7.0*v/rt2);

    double lsum =
        L(-7*v,-7*v,rho) + L(-5*v,-5*v,rho) + L(-3*v,-3*v,rho) +
        L( 3*v, 3*v,rho) + L( 5*v, 5*v,rho) + L( 7*v, 7*v,rho);

    lsum += 2.0 * (
        L(-7*v,-5*v,rho) + L(-7*v,-3*v,rho) + L(-7*v,-v,rho) + L(-7*v,v,rho) +
        L(-7*v,3*v,rho) + L(-7*v,5*v,rho) + L(-7*v,7*v,rho) +
        L(-5*v,-3*v,rho) + L(-5*v,-v,rho) + L(-5*v,v,rho) +
        L(-5*v,3*v,rho) + L(-5*v,5*v,rho) + L(-5*v,7*v,rho) +
        L(-3*v,-v,rho) + L(-3*v,v,rho) +
        L(-3*v,3*v,rho) + L(-3*v,5*v,rho) + L(-3*v,7*v,rho) -
        L(v,v,-rho) + L(v,v,rho) - L(v,3*v,-rho) + L(v,3*v,rho) -
        L(v,5*v,-rho) + L(v,5*v,rho) - L(v,7*v,-rho) + L(v,7*v,rho) +
        L(3*v,5*v,rho) + L(3*v,7*v,rho) + L(5*v,7*v,rho));

    return term + lsum;
}
