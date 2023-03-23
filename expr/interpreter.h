#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <limits>
#include <memory>
#include <stdexcept>
#include <string>
#include <tuple>
#include <vector>
#include "VapourSynth4.h"
#include "VSHelper4.h"
#include "expr.h"
#include "../kernel/umHalf.h"
#include "../kernel/cpufeatures.h"
#include "../kernel/cpulevel.h"

using namespace expr;

class ExprInterpreter {
    const ExprInstruction *bytecode;
    size_t numInsns;
    std::vector<float> registers;
    const VSVideoInfo **srcFormats;

    template <class T>
    static T clamp_int(float x, int depth = std::numeric_limits<T>::digits)
    {
        float maxval = static_cast<float>((1U << depth) - 1);
        return static_cast<T>(std::lrint(std::min(std::max(x, static_cast<float>(std::numeric_limits<T>::min())), maxval)));
    }

    static float bool2float(bool x) { return x ? 1.0f : 0.0f; }
    static bool float2bool(float x) { return x > 0.0f; }
public:
    ExprInterpreter(const ExprInstruction *bytecode, size_t numInsns, const VSVideoInfo **srcFormats);

    void eval(const uint8_t * const *srcp, uint8_t *dstp, const float *consts, int x);
};