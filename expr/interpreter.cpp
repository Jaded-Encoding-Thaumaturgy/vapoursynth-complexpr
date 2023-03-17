#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <limits>
#include <memory>
#include <stdexcept>
#include <string>
#include <tuple>
#include <vector>
#include "interpreter.h"

ExprInterpreter::ExprInterpreter(const ExprInstruction *bytecode, size_t numInsns) : bytecode(bytecode), numInsns(numInsns)
{
    int maxreg = 0;
    for (size_t i = 0; i < numInsns; ++i) {
        maxreg = std::max(maxreg, bytecode[i].dst);
    }
    registers.resize(maxreg + 1);
}

void ExprInterpreter::eval(const uint8_t * const *srcp, uint8_t *dstp, const float *consts, int x)
{
    for (size_t i = 0; i < numInsns; ++i) {
        const ExprInstruction &insn = bytecode[i];

#define SRC1 registers[insn.src1]
#define SRC2 registers[insn.src2]
#define SRC3 registers[insn.src3]
#define DST registers[insn.dst]
        switch (insn.op.type) {
        case ExprOpType::MEM_LOAD_U8: DST = reinterpret_cast<const uint8_t *>(srcp[insn.op.imm.u])[x]; break;
        case ExprOpType::MEM_LOAD_U16: DST = reinterpret_cast<const uint16_t *>(srcp[insn.op.imm.u])[x]; break;
        case ExprOpType::MEM_LOAD_U32: DST = reinterpret_cast<const uint32_t *>(srcp[insn.op.imm.u])[x]; break;
        case ExprOpType::MEM_LOAD_F16: DST = reinterpret_cast<const float16 *>(srcp[insn.op.imm.u])[x]; break;
        case ExprOpType::MEM_LOAD_F32: DST = reinterpret_cast<const float *>(srcp[insn.op.imm.u])[x]; break;
        case ExprOpType::MEM_LOAD_VAR:
            switch (static_cast<MemoryVar>(insn.op.imm.u)) {
            case MemoryVar::VAR_X: DST = x; break;
            default:
                DST = consts[insn.op.imm.u]; break;
            }
        case ExprOpType::CONSTANTF: DST = insn.op.imm.f; break;
        case ExprOpType::CONSTANTI: DST = (float)insn.op.imm.u; break;  // AAAAA
        case ExprOpType::ADD: DST = SRC1 + SRC2; break;
        case ExprOpType::SUB: DST = SRC1 - SRC2; break;
        case ExprOpType::MUL: DST = SRC1 * SRC2; break;
        case ExprOpType::DIV: DST = SRC1 / SRC2; break;
        case ExprOpType::FMA:
            switch (static_cast<FMAType>(insn.op.imm.u)) {
            case FMAType::FMADD: DST = SRC2 * SRC3 + SRC1; break;
            case FMAType::FMSUB: DST = SRC2 * SRC3 - SRC1; break;
            case FMAType::FNMADD: DST = -(SRC2 * SRC3) + SRC1; break;
            case FMAType::FNMSUB: DST = -(SRC2 * SRC3) - SRC1; break;
            };
            break;
        case ExprOpType::MAX: DST = std::max(SRC1, SRC2); break;
        case ExprOpType::MIN: DST = std::min(SRC1, SRC2); break;
        case ExprOpType::EXP: DST = std::exp(SRC1); break;
        case ExprOpType::LOG: DST = std::log(SRC1); break;
        case ExprOpType::POW: DST = std::pow(SRC1, SRC2); break;
        case ExprOpType::SQRT: DST = std::sqrt(SRC1); break;
        case ExprOpType::SIN: DST = std::sin(SRC1); break;
        case ExprOpType::COS: DST = std::cos(SRC1); break;
        case ExprOpType::ABS: DST = std::fabs(SRC1); break;
        case ExprOpType::NEG: DST = -SRC1; break;
        case ExprOpType::CMP:
            switch (static_cast<ComparisonType>(insn.op.imm.u)) {
            case ComparisonType::EQ: DST = bool2float(SRC1 == SRC2); break;
            case ComparisonType::LT: DST = bool2float(SRC1 < SRC2); break;
            case ComparisonType::LE: DST = bool2float(SRC1 <= SRC2); break;
            case ComparisonType::NEQ: DST = bool2float(SRC1 != SRC2); break;
            case ComparisonType::NLT: DST = bool2float(SRC1 >= SRC2); break;
            case ComparisonType::NLE: DST = bool2float(SRC1 > SRC2); break;
            }
            break;
        case ExprOpType::TRUNC: DST = std::trunc(SRC1); break;
        case ExprOpType::ROUND: DST = std::round(SRC1); break;
        case ExprOpType::FLOOR: DST = std::floor(SRC1); break;
        case ExprOpType::CEIL: DST = std::ceil(SRC1); break;
        case ExprOpType::TERNARY: DST = float2bool(SRC1) ? SRC2 : SRC3; break;
        case ExprOpType::AND: DST = bool2float((float2bool(SRC1) && float2bool(SRC2))); break;
        case ExprOpType::OR:  DST = bool2float((float2bool(SRC1) || float2bool(SRC2))); break;
        case ExprOpType::XOR: DST = bool2float((float2bool(SRC1) != float2bool(SRC2))); break;
        case ExprOpType::NOT: DST = bool2float(!float2bool(SRC1)); break;
        case ExprOpType::MEM_STORE_U8:  reinterpret_cast<uint8_t *>(dstp)[x] = clamp_int<uint8_t>(SRC1); return;
        case ExprOpType::MEM_STORE_U16: reinterpret_cast<uint16_t *>(dstp)[x] = clamp_int<uint16_t>(SRC1, insn.op.imm.u); return;
        case ExprOpType::MEM_STORE_U32: reinterpret_cast<uint32_t *>(dstp)[x] = clamp_int<uint32_t>(SRC1, insn.op.imm.u); return;
        case ExprOpType::MEM_STORE_F16: reinterpret_cast<float16 *>(dstp)[x] = (float16)SRC1; return;
        case ExprOpType::MEM_STORE_F32: reinterpret_cast<float *>(dstp)[x] = SRC1; return;
        default: fprintf(stderr, "%s", "illegal opcode\n"); std::terminate(); return;
        }
#undef DST
#undef SRC3
#undef SRC2
#undef SRC1
    }
};
