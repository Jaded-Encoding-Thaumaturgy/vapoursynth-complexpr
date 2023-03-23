/*
* Copyright (c) 2013-2020 Fredrik Mellbin
*
* This file is part of VapourSynth.
*
* VapourSynth is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* VapourSynth is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with VapourSynth; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*/

#include <cassert>
#include "jitcompiler.h"

namespace expr {

void ExprCompiler::addInstruction(const ExprInstruction &insn)
{
    if (insn.op.type == ExprOpType::CONSTANTI) {
        ExprInstruction newInsn{ { ExprOpType::CONSTANTF, (float)insn.op.imm.u } };
        newInsn.dst = insn.dst;
        loadConst(newInsn);
        return;
    }

    switch (insn.op.type) {
    case ExprOpType::MEM_LOAD_U8: load8(insn); break;
    case ExprOpType::MEM_LOAD_U16: load16(insn); break;
    case ExprOpType::MEM_LOAD_U32: load32(insn); break;
    case ExprOpType::MEM_LOAD_F16: loadF16(insn); break;
    case ExprOpType::MEM_LOAD_F32: loadF32(insn); break;
    case ExprOpType::MEM_LOAD_VAR: loadConstVar(insn); break;
    case ExprOpType::CONSTANTF: loadConst(insn); break;
    case ExprOpType::MEM_STORE_U8: store8(insn); break;
    case ExprOpType::MEM_STORE_U16: store16(insn); break;
    case ExprOpType::MEM_STORE_U32: store32(insn); break;
    case ExprOpType::MEM_STORE_F16: storeF16(insn); break;
    case ExprOpType::MEM_STORE_F32: storeF32(insn); break;
    case ExprOpType::ADD: add(insn); break;
    case ExprOpType::SUB: sub(insn); break;
    case ExprOpType::MUL: mul(insn); break;
    case ExprOpType::DIV: div(insn); break;
    case ExprOpType::MOD: mod(insn); break;
    case ExprOpType::FMA: fma(insn); break;
    case ExprOpType::MAX: max(insn); break;
    case ExprOpType::MIN: min(insn); break;
    case ExprOpType::CLAMP: clamp(insn); break;
    case ExprOpType::SQRT: sqrt(insn); break;
    case ExprOpType::ABS: abs(insn); break;
    case ExprOpType::NEG: neg(insn); break;
    case ExprOpType::NOT: not_(insn); break;
    case ExprOpType::AND: and_(insn); break;
    case ExprOpType::OR: or_(insn); break;
    case ExprOpType::XOR: xor_(insn); break;
    case ExprOpType::CMP: cmp(insn); break;
    case ExprOpType::TRUNC: trunc(insn); break;
    case ExprOpType::ROUND: round(insn); break;
    case ExprOpType::FLOOR: floor(insn); break;
    case ExprOpType::CEIL: ceil(insn); break;
    case ExprOpType::TERNARY: ternary(insn); break;
    case ExprOpType::EXP: exp(insn); break;
    case ExprOpType::LOG: log(insn); break;
    case ExprOpType::POW: pow(insn); break;
    case ExprOpType::SIN: sin(insn); break;
    case ExprOpType::COS: cos(insn); break;
    default: assert(false && "illegal opcode"); break;
    }
}

std::pair<ExprCompiler::ProcessLineProc, size_t> compile_jit(const ExprInstruction *bytecode, size_t numInsns, int numInputs, int cpulevel, intptr_t *niter, bool unsafe)
{
	std::unique_ptr<ExprCompiler> compiler;

#ifdef VS_TARGET_CPU_X86
	if (getCPUFeatures()->avx2 && cpulevel >= VS_CPU_LEVEL_AVX2)
		compiler = make_ymm_compiler(numInputs, niter, unsafe);
	else
		compiler = make_xmm_compiler(numInputs, niter, unsafe);
#endif

	if (!compiler)
		return{};

	compiler->addInstructions(bytecode, numInsns);

	return compiler->getCode();
}

} // namespace expr
