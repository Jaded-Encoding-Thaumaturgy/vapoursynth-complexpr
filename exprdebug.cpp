/*
* Copyright (c) 2012-2020 Fredrik Mellbin
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

#include <iomanip>
#include <iostream>
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
#include "kernel/cpufeatures.h"
#include "expr/expr.h"
#include "expr/jitcompiler.h"
#include "kernel/cpulevel.h"

#ifdef VS_TARGET_OS_WINDOWS
#include <windows.h>
#else
#include <sys/mman.h>
#endif

using namespace expr;
using namespace vsh;

namespace {

static const char *op_names[] = {
	"loadu8", "loadu16", "loadf16", "loadf32", "constant",
	"storeu8", "storeu16", "storef16", "storef32",
	"add", "sub", "mul", "div", "fma", "sqrt", "abs", "neg", "max", "min", "cmp",
	"and", "or", "xor", "not",
	"exp", "log", "pow", "sin", "cos",
	"ternary",
	"mux",
	"dup", "swap",
};

static_assert(sizeof(op_names) / sizeof(op_names[0]) == static_cast<size_t>(ExprOpType::SWAP) + 1, "");

static const char *cmp_names[8] = {
	"EQ", "LT", "LE", "?", "NEQ", "NLT", "NLE", "?"
};

static void VS_CC exprDebugCreate(const VSMap *in, VSMap *out, void *userData, VSCore *core, const VSAPI *vsapi) {
    int err;

    try {
        int numInputs = 1024;

        bool optimize = !!vsapi->mapGetInt(in, "optimize", 0, &err);
        if (err) {
            optimize = false;
        }

        VSVideoInfo userVi{};

        int format = vsapi->mapGetIntSaturated(in, "format", 0, &err);
        if (err) {
            format = VSPresetFormat::pfGray8;
        }

        VSVideoFormat f;
        if (vsapi->getVideoFormatByID(&f, format, core) && f.colorFamily != cfUndefined) {
            vsapi->queryVideoFormat(&userVi.format, f.colorFamily, f.sampleType, f.bitsPerSample, f.subSamplingW, f.subSamplingH, core);
        }

        const VSVideoInfo * vi[numInputs] = {};
        for (int i = 0; i < numInputs; i++) {
            vi[i] = &userVi;
        }

        int nexpr = vsapi->mapNumElements(in, "expr");

        std::string expr[3];
        for (int i = 0; i < nexpr; i++) {
            expr[i] = vsapi->mapGetData(in, "expr", i, nullptr);
        }
        for (int i = nexpr; i < 3; ++i) {
            expr[i] = expr[nexpr - 1];
        }

        for (int i = 0; i < f.numPlanes; i++) {
            if (expr[i].empty())
                continue;

            std::vector<ExprInstruction> bytecode = compile(expr[i], vi, numInputs, userVi, optimize);

            std::ostringstream asmCode;

            for (auto &insn : bytecode) {
                asmCode << std::setw(12) << std::left << op_names[static_cast<size_t>(insn.op.type)];

                if (insn.op.type == ExprOpType::MEM_STORE_U8 || insn.op.type == ExprOpType::MEM_STORE_U16 || insn.op.type == ExprOpType::MEM_STORE_F16 || insn.op.type == ExprOpType::MEM_STORE_F32) {
                    asmCode << " r" << insn.src1 << '\n';
                    continue;
                }

                asmCode << " r" << insn.dst;

                if (insn.src1 >= 0)
                    asmCode << ",r" << insn.src1;
                if (insn.src2 >= 0)
                    asmCode << ",r" << insn.src2;
                if (insn.src3 >= 0)
                    asmCode << ",r" << insn.src3;

                switch (insn.op.type) {
                case ExprOpType::MEM_LOAD_U8:
                case ExprOpType::MEM_LOAD_U16:
                case ExprOpType::MEM_LOAD_F16:
                case ExprOpType::MEM_LOAD_F32:
                    asmCode << ',' << clipNamePrefix << insn.op.imm.u;
                    break;
                case ExprOpType::CONSTANT:
                    asmCode << ',' << insn.op.imm.f;
                    break;
                case ExprOpType::FMA:
                    asmCode << "," << insn.op.imm.u;
                    break;
                case ExprOpType::CMP:
                    asmCode << ',' << cmp_names[insn.op.imm.u];
                    break;
                }

                asmCode << '\n';
            }

            std::string asmCodeString = asmCode.str();

            vsapi->mapSetData(out, "asm", asmCodeString.c_str(), asmCodeString.size(), VSDataTypeHint::dtUtf8, maAppend);
        }
    } catch (std::runtime_error &e) {
        vsapi->mapSetError(out, (std::string{ "ExprDebug: " } + e.what()).c_str());
        return;
    }
}

} // namespace


//////////////////////////////////////////
// Init

void exprDebugInitialize(VSPlugin *plugin, const VSPLUGINAPI *vspapi) {
    vspapi->registerFunction("ExprDebug", "expr:data[];format:int:opt;optimize:int:opt;", "asm:data[];", exprDebugCreate, nullptr, plugin);
}
