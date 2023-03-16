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
#include "expr/expr.h"
#include "expr/jitcompiler.h"
#include "expr/interpreter.h"

#ifdef VS_TARGET_OS_WINDOWS
#include <windows.h>
#else
#include <sys/mman.h>
#endif

using namespace vsh;

struct ExprData {
    std::vector<VSNode *> node;
    VSVideoInfo vi;
    std::vector<ExprInstruction> bytecode[3];
    PlaneOp plane[3];
    int numInputs;
    int alignment;
    ExprCompiler::ProcessLineProc proc[3];
    size_t procSize[3];

    bool gotPtrSizes;
    ptrdiff_t *frame_strides[3];  // all src
    intptr_t *ptroffsets[3];  // first is dst, then src shifted by 1

    ExprData() : node(), vi(), plane(), numInputs(), proc() {}

    ~ExprData() {
        for (int i = 0; i < 3; i++) {
            if (proc[i]) {
#ifdef VS_TARGET_OS_WINDOWS
                VirtualFree((LPVOID)proc[i], 0, MEM_RELEASE);
#else
                munmap((void *)proc[i], procSize[i]);
#endif
            }
        }
    }
};

template <bool compiled>
static const VSFrame *VS_CC exprGetFrame(int n, int activationReason, void *instanceData, void **frameData, VSFrameContext *frameCtx, VSCore *core, const VSAPI *vsapi) {
    ExprData *d = static_cast<ExprData *>(instanceData);
    int numInputs = d->numInputs;

    if (activationReason == arInitial) {
        for (int i = 0; i < numInputs; i++)
            vsapi->requestFrameFilter(n, d->node[i], frameCtx);
    } else if (activationReason == arAllFramesReady) {
        const VSFrame *src[numInputs] = {};

        for (int i = 0; i < numInputs; i++)
            src[i] = vsapi->getFrameFilter(n, d->node[i], frameCtx);

        int height = vsapi->getFrameHeight(src[0], 0);
        int width = vsapi->getFrameWidth(src[0], 0);
        int planes[3] = { 0, 1, 2 };

        const VSFrame *srcf[3] = {
            d->plane[0] == PlaneOp::poCopy ? src[0] : nullptr,
            d->plane[1] == PlaneOp::poCopy ? src[0] : nullptr,
            d->plane[2] == PlaneOp::poCopy ? src[0] : nullptr
        };

        VSFrame *dst = vsapi->newVideoFrame2(&d->vi.format, width, height, srcf, planes, src[0], core);

        if (!d->gotPtrSizes) {
            for (int plane = 0; plane < d->vi.format.numPlanes; plane++) {
                d->ptroffsets[plane][0] = d->vi.format.bytesPerSample * 8;

                d->frame_strides[plane][0] = vsapi->getStride(dst, plane);
                for (int i = 0; i < numInputs; i++) {
                    if (d->node[i]) {
                        d->frame_strides[plane][i + 1] = vsapi->getStride(src[i], plane);
                        d->ptroffsets[plane][i + 1] = vsapi->getVideoFrameFormat(src[i])->bytesPerSample * 8;
                    }

                }
            }

            d->gotPtrSizes = true;
        }

        const uint8_t *srcp[numInputs] = {};

        for (int plane = 0; plane < d->vi.format.numPlanes; plane++) {
            if (d->plane[plane] != PlaneOp::poProcess)
                continue;

            for (int i = 0; i < numInputs; i++) {
                if (d->node[i])
                    srcp[i] = vsapi->getReadPtr(src[i], plane);
            }

            uint8_t *dstp = vsapi->getWritePtr(dst, plane);
            int h = vsapi->getFrameHeight(dst, plane);
            int w = vsapi->getFrameWidth(dst, plane);

            std::vector<float> frame_consts(MemoryVar::MV_SIZE, 0.0f);
            frame_consts[MemoryVar::VAR_N] = (float)n;

            if constexpr (compiled) {
                ExprCompiler::ProcessLineProc proc = d->proc[plane];
                int niterations = (w + 7) / 8;

                for (int y = 0; y < h; y++) {
                    frame_consts[MemoryVar::VAR_Y] = y;
                    uint8_t *rwptrs[d->alignment] = { dstp + d->frame_strides[plane][0] * y };
                    for (int i = 0; i < numInputs; i++) {
                        rwptrs[i + 1] = const_cast<uint8_t *>(srcp[i] + d->frame_strides[plane][i + 1] * y);
                    }
                    proc(rwptrs, d->ptroffsets[plane], &frame_consts[0], niterations);
                }
            } else {
                ExprInterpreter interpreter(d->bytecode[plane].data(), d->bytecode[plane].size());

                for (int y = 0; y < h; y++) {
                    frame_consts[MemoryVar::VAR_Y] = y;
                    for (int x = 0; x < w; x++) {
                        interpreter.eval(srcp, dstp, &frame_consts[0], x);
                    }

                    for (int i = 0; i < numInputs; i++) {
                        srcp[i] += d->frame_strides[plane][i + 1];
                    }
                    dstp += d->frame_strides[plane][0];
                }
            }
        }

        for (int i = 0; i < numInputs; i++) {
            vsapi->freeFrame(src[i]);
        }

        return dst;
    }

    return nullptr;
}

static void VS_CC exprFree(void *instanceData, VSCore *core, const VSAPI *vsapi) {
    ExprData *d = static_cast<ExprData *>(instanceData);
    for (int i = 0; i < d->numInputs; i++)
        vsapi->freeNode(d->node[i]);
    delete d;
}

void VS_CC exprCreate(const VSMap *in, VSMap *out, void *userData, VSCore *core, const VSAPI *vsapi) {
    std::unique_ptr<ExprData> d(new ExprData);
    int err;
    int cpulevel;

    try {
        d->numInputs = vsapi->mapNumElements(in, "clips");
        d->node = std::vector<VSNode *>(d->numInputs, nullptr);

        d->alignment = ((d->numInputs + 1) + 7) & ~7;

        for (int i = 0; i < d->numInputs; i++) {
            d->node[i] = vsapi->mapGetNode(in, "clips", i, &err);
        }

        const VSVideoInfo *vi[d->numInputs] = {};
        for (int i = 0; i < d->numInputs; i++) {
            if (d->node[i])
                vi[i] = vsapi->getVideoInfo(d->node[i]);
        }

        for (int i = 0; i < d->numInputs; i++) {
            if (!isConstantVideoFormat(vi[i]))
                throw std::runtime_error("Only clips with constant format and dimensions allowed");
            if (vi[0]->format.numPlanes != vi[i]->format.numPlanes
                || vi[0]->format.subSamplingW != vi[i]->format.subSamplingW
                || vi[0]->format.subSamplingH != vi[i]->format.subSamplingH
                || vi[0]->width != vi[i]->width
                || vi[0]->height != vi[i]->height)
            {
                throw std::runtime_error("All inputs must have the same number of planes and the same dimensions, subsampling included");
            }

            if (
                (vi[i]->format.bitsPerSample > 32 && vi[i]->format.sampleType == stInteger) ||
                (vi[i]->format.bitsPerSample != 16 && vi[i]->format.bitsPerSample != 32 && vi[i]->format.sampleType == stFloat)
            )
                throw std::runtime_error("Input clips must be 8-32 bit integer or 16/32 bit float format");
        }

        d->vi = *vi[0];
        int format = vsapi->mapGetIntSaturated(in, "format", 0, &err);
        if (!err) {
            VSVideoFormat f;
            if (vsapi->getVideoFormatByID(&f, format, core) && f.colorFamily != cfUndefined) {
                if (d->vi.format.numPlanes != f.numPlanes)
                    throw std::runtime_error("The number of planes in the inputs and output must match");
                vsapi->queryVideoFormat(&d->vi.format, d->vi.format.colorFamily, f.sampleType, f.bitsPerSample, d->vi.format.subSamplingW, d->vi.format.subSamplingH, core);
            }
        }

        int nexpr = vsapi->mapNumElements(in, "expr");
        if (nexpr > d->vi.format.numPlanes)
            throw std::runtime_error("More expressions given than there are planes");

        std::string expr[3];
        for (int i = 0; i < nexpr; i++)
            expr[i] = vsapi->mapGetData(in, "expr", i, nullptr);

        for (int i = nexpr; i < 3; ++i)
            expr[i] = expr[nexpr - 1];


        d->gotPtrSizes = false;

        cpulevel = vs_get_cpulevel(core, vsapi);

    
        for (int i = 0; i < d->vi.format.numPlanes; i++) {
            d->frame_strides[i] = new ptrdiff_t[(d->numInputs + 1)];
            d->ptroffsets[i] = new intptr_t[d->alignment];

            if (!expr[i].empty()) {
                d->plane[i] = PlaneOp::poProcess;
            } else {
                if (d->vi.format.bitsPerSample == vi[0]->format.bitsPerSample && d->vi.format.sampleType == vi[0]->format.sampleType)
                    d->plane[i] = PlaneOp::poCopy;
                else
                    d->plane[i] = PlaneOp::poUndefined;

                continue;
            }

            d->bytecode[i] = compile(expr[i], vi, d->numInputs, d->vi);

            if (cpulevel <= VS_CPU_LEVEL_NONE)
                continue;

            std::tie(d->proc[i], d->procSize[i]) = expr::compile_jit(d->bytecode[i].data(), d->bytecode[i].size(), d->numInputs, cpulevel);
        }
#ifdef VS_TARGET_OS_WINDOWS
        FlushInstructionCache(GetCurrentProcess(), nullptr, 0);
#endif
    } catch (std::runtime_error &e) {
        for (int i = 0; i < d->numInputs; i++) {
            vsapi->freeNode(d->node[i]);
        }
        vsapi->mapSetError(out, (std::string{ "Expr: " } + e.what()).c_str());
        return;
    }

    std::vector<VSFilterDependency> deps;
    for (int i = 0; i < d->numInputs; i++)
        deps.push_back({d->node[i], (d->vi.numFrames <= vsapi->getVideoInfo(d->node[i])->numFrames) ? rpStrictSpatial : rpGeneral});

    VSFilterGetFrame getFrame = (cpulevel > VS_CPU_LEVEL_NONE) ? exprGetFrame<true> : exprGetFrame<false>;

    vsapi->createVideoFilter(out, "Expr", &d->vi, getFrame, exprFree, fmParallel, deps.data(), d->numInputs, d.get(), core);

    d.release();
}
