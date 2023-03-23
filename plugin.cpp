#include <vector>

#include "plugin.h"

VS_EXTERNAL_API(void) VapourSynthPluginInit2(VSPlugin *plugin, const VSPLUGINAPI *vspapi) {
    vspapi->configPlugin("com.setsugen.dev", "setsugen", "Schizo Expr Plugin", VS_MAKE_VERSION(1, 0), VAPOURSYNTH_API_VERSION, 0, plugin);

    vspapi->registerFunction("Expr", "clips:vnode[];expr:data[];format:int:opt;", "clip:vnode;", exprCreate, (void *)true, plugin);
    vspapi->registerFunction("UnsafeExpr", "clips:vnode[];expr:data[];format:int:opt;", "clip:vnode;", exprCreate, (void *)false, plugin);
    vspapi->registerFunction("ExprDebug", "expr:data[];format:int:opt;optimize:int:opt;", "asm:data[];", exprDebugCreate, nullptr, plugin);
}