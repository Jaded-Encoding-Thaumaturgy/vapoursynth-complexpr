#include <vector>

#include "exprfilter.h"

VS_EXTERNAL_API(void) VapourSynthPluginInit2(VSPlugin *plugin, const VSPLUGINAPI *vspapi) {
    vspapi->configPlugin("com.setsugen.dev", "setsugen", "Schizo Expr Plugin", VS_MAKE_VERSION(1, 0), VAPOURSYNTH_API_VERSION, 0, plugin);
    exprInitialize(plugin, vspapi);
}