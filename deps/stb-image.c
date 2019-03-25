#define STBI_WINDOWS_UTF8
#define STB_IMAGE_IMPLEMENTATION
#ifndef __SSE2__
# warning SSE is not enabled!
#endif
#include "stb/stb_image.h"
