project('FancyEngine2', ['c', 'd'])
d_args = ['-J../src'] # -enable-color #for ldc
# -color=on #for dmd

if host_machine.system() == 'windows'
  sys = 'win'
elif host_machine.system() == 'macos'
  sys = 'macos'
elif host_machine.system() == 'linux'
  sys = 'linux'
endif

d_versions = []
fancy_source = []
dependencies = []

subdir('deps')

if get_option('graphics_backend') == 'opengl'
  d_versions += '_Gfx_is_opengl'

  d_versions += 'SDL_201' # for bindbc-sdl
  d_versions += 'GL_33'   # for bindbc-opengl
  d_versions += 'GL_ARB'  # bindbc-gl: load all supported ARB/KHR extensions

  fancy_source += ['src/graphics/fancy_model_gl.d', 'src/graphics/model_gl.d', 'src/graphics/shading_gl.d', 'src/graphics/tex_gl.d', 'src/windowing/windows_gl.d']
  fancy_source += gl_source
elif get_option('graphics_backend') == 'vulkan'
  d_versions += '_Gfx_is_vulkan'
  d_versions += 'SDL_206'
  fancy_source += ['src/graphics/fancy_model_vk.d', 'src/graphics/model_vk.d', 'src/graphics/shading_vk.d', 'src/graphics/tex_vk.d', 'src/windowing/windows_vk.d']
  fancy_source += ['src/windowing/windows_vk.d']
  fancy_source += vk_source
endif

if get_option('buildtype') == 'debugoptimized'
  d_versions += '_Build_type_is_dev'
elif get_option('buildtype') == 'release'
  d_versions += '_Build_type_is_release'
endif


fancy_source += ['src/app.d', 'src/asset.d', 'src/config.d', 'src/cstdlib.d', 'src/logging.d', 'src/stdlib.d', 'src/stdmem.d', 'src/stdmath.d', 'src/sound/gorilla.d', 'src/graphics/fancy_model.d', 'src/graphics/model.d', 'src/graphics/shading.d', 'src/graphics/tex.d', 'src/scripting/package.d', 'src/scripting/s7.d', 'src/scripting/s7_lib_interface.d', 'src/windowing/key.d', 'src/windowing/windows.d']
include_dirs = include_directories('src', 'deps/bindbc-opengl/source', 'deps/bindbc-sdl/source', 'deps/erupted/source', 'deps/gfm/math', 'deps/bindbc-assimp/source', 'deps/bindbc-loader/source', 'deps/toml/src')
link_args = ['../lib/' + sys + '/gorilla.lib']


#run_command('git.exe rev-parse --short HEAD > src/.commit.txt')
if sys == 'win'
  run_command('powershell', '-c', 'del build/icon.rc')
  run_command('powershell', '-c', 'del build/icon.res')
  run_command('powershell', '-c', 'add-content build/icon.rc \'100 ICON "../icon.ico"\'')
  run_command('rc.exe', 'build/icon.rc')
  link_args += ['icon.res']
  foreach lib: ['ole32', 'oleaut32', 'strmiids', 'user32']
    link_args += '-L=' + lib + '.lib'
  endforeach
  link_args += ['-L=/nodefaultlib:msvcrtd.lib', '-L=/nodefaultlib:msvcrt.lib', '-L=/nodefaultlib:vcruntime.lib', '-mscrtlib=libcmt']
  run_command('cmd', '/c', 'git rev-parse --short HEAD > src/.commit_hash.txt')
else
  link_args += '-L-Llib/' + sys + '/'
  run_command('sh', '-c', 'git rev-parse --short HEAD > src/.commit_hash.txt')
  dependencies += dependency('openal')
endif


executable('fancyengine2', fancy_source + dep_source, d_args: d_args, link_args: link_args, include_directories: include_dirs, d_module_versions: d_versions, dependencies: dependencies)
