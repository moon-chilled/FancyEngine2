project('FancyEngine2', ['c', 'd'])
d_args = ['-J../src']

if host_machine.system() == 'windows'
  sys = 'win'
elif host_machine.system() == 'macos'
  sys = 'macos'
elif host_machine.system() == 'linux'
  sys = 'linux'
endif

subdir('deps')
fancy_source = ['src/app.d', 'src/asset.d', 'src/cstdlib.d', 'src/logging.d', 'src/stdlib.d', 'src/stdmath.d', 'src/sound/gorilla.d', 'src/graphics/shading.d', 'src/graphics/tex.d', 'src/scripting/ecl.d', 'src/scripting/ecl_lib_interface.d', 'src/scripting/package.d', 'src/windowing/key.d', 'src/windowing/windows.d']
include_dirs = include_directories('src', 'deps/derelictgl3/source', 'deps/derelictsdl2/source', 'deps/derelictutil/source', 'deps/gfm/math')
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
  link_args += '-L=/nodefaultlib:msvcrtd.lib'
else
  link_args += '-L-Llib/' + sys + '/'
endif


executable('fancyengine2', fancy_source + dep_source, d_args: d_args, link_args: link_args, include_directories: include_dirs)
