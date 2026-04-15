project('overspread','c',
  version: '2026.0.1', # match with pyproject.toml
  license: 'MIT', # match with the LICENSE file
  meson_version: '>= 0.64.0',
  default_options : [
      'buildtype=debugoptimized',
      'warning_level=3',
      'c_args=-Wno-unused-function -Wno-conversion -Wno-misleading-indentation -Wno-incompatible-pointer-types',
    ],
)

add_languages('c', native: false)

py_mod =  import('python')
py3 = py_mod.find_installation(pure: false)
py3_dep = py3.dependency()
message(py3.full_path())
message(py3.get_install_dir())

# Determine whether this is being compiled with numpy 1.X or 2.X
numpy_ver = run_command(py3,
    ['-c', 'import numpy; print(numpy.__version__.split(".")[0])'],
      check : true
      ).stdout().strip()
is_numpytwo = numpy_ver.to_int() >= 2

message('The Numpy major version is: ', numpy_ver)

if is_numpytwo
  numpy_nodepr_api = '-DNPY_NO_DEPRECATED_API=NPY_2_0_API_VERSION'
else
    numpy_nodepr_api = '-DNPY_NO_DEPRECATED_API=NPY_1_9_API_VERSION'
endif

# Get NumPy include directories for F2PY
incdir_numpy = run_command(py3,
  ['-c', 'import os; os.chdir(".."); import numpy; print(numpy.get_include())'],
  check : true
).stdout().strip()

incdir_f2py = run_command(py3,
  ['-c', 'import os; os.chdir(".."); from numpy import f2py; print(f2py.get_include())'],
  check : true
).stdout().strip()

inc_dirs = include_directories(incdir_numpy, incdir_f2py)

cc = meson.get_compiler('c')

if cc.has_argument('-Wno-unused-but-set-variable')
  add_global_arguments('-Wno-unused-but-set-variable', language : 'c')
endif

m_dep = cc.find_library('m', required : false)
if m_dep.found()
  add_project_link_arguments('-lm', language : 'c')
  message('Adding -lm to link arguments')
endif

# Add more link arguments
add_project_link_arguments('-lquadmath', language: ['c' ])
message('Adding lquadmath link argument')

if is_numpytwo
  c_flags = ['-DNPY_NO_DEPRECATED_API=NPY_2_0_API_VERSION']
else
  c_flags = ['-DNPY_NO_DEPRECATED_API=NPY_1_9_API_VERSION']
endif

cython_c_args = []

# Platform detection to set more flags for Windows systems
is_windows = host_machine.system() == 'windows'
is_mac = host_machine.system() == 'darwin'
is_mingw = is_windows and cc.get_id() == 'gcc'

if is_windows
  # For mingw-w64, link statically against the UCRT.
  gcc_link_args = ['-lucrt', '-static']

elif is_mac
  c_flags += ['-FALIGN_FUNCTIONS=8']
endif

# Check the python headers
cc.check_header('Python.h', dependencies: [py3_dep], required: true)





spec_sources = 'src/spec_worker/spec_worker.c'

py3.extension_module('_c_spec_worker',
                    spec_sources,
                    dependencies : [py3_dep],
		    include_directories: inc_dirs,
                    c_args : c_flags,
		    subdir: 'overspread',
                    install : true
                    )

list_cmd = run_command('sh', '-c', 'ls overspread/*.py', check: true)
python_files = list_cmd.stdout().strip().split('\n')

py3.install_sources(
  python_files,
  subdir:  'overspread'
)

install_data('overspread/radars.ini', install_dir : py3.get_install_dir() / 'overspread')

install_subdir('overspread/dat',
  install_dir : py3.get_install_dir() / 'overspread',
  strip_directory: false  # 'false' keeps the folder 'my_data' itself
  )
