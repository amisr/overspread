project('overspread','c',
  version: '2026.0.1', # match with pyproject.toml
  license: 'MIT', # match with the LICENSE file
  default_options : [
      'buildtype=debugoptimized',
      'warning_level=3',
    ],
)

py3_mod =  import('python')
py3 = py3_mod.find_installation(pure: false)

cc = meson.get_compiler('c')
m_dep = cc.find_library('m', required : false) # For math functions

spec_sources = 'src/spec_worker/spec_worker.c'

py3.extension_module('_c_spec_worker',
                    spec_sources,
		    dependencies: [m_dep],
		    subdir: 'overspread',
                    install : true,
		    # Ensure symbols are not hidden by default if you have issues
                    gnu_symbol_visibility: 'default'
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
