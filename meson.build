project('overspread','c',
  version: '2026.0.1', # match with pyproject.toml
  license: 'GPL-3', # match with the LICENSE file
  meson_version: '>=0.64.0',
  default_options : ['warning_level=2'],
)


py_mod =  import('python')
py3 = py_mod.find_installation(pure: false)
py3_dep = py3.dependency()
message(py3.full_path())
message(py3.get_install_dir())

spec_sources = ['src/spec_worker/spec_worker.c']
py3.extension_module('_c_spec_worker',
                    spec_sources,
                    dependencies : [py3_dep],
                    subdir: 'overspread',
                    install : true
                    )

list_cmd = run_command('sh', '-c', 'ls overspread/*.py', check: true)
python_files = list_cmd.stdout().strip().split('\n')

py3.install_sources(
  python_files,
  subdir: 'overspread'
)

install_data('overspread/radars.ini', install_dir : py3.get_install_dir() / 'overspread')

install_subdir('overspread/dat',
  install_dir : py3.get_install_dir() / 'overspread',
  strip_directory: false  # 'false' keeps the folder 'my_data' itself
  )
