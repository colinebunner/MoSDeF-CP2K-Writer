from setuptools import setup

setup(name='cssi_cp2k',
      version='0.1',
      description='A Python input/output interface to CP2K, developed as part of the MoSDeF simulation suite.',
      url='https://github.com/ramanishsingh/CSSI_CP2K',
      author='Ramanish Singh',
      author_email='singh891@umn.edu',
      license='MIT',
      packages=['cssi_cp2k','cssi_cp2k/classes','cssi_cp2k/utilities1'],
      install_requires = [
        'numpy',
      ],
      zip_safe=False)
