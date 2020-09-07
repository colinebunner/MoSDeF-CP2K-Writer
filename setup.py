import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="cssi_cp2k", # Replace with your own username
    version="0.0.1",
    author="Colin Bunner and Ramanish Singh",
    author_email="singh891@umn.edu",
    description='A Python input/output interface to CP2K, developed as part of the MoSDeF simulation suite.',
    long_description=long_description,
    long_description_content_type="text/markdown",
   # url='https://github.com/ramanishsingh/cp2kmd',
    packages=['cssi_cp2k','cssi_cp2k/classes','cssi_cp2k/utilities1'],
=======
from setuptools import setup

setup(name='cssi_cp2k',
      version='0.1',
      description='A Python input/output interface to CP2K, developed as part of the MoSDeF simulation suite.',
      url='https://github.com/ramanishsingh/CSSI_CP2K',
      author='Ramanish Singh',
      author_email='singh891@umn.edu',
      license='MIT',
      packages=['cssi_cp2k','cssi_cp2k/classes','cssi_cp2k/utilities1'],
>>>>>>> e3366933857742950e08ad98919fcf0d7eb7a25f
      install_requires = [
        'numpy',
      ],
      zip_safe=False)
