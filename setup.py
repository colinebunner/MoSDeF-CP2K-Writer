import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="mosdef_cp2k_writer", 
    version="0.0.1",
    author="Colin Bunner and Ramanish Singh",
    author_email="singh891@umn.edu",
    description='A Python input/output interface to CP2K, developed as part of the MoSDeF simulation suite.',
    long_description=long_description,
    long_description_content_type="text/markdown",
   # url='https://github.com/ramanishsingh/cp2kmd',
    packages=['mosdef_cp2k_writer','mosdef_cp2k_writer/classes','mosdef_cp2k_writer/utilities'],
      install_requires = [
        'numpy',
      ],
      zip_safe=False)
