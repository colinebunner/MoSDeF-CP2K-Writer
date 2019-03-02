import DBCSR
import FM
import FM_DIAG_SETTINGS
import PRINT
import PRINT_ELPA
import PROGRAM_RUN_INFO
import REFERENCES
import TIMINGS
import os
import cssi_cp2k.utilities as utilities

class GLOBAL:

  BLACS_GRID_VALS     = ["COLUMN","ROW","SQUARE"]
  CALLGRAPH_VALS      = ["ALL","MASTER","NONE"]
  ELPA_KERNEL_VALS    = ["AUTO","AVX2_BLOCK2","AVX2_BLOCK4","AVX2_BLOCK6","AVX512_BLOCK2","AVX512_BLOCK4",
                         "AVX512_BLOCK6","AVX_BLOCK2","AVX_BLOCK4","AVX_BLOCK6","BGP","BGQ","GENERIC",
                         "GENERIC_SIMPLE","GPU","SSE","SSE_BLOCK2","SSE_BLOCK4","SSE_BLOCK6"]
  FFTW_PLAN_TYPE_VALS = ["ESTIMATE","EXHAUSTIVE","MEASURE","PATIENT"]
  PREFERRED_DIAG_LIBRARY_VALS = ["ELPA","SL","SL2"]
  PREFERRED_FFT_LIBRARY_VALS  = ["FFTSG","FFTW","FFTW3"]
  PRINT_LEVEL_VALS            = ["DEBUG","HIGH","LOW","MEDIUM","SILENT"]
  PROGRAM_NAME_VALS           = ["ATOM","CP2K","FARMING","MC_ANALYSIS","OPTIMIZE_BASIS","OPTIMIZE_INPUT",
                                 "SWARM","TEST","TMC"]
  RUN_TYPE_VALS               = ["BAND","BSSE","CELL_OPT","DEBUG","DRIVER","EHRENFEST_DYN",
                                 "ELECTRONIC_SPECTRA","ENERGY","ENERGY_FORCE","GEOMETRY_OPTIMIZATION",
                                 "GEO_OPT","LINEAR_RESPONSE","LR","MC","MOLECULAR_DYNAMICS","MONTECARLO",
                                 "NEGF","NONE","NORMAL_MODES","PINT","RT_PROPAGATION","SPECTRA","TAMC",
                                 "TMC","VIBRATIONAL_ANALYSIS","WAVEFUNCTION_OPTIMIZATION","WFN_OPT"]

  def __init__(self,ALLTOALL_SGL=False,BLACS_GRID='SQUARE',BLACS_REPEATABLE=False,CALLGRAPH='NONE',
               CALLGRAPH_FILE_NAME=None,ECHO_ALL_HOSTS=False,ECHO_INPUT=False,ELPA_KERNEL='AUTO',
               ELPA_QR=False,ELPA_QR_UNSAFE=False,EXTENDED_FFT_LENGTHS=False,FFTW_PLAN_TYPE='ESTIMATE',
               FFTW_WISDOM_FILE_NAME='/etc/fftw/wisdom',FFT_POOL_SCRATCH_LIMIT=15,
               FLUSH_SHOULD_FLUSH=True,OUTPUT_FILE_NAME=None,PREFERRED_DIAG_LIBRARY='SL',
               PREFERRED_FFT_LIBRARY='FFTW3',PRINT_LEVEL='MEDIUM',PROGRAM_NAME='CP2K',
               PROJECT_NAME='PROJECT',RUN_TYPE='ENERGY_FORCE',SAVE_MEM=False,SEED=8675309,TRACE=False,
               TRACE_MASTER=True,TRACE_MAX=2147483647,TRACE_ROUTINES=[],WALLTIME='24:00:00'):

    self.__ALLTOALL_SGL           = ALLTOALL_SGL
    self.__BLACS_GRID             = BLACS_GRID
    self.__BLACS_REPEATABLE       = BLACS_REPEATABLE
    self.__CALLGRAPH              = CALLGRAPH
    self.__CALLGRAPH_FILE_NAME    = CALLGRAPH_FILE_NAME
    self.__ECHO_ALL_HOSTS         = ECHO_ALL_HOSTS
    self.__ECHO_INPUT             = ECHO_INPUT
    self.__ELPA_KERNEL            = ELPA_KERNEL
    self.__ELPA_QR                = ELPA_QR
    self.__ELPA_QR_UNSAFE         = ELPA_QR_UNSAFE
    self.__EXTENDED_FFT_LENGTHS   = EXTENDED_FFT_LENGTHS
    self.__FFTW_PLAN_TYPE         = FFTW_PLAN_TYPE
    self.__FFTW_WISDOM_FILE_NAME  = FFTW_WISDOM_FILE_NAME
    self.__FFT_POOL_SCRATCH_LIMIT = FFT_POOL_SCRATCH_LIMIT
    self.__FLUSH_SHOULD_FLUSH     = FLUSH_SHOULD_FLUSH
    self.__OUTPUT_FILE_NAME       = OUTPUT_FILE_NAME
    self.__PREFERRED_DIAG_LIBRARY = PREFERRED_DIAG_LIBRARY
    self.__PREFERRED_FFT_LIBRARY  = PREFERRED_FFT_LIBRARY
    self.__PRINT_LEVEL            = PRINT_LEVEL
    self.__PROGRAM_NAME           = PROGRAM_NAME
    self.__PROJECT_NAME           = PROJECT_NAME
    self.__RUN_TYPE               = RUN_TYPE
    self.__SAVE_MEM               = SAVE_MEM
    self.__SEED                   = SEED
    self.__TRACE                  = TRACE
    self.__TRACE_MASTER           = TRACE_MASTER
    self.__TRACE_MAX              = TRACE_MAX
    self.__TRACE_ROUTINES         = TRACE_ROUTINES
    self.__WALLTIME               = WALLTIME
    # Consider adding subsec_args options to init
    self.__DBCSR                  = DBCSR()
    self.__FM                     = FM()
    self.__FM_DIAG_SETTINGS       = FM_DIAG_SETTINGS()
    self.__PRINT                  = PRINT()
    self.__PRINT_ELPA             = PRINT_ELPA()
    self.__PROGRAM_RUN_INFO       = PROGRAM_RUN_INFO()
    self.__REFERENCES             = REFERENCES()
    self.__TIMINGS                = TIMINGS()

  @property
  def ALLTOALL_SGL(self):
    return self.__ALLTOALL_SGL

  @property
  def BLACS_GRID(self):
    return self.__BLACS_GRID

  @property
  def BLACS_REPEATABLE(self):
    return self.__BLACS_REPEATABLE

  @property
  def CALLGRAPH(self):
    return self.__CALLGRAPH

  @property
  def CALLGRAPH_FILE_NAME(self):
    return self.__CALLGRAPH_FILE_NAME

  @property
  def ECHO_ALL_HOSTS(self):
    return self.__ECHO_ALL_HOSTS

  @property
  def ECHO_INPUT(self):
    return self.__ECHO_INPUT

  @property
  def ELPA_KERNEL(self):
    return self.__ELPA_KERNEL

  @property
  def ELPA_QR(self):
    return self.__ELPA_KERNEL

  @property
  def ELPA_QR_UNSAFE(self):
    return self.__ELPA_QR_UNSAFE

  @property
  def EXTENDED_FFT_LENGTHS(self):
    return self.__EXTENDED_FFT_LENGTHS

  @property
  def FFTW_PLAN_TYPE(self):
    return self.__FFTW_PLAN_TYPE

  @property
  def FFTW_WISDOM_FILE_NAME(self):
    return self.__FFTW_WISDOM_FILE_NAME

  @property
  def FFT_POOL_SCRATCH_LIMIT(self):
    return self.__FFT_POOL_SCRATCH_LIMIT

  @property
  def FLUSH_SHOULD_FLUSH(self):
    return self.__FLUSH_SHOULD_FLUSH

  @property
  def OUTPUT_FILE_NAME(self):
    return self.__OUTPUT_FILE_NAME

  @property
  def PREFERRED_DIAG_LIBRARY(self):
    return self.__PREFERRED_DIAG_LIBRARY

  @property
  def PREFERRED_FFT_LIBRARY(self):
    return self.__PREFERRED_FFT_LIBRARY

  @property
  def PRINT_LEVEL(self):
    return self.__PRINT_LEVEL

  @property
  def PROGRAM_NAME(self):
    return self.__PROGRAM_NAME

  @property
  def PROJECT_NAME(self):
    return self.__PROJECT_NAME

  @property
  def RUN_TYPE(self):
    return self.__RUN_TYPE

  @property
  def SAVE_MEM(self):
    return self.__SAVE_MEM

  @property
  def SEED(self):
    return self.__SEED

  @property
  def TRACE(self):
    return self.__TRACE

  @property
  def TRACE_MASTER(self):
    return self.__TRACE_MASTER

  @property
  def TRACE_MAX(self):
    return self.__TRACE_MAX

  @property
  def TRACE_ROUTINES(self):
    return self.__TRACE_ROUTINES

  @property
  def WALLTIME(self):
    return self.__WALLTIME

  @property
  def DBCSR(self):
    return self.__DBCSR

  @property
  def FM(self):
    return self.__FM

  @property
  def FM_DIAG_SETTINGS(self):
    return self.__FM_DIAG_SETTINGS

  @property
  def PRINT(self):
    return self.__PRINT

  @property
  def PRINT_ELPA(self):
    return self.__PRINT_ELPA

  @property
  def PROGRAM_RUN_INFO(self):
    return self.__PROGRAM_RUN_INFO

  @property
  def REFERENCES(self):
    return self.__REFERENCES

  @property
  def TIMINGS(self):
    return self.__TIMINGS

  @ALLTOALL_SGL.setter
  def ALLTOALL_SGL(self,val):
    if isinstance(val,bool):
      self.__ALLTOALL_SGL = val
    else:
      errorMessage = "Type: Setter\nVar.: ALLTOALL_SGL\nErr.: ALLTOALL_SGL must be a boolean."

  @BLACS_GRID.setter
  def BLACS_GRID(self,val):
    val = str(val).upper()
    if val in BLACS_GRID_VALS:
      self.__BLACS_GRID = val
    else:
      errrorMessage = ("Type: Setter\nVar.: BLACS_GRID\nErr.: BLACS_GRID val {} not allowed. "
         "Check for typo. Allowed BLACS_GRID values: {}.".format(val,BLACS_GRID_VALS))
    
  
  @BLACS_REPEATABLE.setter
  def BLACS_REPEATABLE(self,val):
    if isinstance(val,bool):
      self.__BLACS_REPEATABLE = val
    else:
      errorMessage = "Type: Setter\nVar.: BLACS_REPEATABLE\nErr.: BLACS_REPEATABLE must be a boolean."

  @CALLGRAPH.setter
  def CALLGRAPH(self,val):
    val = str(val).upper()
    if val in CALLGRAPH_VALS:
      self.__CALLGRAPH = val

  @CALLGRAPH_FILE_NAME.setter
  def CALLGRAPH_FILE_NAME(self,val):
    self.__CALLGRAPH_FILE_NAME = str(val)

  @ECHO_ALL_HOSTS.setter
  def ECHO_ALL_HOSTS(self,val):
    if isinstance(val,bool):
      self.__ECHO_ALL_HOSTS = val
    else:
      errorMessage = "Type: Setter\nVar.: ECHO_ALL_HOSTS\nErr.: ECHO_ALL_HOSTS must be a boolean."

  @ECHO_INPUT.setter
  def ECHO_INPUT(self,val):
    if isinstance(val,bool):
      self.__ECHO_INPUT = val
    else:
      errorMessage = "Type: Setter\nVar.: ECHO_INPUT\nErr.: ECHO_INPUT must be a boolean."

  @ELPA_KERNEL.setter
  def ELPA_KERNEL(self,val):
    val = str(val).upper()
    if val in ELPA_KERNEL_VALS:
      self.__ELPA_KERNEL_VALS = val
    else:
      errorMessage = ("Type: Setter\nVar.: ELPA_KERNEL\nErr.: ELPA_KERNEL val {} not allowed. "
       "Check for typo. Allowed ELPA_KERNEL values: {}".format(val,ELPA_KERNEL_VALS))

  @ELPA_QR.setter
  def ELPA_QR(self,val):
    if isinstance(val,bool):
      self.__ELPA_QR = val
    else:
      errorMessage = "Type: Setter\nVar.: ELPA_QR\nErr.: ELPA_QR must be a boolean."

  @ELPA_QR_UNSAFE.setter
  def ELPA_QR_UNSAFE(self,val):
    if isinstance(val,bool):
      self.__ELPA_QR_UNSAFE = val
    else:
      errorMessage = "Type: Setter\nVar.: ELPA_QR_UNSAFE\nErr.: ELPA_QR_UNSAFE must be a boolean."

  @EXTENDED_FFT_LENGTHS.setter
  def EXTENDED_FFT_LENGTHS(self,val):
    if isinstance(val,bool):
      self.__EXTENDED_FFT_LENGTHS = val
    else:
      errorMessage = "Type: Setter\nVar.: EXTENDED_FFT_LENGTHS\nErr.: EXTENDED_FFT_LENGTHS must be a boolean."

  @FFTW_PLAN_TYPE.setter
  def FFTW_PLAN_TYPE(self,val):
    val = str(val).upper()
    if val in FFTW_PLAN_TYPE_VALS:
      self.__FFTW_PLAN_TYPE = val
    else:
      errorMessage = ("Type: Setter\nVar.: FFTW_PLAN_TYPE\nErr.: FFTW_PLAN_TYPE val {} not allowed. "
        "Check for typo. Allowed FFTW_PLAN_TYPE values: {}".format(val,FFTW_PLAN_TYPE_VALS))

  @FFTW_WISDOM_FILE_NAME.setter
  def FFTW_WISDOM_FILE_NAME(self,val):
    if os.path.isfile(val):
      self.__FFTW_WISDOM_FILE_NAME = val
    else:
      errorMessage = "Type: Setter\nVar.: FFTW_WISDOM_FILE_NAME\nErr.: File {} not found.".format(val)

  @FFT_POOL_SCRATCH_LIMIT.setter
  def FFT_POOL_SCRATCH_LIMIT(self,val):
    if utilities.is_number(val):
      self.__FFT_POOL_SCRATCH_LIMIT = float(val)
    else:
      errorMessage = "Type: Setter\nVar.: FFT_POOL_SCRATCH_LIMIT\nErr.: FFT_POOL_SCRATCH_LIMIT must be numeric."

  @FLUSH_SHOULD_FLUSH.setter
  def FLUSH_SHOULD_FLUSH(self,val):
    if isinstance(val,bool):
      self.__FLUSH_SHOULD_FLUSH = val
    else:
      errorMessage = "Type: Setter\nVar.: FLUSH_SHOULD_FLUSH\nErr.: FLUSH_SHOULD_FLUSH must be a boolean."

  @OUTPUT_FILE_NAME.setter
  def OUTPUT_FILE_NAME(self,val):
    self.__OUTPUT_FILE_NAME = val

  @PREFERRED_DIAG_LIBRARY.setter
  def PREFERRED_DIAG_LIBRARY(self,val):
    val = str(val).upper()
    if val in PREFERRED_DIAG_LIBRARY_VALS:
      self.__PREFERRED_DIAG_LIBRARY = val
    else:
      errorMessage = ("Type: Setter\nVar.: PREFERRED_DIAG_LIBRARY\nErr.: PREFERRED_DIAG_LIBRARY val {} not allowed. "
        "Check for typo. Allowed values are: {}".format(val,PREFERRED_DIAG_LIBRARY_VALS))

  @PREFERRED_FFT_LIBRARY.setter
  def PREFERRED_FFT_LIBRARY(self,val):
    val = str(val).upper()
    if val in PREFERRED_FFT_LIBRARY_VALS:
      self.__PREFERRED_FFT_LIBRARY = val
    else:
      errorMessage = ("Type: Setter\nVar.: PREFERRED_FFT_LIBRARY\nErr.: PREFERRED_FFT_LIBRARY val {} not allowed. "
        "Check for typo. Allowed values are: {}".format(val,PREFERRED_FFT_LIBRARY_VALS))
 
  @PRINT_LEVEL.setter
  def PRINT_LEVEL(self,val):
    val = str(val).upper()
    if val in PRINT_LEVEL_VALS:
      self.__PRINT_LEVEL = val
    else:
      errorMessage = ("Type: Setter\nVar.: PRINT_LEVEL\nErr.: PRINT_LEVEL val {} not allowed. "
       "Check for typo. Allowed values are: {}".format(val,PRINT_LEVEL_VALS))

  @PROGRAM_NAME.setter
  def PROGRAM_NAME(self,val):
    val = str(val).upper()
    if val in PROGRAM_NAME_VALS:
      self.__PROGRAM_NAME = val
    else:
      errorMessage = ("Type: Setter\nVar.: PROGRAM_NAME\nErr.: PROGRAM_NAME {} not allowed. "
        "Check for typo. Allowed values are: {}".format(val,PROGRAM_NAME_VALS))

  @PROJECT_NAME.setter
  def PROJECT_NAME(self,val):
    self.__PROJECT_NAME = val

  @RUN_TYPE.setter
  def RUN_TYPE(self,val):
    val = str(val).upper()
    if val in RUN_TYPE_VALS:
      self.__RUN_TYPE = val
    else:
      errorMessage = ("Type: Setter\nVar.: RUN_TYPE\nErr.: RUN_TYPE {} not allowed. "
        "Check for typo. Allowed values are: {}".format(val,RUN_TYPE_VALS))

  @SAVE_MEM.setter
  def SAVE_MEM(self,val):
    if isinstance(val,bool):
      self.__SAVE_MEM = val
    else:
      errorMessage = "Type: Setter\nVar.: SAVE_MEM\nErr.: SAVE_MEM must be a boolean."

  @SEED.setter
  def SEED(self,val):
    if utilities.is_number(val):
      self.__SEED = int(float(val)//1)

  @TRACE.setter
  def TRACE(self,val):
    if isinstance(val,bool):
      self.__TRACE = val
    else:
      errorMessage = "Type: Setter\nVar.: TRACE\nErr.: TRACE must be a boolean."

  @TRACE_MASTER.setter
  def TRACE_MASTER(self,val):
    if isinstance(val,bool):
      self.__TRACE_MASTER = val
    else:
      errorMessage = "Type: Setter\nVar.: TRACE_MASTER\nErr.: TRACE_MASTER must be a boolean."

  @TRACE_MAX.setter
  def TRACE_MAX(self,val):
    if utilities.is_number(val):
      self.__TRACE_MAX = int(float(val)//1)
    else:
      errorMessage = ("Type: Setter\nVar.: TRACE_MAX\nErr.: TRACE_MAX must be numeric (technically must be an"
       "integer but this code will make ints out of floats).")

  @TRACE_ROUTINES.setter
  def TRACE_ROUTINES(self,val):
    self.__TRACE_ROUTINES = val

  @WALLTIME.setter
  def WALLTIME(self,val):
    hhmmss = str(val).split(":")
    # If given in seconds, not HH:MM:SS
    if len(hhmmss) == 1 and utilities.is_number(val):
      self.__WALLTIME = int(float(val)//1)
    elif len(hhmmss) == 3:
      isn = True
      for v in hhmmss:
        if not(utilities.is_number(v)):
          isn = False
      if isn:
        self.__WALLTIME = val
      else:
        errorMessage = ("Type: Setter\nVar.: WALLTIME\nErr.: Wrong format for walltime: {}. Must be in seconds"
          "or HH:MM:SS.".format(val))
    else:
      errorMessage = ("Type: Setter\nVar.: WALLTIME\nErr.: Wrong format for walltime: {}. Must be in seconds"
        "or HH:MM:SS.".format(val))
