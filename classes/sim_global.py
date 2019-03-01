class GLOBAL:

  def __init__(self,ALLTOALL_SGL=False,BLACS_GRID='SQUARE',BLACS_REPEATABLE=False,CALLGRAPH='NONE',
               CALLGRAPH_FILE_NAME=None,ECHO_ALL_HOSTS=False,ECHO_INPUT=False,ELPA_KERNEL='AUTO',
               ELPA_QR=False,ELPA_QR_UNSAFE=False,EXTENDED_FFT_LENGTHS=False,FFTW_PLAN_TYPE='ESTIMATE',
               FFTW_WISDOM_FILE_NAME='/etc/fftw/wisdom',FFT_POOL_SCRATCH_LIMIT=15,
               FLUSH_SHOULD_FLUSH=True,OUTPUT_FILE_NAME=None,PREFERRED_DIAG_LIBRARY='SL',
               PREFERRED_FFT_LIBRARY='FFTW3',PRINT_LEVEL='MEDIUM',PROGRAM_NAME='CP2K',
               PROJECT_NAME='PROJECT',RUN_TYPE='ENERGY_FORCE',SAVE_MEM=False,SEED=8675309,TRACE=False,
               TRACE_MASTER=True,TRACE_MAX=2147483647,TRACE_ROUTINES=[],WALLTIME='24:00:00',DBCSR=None,
               FM=None,FM_DIAG_SETTINGS=None,PRINT=None,PRINT_ELPA=None,PROGRAM_RUN_INFO=None,
               REFERENCES=None,TIMINGS=None):

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
      errrorMessage = "Type: Setter\nVar.: BLACS_GRID\nErr.: BLACS_GRID val {} not allowed." 
         "Check for typo, otherwise consult CP2K input manual for allowed values.".format(val)
    
  
