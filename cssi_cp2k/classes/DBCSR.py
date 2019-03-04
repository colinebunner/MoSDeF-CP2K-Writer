import cssi_cp2k.utilities
import ACC

class DBCSR:

  MM_DRIVER_VALS = ["AUTO","BLAS","MATMUL","SMM","XSMM"]

  def __init__(self,AVG_ELEMENTS_IMAGES=-1,COMM_THREAD_LOAD=-1,MAX_ELEMENTS_PER_BLOCK=32,
               MM_DRIVER="AUTO",MM_STACK_SIZE=-1,MULTREC_LIMIT=512,NUM_LAYERS_3D=1,
               NUM_MULT_IMAGES=1,N_SIZE_MNK_STACKS=3,USE_COMM_THREAD=True,
               USE_MPI_ALLOCATOR=True,USE_MPI_RMA=False,errors=[]):

    self.__AVG_ELEMENTS_IMAGES    = AVG_ELEMENTS_IMAGES
    self.__COMM_THREAD_LOAD       = COMM_THREAD_LOAD
    self.__MAX_ELEMENTS_PER_BLOCK = MAX_ELEMENTS_PER_BLOCK
    self.__MM_DRIVER              = MM_DRIVER
    self.__MM_STACK_SIZE          = MM_STACK_SIZE
    self.__MULTREC_LIMIT          = MULTREC_LIMIT
    self.__NUM_LAYERS_3D          = NUM_LAYERS_3D
    self.__NUM_MULT_IMAGES        = NUM_MULT_IMAGES
    self.__N_SIZE_MNK_STACKS      = N_SIZE_MNK_STACKS
    self.__USE_COMM_THREAD        = USE_COMM_THREAD
    self.__USE_MPI_ALLOCATOR      = USE_MPI_ALLOCATOR
    self.__USE_MPI_RMA            = USE_MPI_RMA
    self.__errors                 = errors
    self.__ACC                    = ACC.ACC(errors=[])

  @property
  def AVG_ELEMENTS_IMAGES(self):
    return self.__AVG_ELEMENTS_IMAGES

  @property
  def COMM_THREAD_LOAD(self):
    return self.__COMM_THREAD_LOAD

  @property
  def MAX_ELEMENTS_PER_BLOCK(self):
    return self.__MAX_ELEMENTS_PER_BLOCK

  @property
  def MM_DRIVER(self):
    return self.__MM_DRIVER

  @property
  def MM_STACK_SIZE(self):
    return self.__MM_STACK_SIZE

  @property
  def MULTREC_LIMIT(self):
    return self.__MULTREC_LIMIT

  @property
  def NUM_LAYERS_3D(self):
    return self.__NUM_LAYERS_3D

  @property
  def NUM_MULT_IMAGES(self):
    return self.__NUM_MULT_IMAGES

  @property
  def N_SIZE_MNK_STACKS(self):
    return self.__N_SIZE_MNK_STACKS

  @property
  def USE_COMM_THREAD(self):
    return self.__USE_COMM_THREAD

  @property
  def USE_MPI_ALLOCATOR(self):
    return self.__USE_MPI_ALLOCATOR

  @property
  def USE_MPI_RMA(self):
    return self.__USE_MPI_RMA

  @AVG_ELEMENTS_IMAGES.setter
  def AVG_ELEMENTS_IMAGES(self,val):
    if utilities.is_integer(val):
      self.__AVG_ELEMENTS_IMAGES = val
    else:
      errorMessage = ("Type: Setter\nVar.: AVG_ELEMENTS_IMAGES\nErr.: AVG_ELEMENTS_IMAGES must be an"
        " integer.")
      self.__errors.append(errorMessage)

  @COMM_THREAD_LOAD.setter
  def COMM_THREAD_LOAD(self,val):
    if utilities.is_integer(val):
      self.__COMM_THREAD_LOAD = val
    else:
      errorMessage = ("Type: Setter\nVar.: COMM_THREAD_LOAD\nErr.: COMM_THREAD_LOAD must be an"
        " integer.")
      self.__errors.append(errorMessage)

  @MAX_ELEMENTS_PER_BLOCK.setter
  def MAX_ELEMENTS_PER_BLOCK(self,val):
    if utilities.is_positive_integer(val):
      self.__MAX_ELEMENTS_PER_BLOCK = val
    else:
      errorMessage = ("Type: Setter\nVar.: MAX_ELEMENTS_PER_BLOCK\nErr.: MAX_ELEMENTS_PER_BLOCK must be"
        " an integer.")
      self.__errors.append(errorMessage)

  @MM_DRIVER.setter
  def MM_DRIVER(self,val):
    if val in MM_DRIVER_VALS:
      self.__MM_DRIVER = val
    else:
      errorMessage = ("Type: Setter\nVar.: MM_DRIVER\nErr.: MM_DRIVER val {} not allowed. "
        "Check for typo. Allowed values are: {}".format(val,MM_DRIVER_VALS))
      self.__errors.append(errorMessage)

  @MM_STACK_SIZE.setter
  def MM_STACK_SIZE(self,val):
    if utilities.is_integer(val):
      self.__MM_STACK_SIZE = val
    else:
      errorMessage = ("Type: Setter\nVar.: MM_STACK_SIZE\nErr.: MM_STACK_SIZE must be an integer.")
      self.__errors.append(errorMessage)

  @MULTREC_LIMIT.setter
  def MULTREC_LIMIT(self,val):
    if utilities.is_positive_integer(val):
      self.__MULTREC_LIMIT = val
    else:
      errorMessage = ("Type: Setter\nVar.: MULTREC_LIMIT\nErr.: MULTREC_LIMIT must be positive integer.")
      self.__errors.append(errorMessage)

  @NUM_LAYERS_3D.setter
  def NUM_LAYERS_3D(self,val):
    if utilities.is_positive_integer(val):
      self.__NUM_LAYERS_3D = val
    else:
      errorMessage = ("Type: Setter\nVar.: NUM_LAYERS_3D\nErr.: NUM_LAYERS_3D must be positive " 
        "integer.")
      self.__errors.append(errorMessage)

  @NUM_MULT_IMAGES.setter
  def NUM_MULT_IMAGES(self,val):
    if utilities.is_positive_integer(val):
      self.__NUM_MULT_IMAGES = val
    else:
      errorMessage = ("Type: Setter\nVar.: NUM_MULT_IMAGES\nErr.: NUM_MULT_IMAGES must be positive "
        "integer.")
      self.__errors.append(errorMessage)

  @N_SIZE_MNK_STACKS.setter
  def N_SIZE_MNK_STACKS(self,val):
    if utilities.is_positive_integer(val):
      self.__N_SIZE_MNK_STACKS = val
    else:
      errorMessage = ("Type: Setter\nVar.: N_SIZE_MNK_STACKS\nErr.: N_SIZE_MNK_STACKS must be positive "
        "integer.")
      self.__errors.append(errorMessage)

  @USE_COMM_THREAD.setter
  def USE_COMM_THREAD(self,val):
    if isinstance(val,bool):
      self.__USE_COMM_THREAD = val
    else:
      errorMessage = ("Type: Setter\nVar.: USE_COMM_THREAD\nErr.: USE_COMM_THREAD must be a boolean.")
      self.__errors.append(errorMessage)

  @USE_MPI_ALLOCATOR.setter
  def USE_MPI_ALLOCATOR(self,val):
    if isinstance(val,bool):
      self.__USE_MPI_ALLOCATOR = val
    else:
      errorMessage = ("Type: Setter\nVar.: USE_MPI_ALLOCATOR\nErr.: USE_MPI_ALLOCATOR must be a "
        "boolean.")
      self.__errors.append(errorMessage)

  @USE_MPI_RMA.setter
  def USE_MPI_RMA(self,val):
    if isinstance(val,bool):
      self.__USE_MPI_RMA = val
    else:
      errorMessage = ("Type: Setter\nVar.: USE_MPI_RMA\nErr.: USE_MPI_RMA must be a boolean.")
      self.__errors.append(errorMessage)
