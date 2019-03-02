import ACC

class DBCSR:

  def __init__(self,AVG_ELEMENTS_IMAGES=-1,COMM_THREAD_LOAD=-1,MAX_ELEMENTS_PER_BLOCK=32,
               MM_DRIVER="AUTO",MM_STACK_SIZE=-1,MULTREC_LIMIT=512,NUM_LAYERS_3D=1,
               NUM_MULT_IMAGES=1,N_SIZE_MNK_STACKS=3,USE_COMM_THREAD=True,
               USE_MPI_ALLOCATOR=True,USE_MPI_RMA=False):

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
    self.__ACC                    = ACC()

  @property
  def AVG_ELEMENTS_IMAGES(self):
    return self.__AVG_ELEMENTS_IMAGES

  @property
  def COM_THREAD_LOAD(self):
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
